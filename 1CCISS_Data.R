###CCISS 2020 Step 1: Pull data from postgres database
###Kiri Daust

# Copyright 2021 Will Mackenzie
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Initiate a connection CCISS database
#' @param host A character string. Database host.
#' @param ... Additional parameters for dbConnect
init_con <- function(host = "localhost", ...) {
  # hardcoded passwords and login are a security risk
  # best to use sodium and OS credentials store in production
  drv <- dbDriver("PostgreSQL")
  dbConnect(
    drv, user = "postgres", host = host, password = "Kiriliny41", 
    port = 5432, dbname = "cciss_data", ...
  )
}

##Pull HexIDs from lat long inputs
dbGetHexID <- function(points, xName = "Long", yName = "Lat", host = "localhost", ...) {
  
  txt <- st_as_sf(points,coords = c(xName,yName), crs = 4326) %>%
    st_transform(3005) %>%
    st_combine() %>%
    st_as_text()
  
  hexid_sql <- paste0("
  
  SELECT DISTINCT siteno
  FROM hex_grid
  WHERE ST_Intersects(ST_GeomFromText('",txt,"', 3005), hex_grid.geom)
  
  ")
  
  con <- init_con(host, ...)
  on.exit(dbDisconnect(con), add = TRUE)
  out <- dbGetQuery(con, hexid_sql)

  return(out[,1])
}


dbGetCCISS <- function(SiteNo, avg, scn = c("rcp45","rcp85"), host = "localhost", ...){
  
  cciss_future_sql <- paste0("
              
  SELECT * 
  FROM cciss_future 
  WHERE siteno IN (", paste(SiteNo, collapse = ","), ")
    AND scenario IN ('", paste(scn, collapse = "','"), "')
  
  ")
  
  con <- init_con(host, ...)
  on.exit(dbDisconnect(con), add = TRUE)
  
  dat <- dbGetQuery(con, cciss_future_sql)
  setDT(dat)
  setnames(dat, c("GCM","Scenario","FuturePeriod","SiteNo","BGC","BGC.pred"))
  
  cciss_historic_sql <- paste0("
               
  SELECT *
  FROM cciss_historic
  WHERE siteno IN (", paste(SiteNo, collapse = ","), ")
               
  ")
  
  datCurr <- dbGetQuery(con, cciss_historic_sql)
  setDT(datCurr)
  # use set with match in production to avoid `[` overhead
  # this could all be done in the same set call
  # split for clarity
  # Use of .subset and .subset2 is mostly for performance
  m <- match(.subset2(datCurr, "period"), c("Normal61","Current91"))
  set(datCurr, j = "period", value = NULL)
  set(datCurr, j = "FuturePeriod", value = .subset(c(1975,2000), m))
  set(datCurr, j = "GCM", value = .subset(c("Historic","Current"), m))
  setcolorder(datCurr, c("FuturePeriod","siteno","bgc","bgc_pred","GCM"))
  setnames(datCurr, c("FuturePeriod","SiteNo","BGC","BGC.pred","GCM"))
  dat <- rbind(dat, datCurr, fill = T)
  
  if (avg) {
    # This as.factor depends on the values in BGC at runtime
    # This could lead to unexpected results
    dat[, SiteNo := as.numeric(as.factor(BGC))]
  }
  # Why does SiteNo appears twice here
  dat[,TotNum := .N, by = .(SiteNo,FuturePeriod,SiteNo)]
  dat2 <- dat[,.(BGC.prop = .N/TotNum), keyby = .(SiteNo,FuturePeriod,BGC,BGC.pred)]
  dat2 <- unique(dat2)

  return(dat2)
}

