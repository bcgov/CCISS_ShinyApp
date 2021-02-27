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

#' Pull HexIDs (SiteNo) from lat long inputs
#' @param points A data.frame with columns xName yName representing longitude and latitude
#' GPS values (EPSG:4326)
#' @param xName A character string. Longitude column name.
#' @param yName A character string. Latitude column name.
#' @param host A character string. Postgres database host. Default to `localhost`.
#' @param ... Additional parameters for dbConnect.
#' @details Get hex IDs for provided geolocations.
#' @return A vector of hex IDs,
#' @import sf
#' @importFrom RPostgreSQL dbGetQuery dbDisconnect
#' @export
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
  on.exit(RPostgreSQL::dbDisconnect(con), add = TRUE)
  out <- RPostgreSQL::dbGetQuery(con, hexid_sql)
  
  return(out[,1])
}

#' Pull CCISS from a vector of SiteNo
#' @param SiteNo A numeric vector of SiteNo.
#' @param avg A boolean. 
#' @param scn A character string. Scenario name. Either `rcp45` or `rcp85`.
#' @param host A character string. Postgres database host. Default to `localhost`.
#' @param ... Additional parameters for dbConnect.
#' @details Get CCISS for provided SiteNo.
#' @return A data.table containing CCISS information for each provided SiteNo.
#' @import sf
#' @importFrom RPostgreSQL dbGetQuery dbDisconnect
#' @export
dbGetCCISS <- function(SiteNo, avg, scn = c("rcp45","rcp85"), host = "localhost", ...){
  
  cciss_future_sql <- paste0("
              
  SELECT * 
  FROM cciss_future 
  WHERE siteno IN (", paste(SiteNo, collapse = ","), ")
    AND scenario IN ('", paste(scn, collapse = "','"), "')
  
  ")
  
  con <- init_con(host, ...)
  on.exit(RPostgreSQL::dbDisconnect(con), add = TRUE)
  
  dat <- RPostgreSQL::dbGetQuery(con, cciss_future_sql)
  setDT(dat)
  setnames(dat, c("GCM","Scenario","FuturePeriod","SiteNo","BGC","BGC.pred"))
  
  cciss_historic_sql <- paste0("
               
  SELECT *
  FROM cciss_historic
  WHERE siteno IN (", paste(SiteNo, collapse = ","), ")
               
  ")
  
  datCurr <- RPostgreSQL::dbGetQuery(con, cciss_historic_sql)
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
    dat[, SiteNo := as.numeric(as.factor(BGC))]
  }
  # Why does SiteNo appears twice here
  # TODO:
  # Validate if this is the intended behavior
  dat[,TotNum := .N, by = .(SiteNo,FuturePeriod,SiteNo)]
  dat2 <- dat[,.(BGC.prop = .N/TotNum), keyby = .(SiteNo,FuturePeriod,BGC,BGC.pred)]
  dat2 <- unique(dat2)
  
  return(dat2)
}