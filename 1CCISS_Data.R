###CCISS 2020 Step 1: Pull data from postgres database
###Kiri Daust

# Copyright 2021 Province of British Columbia
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


##Pull HexIDs from lat long inputs
dbGetHexID <- function(points, xName = "Long", yName = "Lat", host = "localhost"){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user = "postgres", host = host,password = "Kiriliny41", 
                   port = 5432, dbname = "cciss_data")
  txt <- st_as_sf(points,coords = c(xName,yName), crs = 4326) %>%
    st_transform(3005) %>%
    st_combine() %>%
    st_as_text()
  q1 <- paste0("SELECT DISTINCT siteno FROM hex_grid WHERE ST_Intersects(ST_GeomFromText('",txt,"', 3005), hex_grid.geom)")
  out <- st_read(con, query = q1)
  dbDisconnect(con)
  return(out[,1])
}


dbGetCCISS <- function(SiteNo, avg, scn = c("rcp45","rcp85"), host = "localhost"){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user = "postgres", host = host,password = "Kiriliny41", 
                   port = 5432, dbname = "cciss_data")
  q <- paste0("SELECT * FROM cciss_future WHERE siteno IN (",paste(SiteNo, collapse = ","),
              ") AND scenario IN ('",paste(scn, collapse = "','"),"')")
  dat <- dbGetQuery(con,q)
  dat <- as.data.table(dat)
  setnames(dat,c("GCM","Scenario","FuturePeriod","SiteNo","BGC","BGC.pred"))
  
  q2 <- paste0("SELECT * FROM cciss_historic WHERE siteno IN (",paste(SiteNo, collapse = ","),
              ")")
  datCurr <- dbGetQuery(con,q2)
  datCurr <- as.data.table(datCurr)
  datID <- data.table(period = c("Normal61","Current91"),pID = c(1975,2000), 
                      GCM = c("Historic","Current"))
  datCurr[datID, `:=`(FuturePeriod = i.pID,GCM  = i.GCM), on = "period"]
  datCurr[,period := NULL]
  setcolorder(datCurr, c("FuturePeriod","siteno","bgc","bgc_pred","GCM"))
  setnames(datCurr, c("FuturePeriod","SiteNo","BGC","BGC.pred","GCM"))
  dat <- rbind(dat,datCurr, fill = T)
  
  if(avg){
    dat[,SiteNo := as.numeric(as.factor(BGC))]
  }
  dat[,TotNum := .N, by = .(SiteNo,FuturePeriod,SiteNo)]
  dat2 <- dat[,.(BGC.prop = .N/TotNum), keyby = .(SiteNo,FuturePeriod,BGC,BGC.pred)]
  dat2 <- unique(dat2)
  dbDisconnect(con)
  return(dat2)
}

