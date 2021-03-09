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
#' @param con An active postgre DBI connection.
#' @param points A data.frame with two columns representing longitude and latitude
#' GPS values (EPSG:4326)
#' @details Get hex IDs for provided geolocations.
#' @return A vector of hex IDs,
#' @import sf
#' @importFrom RPostgreSQL dbGetQuery
#' @return BEC Information about features
#' @export
dbGetHexID <- function(con, points) {
  
  txt <- st_as_sf(points, coords = 1L:2L, crs = 4326) %>%
    st_transform(3005) %>%
    st_geometry() %>%
    st_as_text()
  
  hexid_sql <- paste0("
  
  SELECT DISTINCT siteno
  FROM (", paste0("SELECT st_pointfromtext('", txt, "', 3005) geom", collapse = "\n UNION ALL \n") ,") pts
  INNER JOIN hex_grid
  ON ST_Intersects(pts.geom, bec_info.geometry)
  
  ")
  
  return(out[,1])
}

#' Pull bec_info from lat long inputs
#' @inheritParams dbGetHexID
#' @return BEC Information about features
#' @export
dbGetBecInfo <- function(con, points) {
  
  txt <- st_as_sf(points, coords = 1L:2L, crs = 4326L) %>%
    st_transform(3005) %>%
    st_geometry() %>%
    st_as_text()
  
  bec_info_sql <- paste0("
  
  SELECT zone,
         subzone,
         variant,
         phase,
         natural_disturbance,
         map_label,
         bgc_label,
         zone_name,
         subzone_name,
         variant_name,
         phase_name,
         natural_disturbance_name,
         feature_area_sqm,
         feature_length_m,
         feature_area,
         feature_length
  FROM (", paste0("SELECT st_pointfromtext('", txt, "', 3005) geom", collapse = "\n UNION ALL \n") ,") pts
  LEFT JOIN bec_info
  ON ST_Intersects(pts.geom, bec_info.geometry)
  
  ")

  return(setDT(RPostgreSQL::dbGetQuery(con, bec_info_sql)))  
}

#' Pull CCISS from a vector of SiteNo
#' @param con An active postgre DBI connection.
#' @param SiteNo A numeric vector of SiteNo.
#' @param avg A boolean. 
#' @param scn A character string. Scenario name. Either `rcp45` or `rcp85`.
#' @details Get CCISS for provided SiteNo.
#' @return A data.table containing CCISS information for each provided SiteNo.
#' @import sf
#' @importFrom RPostgreSQL dbGetQuery
#' @export
dbGetCCISS <- function(con, SiteNo, avg, scn = c("rcp45","rcp85")){
  
  cciss_future_sql <- paste0("
              
  SELECT * 
  FROM cciss_future 
  WHERE siteno IN (", paste(SiteNo, collapse = ","), ")
    AND scenario IN ('", paste(scn, collapse = "','"), "')
  
  ")
  
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