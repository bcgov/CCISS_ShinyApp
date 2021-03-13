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
#' @importFrom RPostgres dbGetQuery
#' @return BEC Information about features
#' @export
dbGetHexID <- function(con, points) {
  
  txt <- st_as_sf(points, coords = 1L:2L, crs = 4326) %>%
    st_transform(3005) %>%
    st_geometry() %>%
    st_as_text()
  
  hexid_sql <- paste0("
  
  SELECT cast(siteno as text) siteref
  FROM (", paste0("SELECT st_pointfromtext('", txt, "', 3005) geom", collapse = "\n UNION ALL \n") ,") pts
  LEFT JOIN hex_grid
  ON ST_Intersects(pts.geom, hex_grid.geom)
  
  ")
  
  return(RPostgres::dbGetQuery(con, hexid_sql)[,1])
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
         feature_length,
         bcb_hres.objectid is NOT NULL onbcland
  FROM (", paste0("SELECT st_pointfromtext('", txt, "', 3005) geom", collapse = "\n UNION ALL \n") ,") pts
  LEFT JOIN bec_info
  ON ST_Intersects(pts.geom, bec_info.geometry)
  LEFT JOIN bcb_hres
  ON ST_Intersects(pts.geom, bcb_hres.geometry)
  ")

  return(setDT(RPostgres::dbGetQuery(con, bec_info_sql)))  
}

#' Pull CCISS from a vector of SiteNo
#' @param con An active postgre DBI connection.
#' @param points A data.frame with two columns representing longitude and latitude
#' GPS values (EPSG:4326)
#' @param avg A boolean. 
#' @param scn A character string. Scenario name. Either `rcp45` or `rcp85`.
#' @details Get CCISS for provided SiteNo.
#' @return A data.table containing CCISS information for each provided SiteNo.
#' @import sf
#' @importFrom RPostgres dbGetQuery
#' @export
dbGetCCISS <- function(con, points, avg, scn = c("rcp45","rcp85")){

  txt <- st_as_sf(points, coords = 1L:2L, crs = 4326) %>%
    st_transform(3005) %>%
    st_geometry() %>%
    st_as_text()
  
  groupby = "siteno"
  if (isTRUE(avg)) {
    groupby = "bgc"
  }
    
  cciss_sql <- paste0("
  WITH siteno AS (
  
    SELECT DISTINCT siteno
    FROM (", paste0("SELECT st_pointfromtext('", txt, "', 3005) geom", collapse = "\n UNION ALL \n") ,") pts
    INNER JOIN hex_grid
    ON ST_Intersects(pts.geom, hex_grid.geom)
  
  ), cciss AS (
  
    SELECT futureperiod,
           cciss_future.siteno,
           bgc,
           bgc_pred,
           gcm
    FROM cciss_future
    JOIN siteno
      ON siteno.siteno = cciss_future.siteno
    WHERE scenario IN ('", paste(scn, collapse = "','"), "')
    
    UNION ALL
    
    SELECT CASE period
             WHEN 'Normal61' THEN '1975'
             WHEN 'Current91' THEN '2000'
           END futureperiod,
           cciss_historic.siteno,
           bgc,
           bgc_pred,
           CASE period
             WHEN 'Normal61' THEN 'Historic'
             WHEN 'Current91' THEN 'Current'
           END gcm
    FROM cciss_historic
    JOIN siteno
      ON siteno.siteno = cciss_historic.siteno
  
  ), cciss_count_den AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           COUNT(siteno) n
    FROM cciss
    GROUP BY ", groupby, ", futureperiod
  
  ), cciss_count_num AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           bgc,
           bgc_pred,
           COUNT(siteno) n
    FROM cciss
    GROUP BY ", groupby, ", futureperiod, bgc, bgc_pred
  
  )
  
  SELECT cast(a.siteref as text) siteref,
         a.futureperiod,
         a.bgc,
         a.bgc_pred,
         a.n/cast(b.n as float) bgc_prop
  FROM cciss_count_num a
  JOIN cciss_count_den b
    ON a.siteref = b.siteref
   AND a.futureperiod = b.futureperiod
  
  ")
  
  dat <- setDT(RPostgres::dbGetQuery(con, cciss_sql))

  setnames(dat, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  
  return(dat)
}
