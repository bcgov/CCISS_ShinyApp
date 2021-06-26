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

#' Pull points info from lat long inputs
#' @param con An active postgres DBI connection.
#' @param points A data.frame with two columns representing longitude
#'  (Long) and latitude (Lat) with GPS values (EPSG:4326) 
#' @return BEC Information, Site Reference Hex Grid Number and elevation in meter
#' @importFrom RPostgres dbGetQuery
#' @export
dbPointInfo <- function(con, points) {
  
  # It is more efficient for the query optimizer to do
  # each join in a separate sql, better use of index and caching
  # geometry have been optimized (split polygons) for the
  # whole function to return under 50ms
  
  Long <- .subset2(points, "Long")
  Lat <- .subset2(points, "Lat")
  
  # Some elevation rasters extent overlap. I'm taking the maximum elevation value per points
  # From the test I've done, they all share the same values.
  elev_info_sql <- paste0("
    WITH pts4269 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 4269) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT MAX(ROUND(CAST(ST_Value(dem.rast, pts.geom) as NUMERIC), 2)) elevation_m
    FROM bc_elevation dem
    CROSS JOIN pts4269 pts
    WHERE ST_Intersects(dem.rast, pts.geom)
    GROUP BY pts
  ")
  
  
  bec_info_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT bec.zone,
           bec.subzone,
           bec.variant,
           bec.phase,
           bec.natural_disturbance,
           bec.map_label,
           bec.bgc_label,
           bec.zone_name,
           bec.subzone_name,
           bec.variant_name,
           bec.phase_name,
           bec.natural_disturbance_name,
           bec.feature_area_sqm,
           bec.feature_length_m
    FROM pts3005 pts
    LEFT JOIN bec_info bec
    ON ST_Intersects(pts.geom, bec.geometry)
  ")
  
  site_ref_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT cast(hgrid.siteno as text) site_no
    FROM pts3005 pts 
    LEFT JOIN hex_grid hgrid
    ON ST_Intersects(pts.geom, hgrid.geom)
  ")
  
  bc_land_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT bcb_hres.objectid is NOT NULL onbcland
    FROM pts3005 pts
    LEFT JOIN bcb_hres
    ON ST_Within(pts.geom, bcb_hres.geometry)
  ")
  
  bc_forest_region <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT region_ref forest_region
    FROM pts3005 pts
    LEFT JOIN bc_forest_regions
    ON ST_Within(pts.geom, bc_forest_regions.geom)                       
  ")
  
  res1 <- setDT(RPostgres::dbGetQuery(con, bec_info_sql))
  res2 <- setDT(RPostgres::dbGetQuery(con, elev_info_sql))
  res3 <- setDT(RPostgres::dbGetQuery(con, site_ref_sql))
  res4 <- setDT(RPostgres::dbGetQuery(con, bc_land_sql))
  res5 <- setDT(RPostgres::dbGetQuery(con, bc_forest_region))
  
  return(cbind(res1, res2, res3, res4, res5))  
}

# Use the database, it is faster than R native sf functions
# The buffer is to make sure points are not at the edge of the map
#' Buffered bounding box
#' @return a bounding box 1km around points
#' @inheritParams dbPointInfo
#' @param buffer A numeric. Buffer distance in km.
#' @export
dbBbox <- function(con, points, buffer) {
  Long <- .subset2(points, "Long")
  Lat <- .subset2(points, "Lat")
  bbox_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT st_xmin(box),
           st_ymin(box),
           st_xmax(box),
           st_ymax(box)
    FROM (
      SELECT ST_Extent(st_transform(st_buffer(pts.geom, ", buffer, ", 3), 4326)) box
      FROM pts3005 pts
    ) box
  ")
  unname(RPostgres::dbGetQuery(con, bbox_sql))
}

# drv <- dbDriver("Postgres")
# con <- dbConnect(drv, user = "postgres", password = "jujL6cB3Wm9y", host = "138.197.168.220", 
#                  port = 5432, dbname = "cciss")
# 
# sites <- 6305115:6305125
# test <- dbGetCCISS(con, sites, FALSE, scn = "ssp370")

#' Pull CCISS from a vector of SiteNo
#' @param con An active postgres DBI connection.
#' @param siteno A character vector of siteno.
#' @param avg A boolean. 
#' @param scn A character string. Scenario name. Either `rcp45` or `rcp85`.
#' @details Get CCISS for provided SiteNo.
#' @return A data.table containing CCISS information for each provided SiteNo.
#' @importFrom RPostgres dbGetQuery
#' @export
dbGetCCISS <- function(con, siteno, avg, scn = c("ssp126","ssp245","ssp370","ssp585")){

  groupby = "siteno"
  if (isTRUE(avg)) {
    groupby = "bgc"
  }
  
  ##cciss_future is now test_future  
  cciss_sql <- paste0("
  WITH cciss AS (
    SELECT futureperiod,
           test_future.siteno,
           bgc,
           bgc_pred,
           gcm
    FROM test_future
    WHERE scenario IN ('", paste(scn, collapse = "','"), "') and
          siteno IN (", paste(unique(siteno), collapse = ","), ")
    
    UNION ALL
    
    SELECT CASE period
             WHEN 'Normal61' THEN '1975'
             WHEN 'Current91' THEN '2000'
           END futureperiod,
           test_historic.siteno,
           bgc,
           bgc_pred,
           CASE period
             WHEN 'Normal61' THEN 'Historic'
             WHEN 'Current91' THEN 'Current'
           END gcm
    FROM test_historic
    WHERE siteno IN (", paste(unique(siteno), collapse = ","), ")
  
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
  print(dat)
  return(dat)
}
