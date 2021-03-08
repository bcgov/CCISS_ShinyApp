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
#' @return BEC Information about features
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

#' Pull bec_info from lat long inputs
#' @inheritParams dbGetHexID
#' @return BEC Information about features
#' @export
dbGetBecInfo <- function(points, xName = "Long", yName = "Lat", host = "localhost", ...) {
  
  txt <- st_as_sf(points,coords = c(xName,yName), crs = 4326) %>%
    st_transform(3005) %>%
    st_combine() %>%
    st_as_text()
  
  bec_info_sql <- paste0("
  
  SELECT \"ZONE\",
         \"SUBZONE\",
         \"VARIANT\",
         \"PHASE\",
         \"NATURAL_DISTURBANCE\",
         \"MAP_LABEL\",
         \"BGC_LABEL\",
         \"ZONE_NAME\",
         \"SUBZONE_NAME\",
         \"VARIANT_NAME\",
         \"PHASE_NAME\",
         \"NATURAL_DISTURBANCE_NAME\",
         \"FEATURE_AREA_SQM\",
         \"FEATURE_LENGTH_M\",
         \"FEATURE_AREA\",
         \"FEATURE_LENGTH\"
  FROM bec_sf
  WHERE ST_Intersects(ST_GeomFromText('",txt,"', 3005), bec_sf.geometry)
  
  ")
  
  con <- init_con(host, ...)
  on.exit(RPostgreSQL::dbDisconnect(con), add = TRUE)
  out <- RPostgreSQL::dbGetQuery(con, bec_info_sql)
  
  return(out)  
}

#' Pull bec_info from lat long inputs
#' @inheritParams dbGetHexID
#' @return BEC Information about features
#' @export
localGetBecInfo <- function(points, xName = "Long", yName = "Lat", host, ...) {
  
  # Return immediatly if points is empty
  if (nrow(points) == 0L) {
    return(list(points = points))
  }
  
  pts <- st_as_sf(points, coords = c(xName, yName), crs = 4326)
  pts <- st_transform(pts, crs = 3005)
  
  # Remove points that do not have valid geometries
  has_geom <- which(!sf::st_is_empty(pts))
  points <- points[i = has_geom]
  pts <- pts[has_geom, ]
  
  # Faster sf_intersects by skipping costly operations and pre filtering
  # Do not intersects with polygons you know the bbox to be outside your
  # points of interest.
  midx <- integer()
  for (pt in pts$geometry) {
    b <- as.list(sf::st_bbox(pt))
    idx <- which(.subset2(b, "xmin") < .subset2(host, "xmax") &
                 .subset2(b, "xmax") > .subset2(host, "xmin") &
                 .subset2(b, "ymin") < .subset2(host, "ymax") &
                 .subset2(b, "ymax") > .subset2(host, "ymin"))
    midx <- c(midx, idx)
  }
  midx <- sort(unique(midx))
  
  # Return early with empty points when all points outside bboxes
  if (length(midx) == 0) {
    return(list(points = points[i = 0]))
  }
  
  m <- sf::st_intersects(pts$geometry, host$geometry[midx])
  has_m <- which(lengths(m) > 0)
  
  # Return early with empty if no intersects match is found
  if (length(has_m) == 0) {
    return(list(points = points[i = 0]))
  }
  
  # Retain only points with a match with filtered BEC Info
  list(points = points[i = has_m], info = host[i = midx[unlist(m)], j = 3L:18L])
  
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