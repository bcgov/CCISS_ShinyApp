library(RPostgreSQL)
library(bcmaps)
library(sf)
library(glue)

postgis_password = .rs.api.askForPassword("PostGis database password")

con <- RPostgreSQL::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  dbname = "postgres",
  port = 5432, 
  user = "postgres",
  password = postgis_password
)

dbSendQuery(con, "CREATE DATABASE cciss_data;")

con <- RPostgreSQL::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  dbname = "cciss_data",
  port = 5432, 
  user = "postgres",
  password = postgis_password
)

dbSendQuery(con, "CREATE EXTENSION postgis;")

bec <- bec()
names(bec) <- tolower(names(bec))
st_write(bec, con, "bec_info")
dbSendQuery(con, glue("
    CREATE INDEX {tb}_geom_idx ON {tb} USING GIST ({geom});
  ", tb = "bec_info", geom = "geometry"))
dbSendQuery(con, glue("VACUUM ANALYZE {tb};", tb = "bec_info"))

bcb_hres <- bc_bound_hres()
names(bcb_hres) <- tolower(names(bcb_hres))
st_write(bcb_hres, con, "bcb_hres")
dbSendQuery(con, glue("
    CREATE INDEX {tb}_geom_idx ON {tb} USING GIST ({geom});
  ", tb = "bcb_hres", geom = "geometry"))
dbSendQuery(con, glue("VACUUM ANALYZE {tb};", tb = "bcb_hres"))


# Testing intersects speed (should be below 30ms)

query <- "
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
  FROM (
     SELECT st_pointfromtext('POINT (1287191 750622.7)', 3005) geom
     UNION ALL
     SELECT st_pointfromtext('POINT (1290741 732413.5)', 3005) geom
     UNION ALL
     SELECT st_pointfromtext('POINT (1305176 711244.7)', 3005) geom
     UNION ALL
     SELECT st_pointfromtext('POINT (1258758 744210.9)', 3005) geom
     UNION ALL
     SELECT st_pointfromtext('POINT (1300891 799281.2)', 3005) geom
  ) pts
  LEFT JOIN bec_info
  ON ST_Intersects(pts.geom, bec_info.geometry)"

microbenchmark::microbenchmark({a <- dbGetQuery(con, query)})