# Add required bec and bc_hres_bounds to database
# add index and split geometries

library(RPostgres)
library(bcmaps)
library(sf)
library(glue)

# Add missing data + create some indexes for better query performances

con <- RPostgres::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

dbSendQuery(con, "CREATE EXTENSION postgis;")

# For production performance it is more efficient to split
# complex polygons  and rely on index. Both tables where this
# is applied are only used to fetch information.
splitpoly <- function(con, table, geom = "geometry") {
  # Split polygons for faster intersect lookup
  fields <- dbListFields(con, table)
  fields_no_geo <- paste(fields[!fields %chin% geom], collapse = ", ")
  fields <- paste(fields, collapse = ", ")
  dbSendQuery(con, glue("
  with complex_areas_to_subdivide as (
    delete from {table}
    where ST_NPoints({geom}) > 255
    returning {fields}
  )
  insert into {table} ({fields})
    select
        {fields_no_geo},
        ST_Subdivide({geom}, 255) as {geom}
    from complex_areas_to_subdivide;
  ", fields = fields, fields_no_geo = fields_no_geo, geom = geom, table = table))
}

indextb <- function(con, table, geom = "geometry") {
  # Drop existing index
  try({
    dbSendQuery(con, glue("
      DROP INDEX {tb}_geom_idx;
    ", tb = table))
  })
  # Create index and vacuum
  dbSendQuery(con, glue("
    CREATE INDEX {tb}_geom_idx ON {tb} USING GIST ({geom});
  ", tb = table, geom = geom, type = type))
  dbSendQuery(con, glue("VACUUM ANALYZE {tb};", tb = table))  
}

bec <- bec()
names(bec) <- tolower(names(bec))
st_write(bec, con, "bec_info")
splitpoly(con, "bec_info")
indextb(con, "bec_info")

bcb_hres <- bc_bound_hres()
names(bcb_hres) <- tolower(names(bcb_hres))
st_write(bcb_hres, con, "bcb_hres")
splitpoly(con, "bcb_hres")
indextb(con, "bcb_hres")

# Additional missing index
indextb(con, "hex_grid", geom = "geom")

dbSendQuery(con, glue("
    CREATE INDEX {tb}_idx ON {tb} USING BTREE ({idx});
  ", tb = "cciss_historic", idx = "siteno"))
dbSendQuery(con, glue("VACUUM ANALYZE {tb};", tb = "cciss_historic"))

# Add provided forest regions



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
