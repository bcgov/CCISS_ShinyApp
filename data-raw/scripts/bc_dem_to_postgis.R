# Import BC Digital Elevation Model rasters to postgis database

library(bcmaps)
library(sf)
dbExecute(con,"CREATE EXTENSION postgis_raster;")

bc <- st_cast(st_as_sfc(bc_bbox("sf")), "POLYGON")
vrt <- cded(aoi = bc, ask = FALSE)
cache <- bcmaps::show_cached_files()
cache <- cache[cache$is_dir == TRUE & grepl("cded", cache$file),]$file
tifs <- dir(cache, pattern = "tif$", full.names = TRUE, recursive = TRUE)

file.rename(tifs, file.path(cache, basename(tifs)))

# Run from cache dir (raster2pgsql is insalled as part of postgis geo bundle and 
# should be made available on path by adding PostgreSQL\XX\bin to path)
#
# $ raster2pgsql -d -C -r -F -I -M -e -Y -s 4269 *.tif public.bc_elevation > out.sql
# $ psql --username={user} --host={host} -W -f out.sql {dbname}
