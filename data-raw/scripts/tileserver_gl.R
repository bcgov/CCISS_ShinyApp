## Here I'm leveraging bcgov existant packages
## to obtain map data.
library(bcmaps)
library(sf)
library(analogsea)
library(bccciss)

options("bcdata.chunk-limit" = 100)
out_dir <- "./data-raw/shp"
shp_name <- "BEC_MAP.shp"
keep <- c("MAP_LABEL", "ZONE", "OBJECTID")
layer <- "BEC Map"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## Go take a long walk outside, this took about 3h on my laptop.
bcmaps::bec(ask = FALSE) %>%
  ## Using Hi-Res boundaries map to carve out
  ## BC coastline from BEC Map
  st_intersection(bcmaps::bc_bound_hres()) %>%
  ## Set CRS for tippecanoe
  st_transform(4326) %>%
  # Keep only the relevant info for styling, info
  # will be retrieved from postgis
  # `[` is a pipeable way to use x[i] , see ?`[`
  `[`(keep) %>%
  st_write(dsn = file.path(out_dir, shp_name), layer = layer, overwrite = TRUE)

# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
# tileserver <- setup_docklet()
tileserver <- droplets()[[1]]
# About 5-6h
remote_shp_tiles(tileserver,
                 "-z18 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = TRUE)
launch_tileserver(tileserver, config = "./data-raw/config/tileserver/config.json", styles = "./data-raw/config/tileserver/becstyle.json")

