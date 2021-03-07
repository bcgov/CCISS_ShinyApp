## Here I'm leveraging bcgov existant packages
## to obtain map data.
library(bcdata) # Requires version 0.2.1.9000
library(bcmaps)
library(sf)
library(analogsea)
library(bccciss)

options("bcdata.chunk-limit" = 100)
out_dir <- "./data-raw/BEC/shp"
shp_name <- "BEC_MAP.shp"
keep <- c("MAP_LABEL", "ZONE")
id <- "OBJECTID"
layer <- "BEC Map"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## Go take a long walk outside, this took about 3h on my laptop.
bcdc_get_data(bcdc_search("BEC Map")[["bec-map"]]) %>%
  ## Using Hi-Res boundaries map to carve out
  ## BC coastline from BEC Map
  st_intersection(bc_bound_hres()) %>%
  ## Uncomment following two lines to save intermediate step
  # saveRDS("./data-raw/BEC/bec_clip.RDS")
  # readRDS("./data-raw/BEC/bec_clip.RDS") %>%
  ## Set CRS for tippecanoe
  st_transform(4326) %>%
  # Keep only the relevant info for styling, the rest
  # will be retrieved by Shiny via OBJECTID (id)
  # `[` is a pipeable way to use x[i] , see ?`[`
  `[`(c(keep, id)) %>%
  st_write(dsn = file.path(out_dir, shp_name), layer = layer, overwrite = TRUE)

# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
# tileserver <- setup_docklet()
tileserver <- droplets()[[1]]
remote_shp_tiles(tileserver, paste0("--use-attribute-for-id=", id),
                 "-z18 --simplification=20 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping",
                 source_dir = out_dir, skip_upload = TRUE)
launch_tileserver(tileserver, config = "./inst/tileserver/config.json", styles = "./inst/tileserver/becstyle.json")
