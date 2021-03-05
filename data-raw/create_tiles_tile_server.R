## Here I'm leveraging bcgov existant packages
## to obtain map data.
library(bcdata) # Requires https://github.com/bcgov/bcdata/pull/250
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
dir.create(out_dir, recursive = TRUE)

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
  # will be retrieved by Shiny via OBJECTID
  # `[` is a pipeable way to use x[i] , see ?`[`
  `[`(c(keep, id)) %>%
  st_write(dsn = file.path(out_dir, shp_name), layer = layer, overwrite = TRUE)

# Great, all that's left is launching a VM, create the tiles
# and serve them. Creating tiles is quite ressources intensive.
## https://openmaptiles.org/docs/generate/custom-vector-from-shapefile-geojson/
## https://openmaptiles.org/docs/host/tileserver-gl/


# I'm using a DO VM in Toronto but the same could be achieved on any other cloud provider.
# The steps would similar. 

# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
droplet <- droplet_create(
  image = "docker-20-04",
  region = "tor1",
  size = "s-2vcpu-4gb-intel",
  tags = "tileserver",
  ssh_keys = names(keys()),
  wait = TRUE)

# Sometimes network interface is still not alive
Sys.sleep(10)

setup_docklet(droplet)
droplet_upload(droplet, local = out_dir, remote = "/tmp/")
# This will convert a shp file to geojson on the remote server
droplet_ssh(droplet, paste("ogr2ogr -f GeoJSON",
                           file.path("/tmp", gsub("\\.shp", "\\.geojson", shp_name)),
                           file.path("/tmp", shp_name)))
# Explainations on the different options is available at
# https://github.com/mapbox/tippecanoe
# /mapdata is predefined folder where tileserver will be 
# looking for mbtiles files.
droplet_ssh(droplet, paste("tippecanoe -o",
                           file.path("/mapdata", gsub("\\.shp", "\\.mbtiles", shp_name)),
                           paste0("--use-attribute-for-id=", id),
                           "-z14 --force --simplify-only-low-zooms",
                           "--coalesce-densest-as-needed --extend-zooms-if-still-dropping",
                           file.path("/tmp", gsub("\\.shp", "\\.geojson", shp_name))))

# Styles

# call launch
  

  
  

