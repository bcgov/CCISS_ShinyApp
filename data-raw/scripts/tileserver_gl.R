# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.
# Here I'm leveraging bcgov existing packages to obtain map data.
library(sf)
library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(ccissdev)
library(data.table)

out_dir <- "./data-raw/shp"
shp_name <- "BEC_MAP.shp"
layer <- "BECMap"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
library(sf)
dat <- st_read("~/CommonTables/BC_BGCv12.gpkg")
dat <- st_transform(dat,4326)
dat <- dat["BGC"]
dat <- st_cast(dat,"MULTIPOLYGON")
dat2 <- aggregate(dat, list(dat$BGC), function(x) x[1])
dat2$Group.1 <- NULL
dat2$OBJECTID <- seq_along(dat2$BGC)
dat2 <- dat[,c(1,2,4,3)]
colnames(dat)[1:3] <- c("MAP_LABEL","ZONE","OBJECTID")
dat <- st_cast(dat,"MULTIPOLYGON")
st_write(dat2,dsn = out_dir,layer = layer,driver = "ESRI Shapefile",overwrite = T, append = F)
# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
# tileserver <- setup_docklet()
# Or Reuse an existing droplet
tileserver <- droplets()[["tileserver-wna"]]
# About 5-6h
remote_shp_tiles(tileserver,
                 "-z18 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = FALSE)
launch_tileserver(tileserver, config = "./data-raw/config/tileserver/config.json", styles = "./data-raw/config/tileserver/becstyle.json")

