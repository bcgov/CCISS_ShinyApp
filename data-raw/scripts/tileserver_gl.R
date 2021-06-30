# This file is used to create the tileserver on Digital Ocean
# and create tiles from shape file.
# Here I'm leveraging bcgov existing packages to obtain map data.
library(sf)
library(analogsea)
library(ccissdev)
library(data.table)

options("bcdata.chunk-limit" = 100)
out_dir <- "./data-raw/shp"
shp_name <- "BEC_MAP.shp"
keep <- c("MAP_LABEL", "ZONE", "OBJECTID")
layer <- "BECMap"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

bgcMap <- st_read("~/CommonTables/WNA_BGC_v12_21May2021.gpkg")
bgcInfo <- fread("~/CommonTables/All_BGCs_Info_v12_2.csv") 
BCbgc <- bgcInfo[DataSet == "BC",BGC]
bgcMap <- bgcMap[bgcMap$BGC %in% BCbgc,]
st_write(bgcMap,"~/CommonTables/BC_BGCv12.gpkg")
# Digital Ocean provisioning - Setup your SSH keys in your accounts before running these.
# tileserver <- setup_docklet()
# Or Reuse an existing droplet
tileserver <- droplets()[["tileserver-gl"]]
# About 5-6h
remote_shp_tiles(tileserver,
                 "-z18 --simplification=10 --force --coalesce-densest-as-needed --extend-zooms-if-still-dropping --detect-shared-borders",
                 source_dir = out_dir, skip_upload = TRUE)
launch_tileserver(tileserver, config = "./data-raw/config/tileserver/config.json", styles = "./data-raw/config/tileserver/becstyle.json")

