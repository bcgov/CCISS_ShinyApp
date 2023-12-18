library(RPostgres)
library(terra)
library(data.table)

rast_files <- list.files("./spatial_app/data/Sunshine/", full.names = TRUE, pattern = "*.tif")
spp_files <- rast_files[grep("Spp.*",rast_files)]

all_spp <- rast(spp_files)

rast_files <- list.files("./spatial_app/data/Sunshine/", full.names = FALSE, pattern = "*.tif")
spp_names <- rast_files[grep("Spp.*",rast_files)]

meta_dt <- data.table(full_nm = spp_names)
meta_dt[,c("group","stat","spp","edapos","scenario","period","junk") := tstrsplit(full_nm, ".", fixed = T)]
meta_dt[,junk := NULL]
meta_dt[,laynum := seq_along(period)]  
  
writeRaster(all_spp, "temp_rast.tif")
library(ssh)
con <- ssh_connect("root@178.128.233.227")
scp_upload(con, "temp_rast.tif", to = "/share/")
ssh_exec_wait(con, command = c(
  'cd /share/',
  'GTIFF_DIRECT_IO=YES raster2pgsql -s 4326 -I -M temp_rast.tif -t 80x80 sunshine_spp > sunshine_spp.sql'
))
