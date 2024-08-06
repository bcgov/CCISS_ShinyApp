library(climr)
library(data.table)
library(terra)
library(ranger)

addVars <- function(dat) {
  dat[, PPT_MJ := PPT_05 + PPT_06]
  dat[, PPT_JAS := PPT_07 + PPT_08 + PPT_09]
  dat[, PPT.dormant := PPT_at + PPT_wt]
  dat[, CMD.def := pmax(0, 500 - PPT.dormant)]
  dat[, CMDMax := CMD_07]   ## TODO: THIS IS NOT NECESSARILY CMD MAX
  dat[, CMD.total := CMD.def + CMD]
  #dat[, DD_delayed := pmax(0, ((DD_0_at + DD_0_wt)*0.0238) - 1.8386)]
}

tile_predict <- function(BGCmodel2, Y1, maxSize = 6000000){
  n = nrow(Y1)
  brks <- seq(1,n,by = maxSize)
  brks <- c(brks,n)
  Y1[,BGC.pred := NA_character_]
  for(j in 1:(length(brks)-1)){
    Y1[brks[j]:brks[j+1],BGC.pred := predict(BGCmodel2, Y1[brks[j]:brks[j+1],-c(1:3)],num.threads = 14)[['predictions']]]
  }
  TRUE
}


library(sf)
outline <- st_read("../Common_Files/gpr_000b11a_e/gpr_000b11a_e.shp")
bc_ol <- outline[1,]
plot(bc_ol)
bc_ol <- bc_ol[,"PRUID"]
bc_ol2 <- vect(bc_ol)
dem_hr <- rast("../Common_Files/WNA_DEM_SRT_30m_cropped.tif")
temp <- crop(dem_hr,bc_ol2)
temp_dem <- aggregate(temp, fact = 5)
bc_rast <- rasterize(bc_ol, temp_dem)
final_dem <- mask(temp_dem, bc_rast)
writeRaster(final_dem, "BC_DEM_100m.tif")
# 
# bc_ol <- disagg(bc_ol, fact = 10)
# t2 <- resample(temp, bc_ol)
# 

final_dem <- rast("BC_DEM_100m.tif")
final_dem <- aggregate(final_dem, fact = 2)
#################climr####################
points_dat <- as.data.frame(final_dem, cells=T, xy=T)
colnames(points_dat) <- c("id", "lon", "lat", "elev")
points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input

vars_needed <- c("CMD_sm", "DDsub0_sp", "DD5_sp", "Eref_sm", "Eref_sp", "EXT", 
  "MWMT", "NFFD_sm", "NFFD_sp", "PAS", "PAS_sp", "SHM", "Tave_sm", 
  "Tave_sp", "Tmax_sm", "Tmax_sp", "Tmin", "Tmin_at", "Tmin_sm", 
  "Tmin_sp", "Tmin_wt","CMI", "PPT_05","PPT_06","PPT_07","PPT_08","PPT_09","PPT_at","PPT_wt","CMD_07","CMD"
)
gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
ssp_use <- "ssp245"
periods_use <- list_gcm_periods()

splits <- c(seq(1,nrow(points_dat), by = 2000000),nrow(points_dat)+1)
BGCmodel <- readRDS("../Common_Files/WNA_BGCv12_10May24.rds")

gcm_curr <- gcms_use[1]
period_curr <- periods_use[1]
pred_ls <- list()
for(i in 1:(length(splits)-1)){
  cat(i,"\n")
  clim_dat <- downscale(points_dat[splits[i]:(splits[i+1]-1),], 
                        gcms = gcm_curr,
                        gcm_periods = period_curr,
                        ssps = ssp_use,
                        max_run = 1L,
                        vars = vars_needed,
                        return_refperiod = FALSE)
  clim_dat <- clim_dat[RUN != "ensembleMean",]
  addVars(clim_dat)
  clim_dat <- na.omit(clim_dat)
  setnames(clim_dat, old = "DDsub0_sp", new = "DD_0_sp")
  temp <- predict(BGCmodel, data = clim_dat)
  dat <- data.table(cellnum = clim_dat$id, bgc_pred = temp$predictions)
  pred_ls[[i]] <- dat
  rm(clim_dat)
  gc()
}

all_pred <- rbindlist(pred_ls)
all_pred[,bgc_id := as.numeric(as.factor(bgc_pred))]
values(final_dem) <- NA
final_dem[all_pred$cellnum] <- all_pred$bgc_id
writeRaster(final_dem, "BGC_Pred_200m.tif")
bgc_id <- unique(all_pred[,.(bgc_pred,bgc_id)])
fwrite(bgc_id, "BGC_ID.csv")

cols <- fread("./WNAv12_3_SubzoneCols.csv")
bgc_id[cols, colour := i.colour, on = c(bgc_pred = "classification")]

coltab(final_dem) <- bgc_id[,.(bgc_id,colour)]
plot(final_dem)
rgbbgc <- colorize(final_dem, to = "rgb")
writeRaster(rgbbgc, "BGC_Pred_RGB.tif")  


temp <- BGCmodel$forest$independent.variable.names
temp[!temp %in% names(clim_dat)]
b
