## Input data for CCISS spatial app
## Kiri Daust, Colin Mahony, 2023

library(data.table)
library(sf)
library(RPostgreSQL)
library(ccissdev)
library(pool)
library(RColorBrewer)
library(terra)
library(Rcpp)
library(climr)
library(ranger)

##db connections
sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##required functions
addVars <- function(dat){
  dat[,PPT_MJ := PPT05 + PPT06]
  dat[,PPT_JAS := PPT07 + PPT08 + PPT09]
  dat[,PPT.dormant := PPT_at + PPT_wt]
  dat[,CMD.def := 500 - PPT.dormant]
  dat[CMD.def < 0, CMD.def := 0]
  dat[,CMDMax := CMD07]
  dat[,CMD.total := CMD.def + CMD]
  dat[,DD_delayed := ((DD_0_at + DD_0_wt)*0.0238) - 1.8386]
  dat[DD_delayed < 0, DD_delayed := 0]
}

##trying to predict all pixels at once crashses due to RAM issues
##so this function predicts in segments
tile_predict <- function(Y1, pred_vars, maxSize = 6000000){
  n = nrow(Y1)
  brks <- seq(1,n,by = maxSize)
  brks <- c(brks,n)
  Y1[,BGC.pred := NA_character_]
  for(j in 1:(length(brks)-1)){
    Y1[brks[j]:brks[j+1],BGC.pred := predict(BGCmodel, Y1[brks[j]:brks[j+1],..pred_vars],num.threads = 10)[['predictions']]]
  }
  TRUE
}

cppFunction('NumericVector ModelDir(NumericMatrix x, NumericVector Curr, std::string dir){
  int n = x.nrow();
  NumericVector res(n);
  NumericVector temp(5);
  NumericVector temp2;
  double curr_suit;
  if(dir == "Improve"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(0,curr_suit)]);
    }
  }else{
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_back(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(curr_suit,4)]);
    }
  }
  
  return(res);
}
')

##Function to calculate feasibility from summarised predictions as used in CCISS tool
cciss_full <- function(SSPred,suit,spp_select){
  
  suit <- suit[Spp %in% spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
  # Fill with 0 if columns does not exist, encountered the error at SiteRef 3104856 
  colNms <- c("1","2","3","X")
  set(suitVotes, j = as.character(1:5)[!as.character(1:5) %in% names(suitVotes)], value = 0)
  
  suitVotes[,VoteSum := `1`+`2`+`3`+`4`+`5`]
  suitVotes[,X := 1 - VoteSum]
  suitVotes[,VoteSum := NULL]
  suitVotes[,X := X + `5` + `4`]
  suitVotes[,`:=`(`5` = NULL, `4` = NULL)]
  setkey(suitVotes, SS_NoSpace, Spp)
  setkey(suit, SS_NoSpace, Spp)
  suitVotes[suit, Curr := i.Feasible]
  suitVotes[is.na(Curr), Curr := 5]
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[Curr > 3.5, Curr := 4]
  
  suitVotes[,Improve := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Improve"),.SDcols = colNms]
  suitVotes[,Decline := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Decline"),.SDcols = colNms]
  datRot <- suitVotes[,lapply(.SD, mean),.SDcols = c("Improve","Decline"), by = list(SiteRef,SS_NoSpace,FuturePeriod,Spp,Curr)]
  datRot[,`:=`(Improve = round(Improve*100),Decline = round(Decline*100))]
  datRot[,Curr := NULL]
  
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,Newsuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitVotes <- merge(suitVotes, datRot, by = c('SiteRef','FuturePeriod','SS_NoSpace','Spp'),all = T)
  suitRes <- suitVotes[,.(Curr = mean(Curr),Newsuit = mean(Newsuit), Improve = mean(Improve), Decline = mean(Decline)), by = .(SiteRef,FuturePeriod,Spp)]
  return(suitRes)
}

##general cciss function - takes as input reference BGC, predicted BGC, edatopic position
cciss_basic <- function(bgc_preds, selected_edatope, selected_spp, suit_table){
  eda_table <- copy(E1)
  eda_table[,HasPos := if(any(Edatopic == selected_edatope)) T else F, by = .(SS_NoSpace)]
  eda_table <- eda_table[(HasPos),]
  eda_table <- eda_table[is.na(SpecialCode),]
  eda_table <- unique(eda_table[,.(BGC,SS_NoSpace)])
  setkey(eda_table, BGC)
  
  idCols <- names(bgc_preds)
  idCols <- idCols[!idCols %in% c("BGC.pred", "BGC.ref")]
  setkey(bgc_preds,BGC.ref)
  bgc_ss <- eda_table[bgc_preds, allow.cartesian = T]
  setnames(bgc_ss, old = c("BGC","SS_NoSpace"), new = c("BGC.ref", "SS.ref"))
  setkey(bgc_ss, BGC.pred)
  bgc_ss <- eda_table[bgc_ss, allow.cartesian = T]
  setnames(bgc_ss, old = c("BGC","SS_NoSpace"), new = c("BGC.pred", "SS.pred"))
  setorderv(bgc_ss, c(idCols))
  
  suit_table <- suit_table[Spp == selected_spp,]
  suit_table[,`:=`(BGC = NULL,
                   Spp = NULL)]
  setkey(bgc_ss, SS.ref)
  bgc_ss[suit_table, Feas.ref := i.Feasible, on = c(SS.ref = "SS_NoSpace")]
  setkey(bgc_ss, SS.pred)
  bgc_ss[suit_table, Feas.pred := i.Feasible, on = c(SS.pred = "SS_NoSpace")]
  
  feas_out <- bgc_ss[,.(Feas.ref = mean(Feas.ref), Feas.pred = mean(Feas.pred)),
                     by = c(idCols, "BGC.ref", "BGC.pred")]
  
  return(feas_out)
}

##==============================================================================

### -------------------------------------------------------
### study area setup
### -------------------------------------------------------

studyarea <- "BC"

# output directory for data created in this script
dir.create(file.path("spatial_app/data", studyarea))
outdir <- paste("spatial_app/data", studyarea, sep="/")

### -------------------------------------------------------
### common variables
#lookup tables
spps.lookup <- read.csv("./data-raw/data_tables/Tree speciesand codes_2.0_25Aug2021.csv")
edatope.name <- c("Medium-Mesic", "Poor-Subxeric", "Rich-Hygric")
BGCcolors <- read.csv("data-raw/data_tables/WNAv11_Zone_Colours.csv")

gcms <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")

edatopes <- c("B2", "C4", "D6")
edatope.names <- c("Poor-subxeric", "Medium-mesic", "Rich-hygric")

# ssps <- c("ssp126", "ssp245")
# ssp.names=c("SSP1-2.6", "SSP2-4.5")
ssps <- c("ssp245")
ssp.names=c("SSP2-4.5")

periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100") 
period.names=c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

#BGC color scheme
BGCcolors.BC <- read.csv("data-raw/data_tables/BGCzone_Colorscheme.csv")
BGCcolors <- read.csv("data-raw/data_tables/WNAv11_Zone_Colours.csv")
BGCcolors.subzone <- read.csv("data-raw/data_tables/WNAv12_3_SubzoneCols.csv")
BGCcolors$colour <- as.character(BGCcolors$colour)
BGCcolors$colour[match(BGCcolors.BC$zone, BGCcolors$classification)] <- as.character(BGCcolors.BC$HEX) # reset BC colors to traditional BGC zone colors
ColScheme <- BGCcolors$colour
levels.bgc <- BGCcolors.subzone[,1]
levels.zone <- BGCcolors[,1]
zone.lookup <- levels.bgc
for(i in levels.zone){ zone.lookup[grep(i,levels.bgc)] <- i }
write.csv(levels.bgc, "spatial_app/data/levels.bgc.csv", row.names = F) # ISSUE: LIKELY WE WANT TO JUST CREATE THIS AS A LOOKUP TABLE IN THE data-raw/data_tables/ DIRECTORY

##climr variables need for this model
vars_needed <- c("DD5","DD_0_at","DD_0_wt","PPT05","PPT06","PPT07","PPT08","PPT09","CMD","PPT_at","PPT_wt","CMD07","SHM", "AHM", "NFFD", "PAS", "CMI")

### -------------------------------------------------------
### dem and climr input table
### -------------------------------------------------------

if(studyarea=="BC"){
  dem <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/PRISM_dem.asc")
  dem <- aggregate(dem, fact=3)
  bnd <- vect(paste("spatial_app/bdy/bdy", studyarea, "shp", sep=".")) #boundary file
  bnd <- project(bnd,"epsg:4326") # project to albers to be able to specify resolution in meters. 
  dem <- mask(dem,bnd)
  dem <- trim(dem)
} else {
  ##make study area dem
  dem_source <- rast("../Common_Files/WNA_DEM_SRT_30m_cropped.tif") ##DEM - I'm using a 30 m one
  bnd <- vect(paste("spatial_app/bdy/bdy", studyarea, "shp", sep=".")) #boundary file
  bnd <- project(bnd,"epsg:4326") # project to albers to be able to specify resolution in meters. 
  land <- vect("//objectstore2.nrs.bcgov/ffec/Generic_Spatial_Data/Land_Water_SimplifyPolygon.shp")
  land <- project(land, "epsg:4326")
  land <- crop(land, bnd) #have to do this because of point roberts
  bnd <- crop(bnd, land)
  dem <- rast(bnd,res = 0.006) ## ENHANCEMENT NEEDED: CHANGE HARD-CODED RESOLUTION TO DYNAMIC RESOLUTION MATCHING USER-SPECIFIED NUMBER OF CELLS
  dem <- project(dem_source,dem, method="near") ## extract 30m dem values to the custom raster. use nearest neighbour to preserve elevation variance. 
  dem <- mask(dem,bnd)
}

# sum(!is.na(values(dem)))
# plot(dem)
# plot(bnd)
# plot(land, add=T, col="blue")

X <- dem # base raster
values(X) <- NA

## make the climr input file
points_dat <- as.data.frame(dem, cells=T, xy=T)
colnames(points_dat) <- c("id", "x", "y", "el")
points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
# values(X)[points_dat$id] <- points_dat$el ; plot(X)

## attribute BGCs to points
# bgcs <- st_read("//objectstore2.nrs.bcgov/ffec/WNA_BGC/WNA_BGC_v12_5Apr2022.gpkg") ##BGC map. takes forever to download for some reason... maybe vpn? 
bgcs <- st_read("../Common_Files/WNA_BGC_v12_5Apr2022.gpkg") ##BGC map.
# library(bcmaps)
# bgcs <- bec() ##BGC map from bcmaps package
points_sf <- st_as_sf(points_dat, coords = c("x","y"), crs = 4326)
points_sf <- st_transform(points_sf,3005)
bgc_att <- st_join(points_sf, bgcs)
bgc_att <- data.table(st_drop_geometry(bgc_att))
# X[points_dat$id] <- factor(bgc_att$MAP_LABEL, levels=levels.bgc); plot(X) # test


### -------------------------------------------------------
### export rasters and maps of reference biogeoclimatic units
### -------------------------------------------------------

# reference BGC units
bgc.ref <- bgc_att$BGC
values(X) <- NA
X[points_dat$id] <- factor(bgc.ref, levels=levels.bgc) 
writeRaster(X, datatype="FLT4S", paste(outdir,"/bgc.ref.tif",sep = "."), overwrite=T)

png(filename=paste("spatial_app/www/refmap", studyarea,"variants.png",sep="."), type="cairo", units="in", width=4, height=4, pointsize=11, res=200)
par(mar=c(0,0,0,0))
X[1:length(levels.bgc)] <- 1:length(levels.bgc) # this is a patch that is necessary to get the color scheme right.
plot(X, xaxt="n", yaxt="n", col=BGCcolors.subzone$colour, legend=FALSE, axes=F, bty="n", box=FALSE)
X[-(1:length(levels.bgc))] <- NA # cover up the color bar
plot(X, add=T, col="white", legend=FALSE) # cover up the color bar
plot(bnd, add=T, lwd=1, col=NA)
dev.off()

# reference BGC zones
zone.ref <- zone.lookup[match(bgc.ref, levels.bgc)]
values(X) <- NA
X[points_dat$id] <- factor(zone.ref, levels=levels.zone)
writeRaster(X, datatype="FLT4S", paste(outdir,"/zone.ref.tif",sep = "."), overwrite=T)

png(filename=paste("spatial_app//www/refmap", studyarea,"zones.png",sep="."), type="cairo", units="in", width=4, height=4, pointsize=11, res=200)
par(mar=c(0,0,0,0))
values(X)[1:length(levels.zone)] <- 1:length(levels.zone) # this is a patch that is necessary to get the color scheme right.
plot(X, xaxt="n", yaxt="n", col=ColScheme, legend=FALSE, axes=F, bty="n", box=FALSE)
values(X)[-(1:length(levels.zone))] <- NA # cover up the color bar
plot(X, add=T, col="white", legend=FALSE) # cover up the color bar
plot(bnd, add=T, lwd=1, col=NA)
dev.off()

write.csv(unique(bgc.ref[!is.na(bgc.ref)]), paste(outdir, "/bgcs.native.csv", sep="."), row.names = F)
write.csv(unique(zone.ref[!is.na(zone.ref)]), paste(outdir, "/zones.native.csv", sep="."), row.names = F)

# ===============================================================================
# ===============================================================================
# BGC Projections
# ===============================================================================
# ===============================================================================

load("../Common_Files/BGCModel_Extratrees_FullData.Rdata") ##load RF model
pred_vars <- BGCmodel[["forest"]][["independent.variable.names"]] ##required predictors

### -------------------------------------------------------
### BGC Projections for reference period
### -------------------------------------------------------

clim <- climr_downscale(points_dat,
                                 which_normal = "BC",
                                 gcm_models = NULL,
                                 return_normal = TRUE, ##1961-1990 period
                                 vars = list_variables())
addVars(clim)
identity.grid <- data.table(ID=clim$ID, GCM=rep("obs", dim(clim)[1]), SSP=rep("obs", dim(clim)[1]), RUN=rep(NA, dim(clim)[1]), PERIOD=clim$PERIOD)

## calculate mean climate of study area for use in calculating change
clim.refmean <- apply(as.data.frame(clim)[,-c(1:2)], 2, FUN=mean, na.rm=T)
write.csv(t(as.data.frame(clim.refmean)), paste(outdir, "/clim.refMean.csv", sep="."), row.names = F)

#initiate the table to store the climate change values (zeros because this is the reference period)
change <- data.frame("GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(t(rep(0, length(clim.refmean)))))
names(change)[-c(1:4)] <- names(clim.refmean)

#clean the points_dat and clim tables of NAs
points_dat <- points_dat[!is.nan(clim$CMI)|!is.na(clim$CMI),]
clim <- clim[!is.nan(CMI)|!is.na(CMI),]

# Predict BGC
tile_predict(clim,pred_vars=pred_vars) 
bgc_preds_ref <- clim[,.(ID,PERIOD,BGC.pred)] 

values(X) <- NA
X[bgc_preds_ref$ID] <- factor(bgc_preds_ref$BGC.pred, levels=levels.bgc) #ISSUE: THE LEVELS.BGC IS NOT ALIGNED WITH THE RF MODEL. NEED TO RESOLVE AND GET THE CORRECT LEVELS. 
# plot(X)
writeRaster(X, paste(outdir, "/BGC.pred.ref.tif", sep="."),overwrite=TRUE)

# # [ISSUE: THE LEVELS IN THE BGC MODEL DON'T APPEAR TO BE COMPLETE]
# test <- predict(BGCmodel, clim)
# levels.rf <- levels(test$predictions)
# levels.rf[-which(levels.rf%in%levels.bgc)]
# levels.bgc[-which(levels.bgc%in%levels.rf)]

### -------------------------------------------------------
### BGC Projections for historical periods
### -------------------------------------------------------

clim <- climr_downscale(points_dat,
                                 which_normal = "BC",
                                 gcm_models = NULL,
                                 historic_period = "2001_2020",
                                 return_normal = F, ##1961-1990 period
                                 vars = list_variables())
addVars(clim)

## calculate climate change
clim.mean <- apply(as.data.frame(clim)[,-c(1:2)], 2, FUN=mean, na.rm=T)
change.temp <- clim.mean - clim.refmean
change <- rbind(change, data.frame("GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(t(change.temp))))

# Predict BGC
clim <- clim[!is.nan(CMI)|!is.na(CMI),]
tile_predict(clim,pred_vars) 
bgc_preds_hist <- clim[,.(ID,PERIOD,BGC.pred)] 
bgc_preds_hist[bgc_preds_ref, BGC.ref := i.BGC.pred, on = "ID"]

values(X) <- NA
X[bgc_preds_hist$ID] <- factor(bgc_preds_hist$BGC.pred, levels=levels.bgc)
writeRaster(X, paste(outdir, "/BGC.pred.hist.2001_2020.tif", sep="."),overwrite=TRUE)

### -------------------------------------------------------
### BGC Projections for future periods
### -------------------------------------------------------

ssp=ssps[1]
for(ssp in ssps){
  period=periods[1]
  for(period in periods){

      # Climate data
      clim <- climr_downscale(points_dat,
                              which_normal = "BC",
                              gcm_models = gcms,
                              ssp = ssp,
                              gcm_period = period,
                              max_run = 3L,
                              return_normal = FALSE,
                              vars = list_variables())
      addVars(clim)
      unique(clim$GCM)
      
      ## calculate ensemble mean and append to clim
      clim.ensembleMean <- clim[RUN == "ensembleMean", lapply(.SD, mean), by = ID, .SDcols = !(ID:PERIOD)]
      identity <- data.table(
        ID = clim.ensembleMean$ID,
        GCM = rep("ensembleMean", dim(clim.ensembleMean)[1]),
        SSP = ssp, 
        RUN = rep("ensembleMean", dim(clim.ensembleMean)[1]), 
        PERIOD = period
        )
      clim.ensembleMean <- cbind(identity, clim.ensembleMean[,!"ID"])
      clim <- rbind(clim, clim.ensembleMean)

      ## calculate mean climate change across study area [ISSUE: REFACTOR TO DATA.TABLE]
      clim.mean <- as.data.frame(clim[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = .(GCM, SSP, RUN, PERIOD), .SDcols = !(ID:PERIOD)]) #mean value for each run across the study area. 
      change.temp <- sweep(clim.mean[,-c(1:4)], 2, clim.refmean, FUN='-') # subtract the reference period mean vector from each row. 
      change <- rbind(change, cbind(clim.mean[,c(1:4)], change.temp)) # append to the mean change table. 
      
      ## BGC projections 
      clim <- clim[!is.nan(CMI)|!is.na(CMI),]
      tile_predict(clim,pred_vars) ##predict BGC!
      bgc_preds_temp <- clim[,.(ID,GCM,SSP,RUN,PERIOD,BGC.pred)] ##this now has all the raw predictions
      bgc_preds_temp[bgc_preds_ref, BGC.ref := i.BGC.pred, on = "ID"]

      # append the predictions to the predictions table
      bgc_preds <- if(period==periods[1] & ssp==ssps[1]) bgc_preds_temp else rbind(bgc_preds, bgc_preds_temp)
      
    print(period)
  }
  print(ssp)
}

write.csv(change, paste(outdir, "/clim.meanChange.csv", sep="."), row.names = F)

rm(clim)
gc()

unique(change$GCM)
#===============================================================================
# export bgc projection rasters for a subset of simulations that represent the centroid and extremes of the ensemble
#===============================================================================

library(MASS)
source("KKZ.R") # this is the KKZ script provided by Alex Cannon

# use KKZ to select a subset of simulations that represent the centroid and extremes of the ensemble
select <- which(change$RUN != "ensembleMean" & change$SSP=="ssp245" & change$PERIOD == "2081_2100")
x <- change[select, which(names(change)%in%pred_vars)] 
id <- change[select, c(1,3)] 
x <- as.matrix(x[,]) # necessary for the subset.kkz function to work
x <- scale(x) #z-standardize the data
attr(x,"scaled:center")<-NULL
attr(x,"scaled:scale")<-NULL
x.kkz <- subset.kkz(x,n.cases=6) # this is the KKZ algorithm sourced from the KKZ.R script
id.kkz <- id[as.numeric(row.names(x.kkz$cases)),]
id.kkz <- rbind(id.kkz, data.frame(GCM="ensembleMean", RUN="ensembleMean")) # force the subset to include the ensemble mean
write.csv(id.kkz, paste(outdir, "/id.kkz.csv", sep="."), row.names = F)

# plot the subset in PCA space
x.pca <- predict(prcomp(x), x)
par(mar=c(3,3,0.1,0.1))
eqscplot(x.pca[,1:2], col="white")
text(x.pca[,1:2], rownames(x), cex=0.7)
points(x.pca[which(row.names(x.pca)%in%row.names(id.kkz)),1:2], col=2, cex=3)

# export rasters
for(ssp in ssps){
  for(period in periods){
    for(i in 1:dim(id.kkz)[1]){
      bgc.pred <- bgc_preds[GCM==id.kkz$GCM[i] & SSP==ssp & RUN==id.kkz$RUN[i] & PERIOD==period, BGC.pred]
      values(X) <- NA
      X[points_dat$id] <- factor(bgc.pred, levels=levels.bgc)
      # plot(X)
      writeRaster(X, paste(outdir, "/BGC.pred", id.kkz$GCM[i], id.kkz$RUN[i], ssp, period,"tif", sep="."),overwrite=TRUE)
    }
    print(period)
  }
  print(ssp)
}


#===============================================================================
# Make and export summary tables of bgc units for each future
#===============================================================================

# Reference bgc
index=1 # added this because rows are reordered (alphabetically) during dcast and i need a way to preserve row order. 
BGC.pred.ref <- bgc_preds_ref$BGC.pred
zone.pred.ref <- zone.lookup[match(BGC.pred.ref, levels.bgc)]
PredSum.bgc <- data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(table(BGC.pred.ref, dnn=c("bgc.pred"))))
PredSum.zone <- data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(table(zone.pred.ref, dnn=c("zone.pred"))))
PredSum.bgc.home <- data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(table(BGC.pred.ref[which(BGC.pred.ref == BGC.pred.ref)], dnn=c("bgc.pred"))))
PredSum.zone.home <- data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(table(zone.pred.ref[which(zone.pred.ref == zone.pred.ref)], dnn=c("zone.pred"))))

# Historical bgc
index <- index+1
bgc.pred <- bgc_preds_hist$BGC.pred
zone.pred <- zone.lookup[match(bgc.pred, levels.bgc)]
PredSum.bgc <- rbind(PredSum.bgc, data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(table(bgc.pred, dnn=c("bgc.pred")))))
PredSum.zone <- rbind(PredSum.zone, data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(table(zone.pred, dnn=c("zone.pred")))))
PredSum.bgc.home <- rbind(PredSum.bgc.home, data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(table(bgc.pred[which(bgc.pred == BGC.pred.ref)], dnn=c("bgc.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion
PredSum.zone.home <- rbind(PredSum.zone.home, data.frame(index=index, "GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(table(zone.pred[which(zone.pred == zone.pred.ref)], dnn=c("zone.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion

# Future bgc
for(ssp in ssps){
  for(period in periods){
    for(gcm in unique(bgc_preds$GCM)){
      runs <- unique(bgc_preds[GCM==gcm, RUN])
      run=runs[1]
      for(run in runs){
        index <- index+1
        # bgc predictions
        bgc.pred <- bgc_preds[GCM==gcm & SSP==ssp & RUN==run & PERIOD==period,]
        bgc.pred <- bgc.pred[order(ID), BGC.pred] # extra step here just to ensure that the values are in order of ascending ID. 
        
        # zone lists
        zone.pred <- zone.lookup[match(bgc.pred, levels.bgc)]
        
        #summary tables
        PredSum.bgc <- rbind(PredSum.bgc, data.frame(index=index, "GCM"=gcm, "SSP"=ssp, "RUN"=run, "PERIOD"=period, as.data.frame(table(bgc.pred, dnn=c("bgc.pred")))))
        PredSum.zone <- rbind(PredSum.zone, data.frame(index=index, "GCM"=gcm, "SSP"=ssp, "RUN"=run, "PERIOD"=period, as.data.frame(table(zone.pred, dnn=c("zone.pred")))))
        temp <- table(bgc.pred[which(bgc.pred == BGC.pred.ref)], dnn=c("bgc.pred")) # pulling this out to solve for edge case where there is no persistence (no matches between ref and pred)
        PredSum.bgc.home <- rbind(PredSum.bgc.home, data.frame(index=index, "GCM"=gcm, "SSP"=ssp, "RUN"=run, "PERIOD"=period, if(length(temp)==0) data.frame(bgc.pred=NA, Freq=NA) else as.data.frame(temp))) #within home range of each bgc unit, for calculations of persistence and expansion
        PredSum.zone.home <- rbind(PredSum.zone.home, data.frame(index=index, "GCM"=gcm, "SSP"=ssp, "RUN"=run, "PERIOD"=period, as.data.frame(table(zone.pred[which(zone.pred == zone.pred.ref)], dnn=c("zone.pred"))))) #within home range of each bgc unit, for calculations of persistence and expansion
        # print(run)
      }
      # print(gcm)
    }
    print(period)
  }
  print(ssp)
}

# write out summary of bgc units for each future. #ISSUE: NEED TO REFACTOR THIS WHOLE SCRIPT TO DATA.TABLE
PredSum.bgc.wide <- dcast(setDT(PredSum.bgc), index+GCM+SSP+RUN+PERIOD~bgc.pred, value.var = "Freq")
PredSum.zone.wide <- dcast(setDT(PredSum.zone), index+GCM+SSP+RUN+PERIOD~zone.pred, value.var = "Freq")
PredSum.bgc.home.wide <- dcast(setDT(PredSum.bgc.home), index+GCM+SSP+RUN+PERIOD~bgc.pred, value.var = "Freq")
if("NA" %in% names(PredSum.bgc.home.wide)) PredSum.bgc.home.wide <- PredSum.bgc.home.wide[,!"NA"] #remove the NA column if it exists
PredSum.zone.home.wide <- dcast(setDT(PredSum.zone.home), index+GCM+SSP+RUN+PERIOD~zone.pred, value.var = "Freq")
if("NA" %in% names(PredSum.zone.home.wide)) PredSum.zone.home.wide <- PredSum.zone.home.wide[,!"NA"]

write.csv(PredSum.bgc.wide, paste(outdir, "/PredSum.bgc.csv", sep="."), row.names = F)
write.csv(PredSum.zone.wide, paste(outdir, "/PredSum.zone.csv", sep="."), row.names = F)
write.csv(PredSum.bgc.home.wide, paste(outdir, "/PredSum.bgc.home.csv", sep="."), row.names = F)
write.csv(PredSum.zone.home.wide, paste(outdir, "/PredSum.zone.home.csv", sep="."), row.names = F)


#===============================================================================
#===============================================================================
# Species Feasibility Projections
#===============================================================================
#===============================================================================

##read feasibility table from db
S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

# select the species to run the analysis on
spps <- unique(S1$Spp)
spps <- spps[-which(spps=="X")]
spps.candidate <- spps.lookup$TreeCode[-which(spps.lookup$Exclude=="x")]
spps <- as.character(spps[which(spps%in%spps.candidate)] )
# spps <- c("Pl", "Fd", "Cw", "Ep", "Dr", "Hw", "Mb", "Pw", "Ss", "Ba", "Yc", "Hm")

# function for calculating fractional suitabilities. 
# ISSUE: RENAME SUIT TO FEAS THROUGHOUT THE SCRIPT
fractionize <- function(x){
  x[is.na(x)] <- 5 #set the NA values to suitability 5 (weights unsuitable a bit more heavily than suitable classes during averaging)
  x[x==4] <- 5 #set 4 to suitability 5
  x <- 1-(x-1)/4 #calculate as fractional suitability
  return(x)
}

## non-THLB BGCs for exclusion from area summaries 
## ISSUE: ADD LAKES TO THIS, OR REMOVE LAKE CELLS FROM THE ANALYSIS ENTIRELY
BGCs_notin_THLB <- read.csv("data-raw/data_tables/BGCs_notin_THLB.csv")
exclude <- bgc_att$id[which(bgc_att$MAP_LABEL%in%BGCs_notin_THLB$BGC[which(BGCs_notin_THLB$Exlude=="x")])]
include <- if(length(exclude)>0) bgc_att$id[-which(bgc_att$id%in%exclude)] else bgc_att$id

edatope="C4"
for(edatope in edatopes){
  
  #initiate tables to store summary values
  PredSum.suit <- data.frame(PredSum.bgc.wide[,1:5], as.data.frame(matrix(rep(NA, length(spps)*dim(PredSum.bgc.wide)[1]), dim(PredSum.bgc.wide)[1])))
  names(PredSum.suit)[-c(1:5)] <- spps
  PredSum.spp <- PredSum.suit
  PredSum.suit.home <- PredSum.suit #home is for counting cells within historical range. 
  PredSum.spp.home <- PredSum.suit
  
  spp="Fd"
  for(spp in spps){
    
    # get the suitability for the reference period and recent observed predicted BGC.
    suit.hist <- cciss_basic(bgc_preds_hist, edatope, spp, S1)
    
    #reference period suitabilities
    suit.ref <- suit.hist[ID%in%include]
    suit.ref <- fractionize(suit.ref[order(ID), Feas.ref]) # second step to ensure order of IDs is sequential
    outRange.ref <- which(suit.ref==0)
    row <- which(PredSum.suit$GCM=="obs" & PredSum.suit$PERIOD=="1961_1990")
    col <- which(names(PredSum.suit)==spp)
    PredSum.suit[row,col] <- round(sum(suit.ref))
    PredSum.spp[row,col] <- round(sum(suit.ref>0))
    PredSum.suit.home[row,col] <- round(sum(suit.ref[-outRange.ref]))
    PredSum.spp.home[row,col] <- round(sum((suit.ref>0)[-outRange.ref]))
    
    #recent observed climate
    suit.proj <- suit.hist[ID%in%include]
    suit.proj <- fractionize(suit.proj[order(ID), Feas.pred]) # second step to ensure order of IDs is sequential
    row <- which(PredSum.suit$GCM=="obs" & PredSum.suit$PERIOD=="2001_2020")
    PredSum.suit[row,col] <- round(sum(suit.proj))
    PredSum.spp[row,col] <- round(sum(suit.proj>0))
    PredSum.suit.home[row,col] <- round(sum(suit.proj[-outRange.ref]))
    PredSum.spp.home[row,col] <- round(sum((suit.proj>0)[-outRange.ref]))
    
    # Future Climates
    suit <- cciss_basic(bgc_preds, edatope, spp, S1)
    
    # Evaluate if species is too minor across all futures for inclusion in results
    total.area <- dim(points_dat)[1] # this is the total number of cells in the study area
    suit.area <- sum(table(suit$Feas.pred)[which(names(table(suit$Feas.pred))<4)])/(dim(unique(bgc_preds[,2:5]))[1]) #this is the average number of cells in which the species is suitable
    small <- suit.area/total.area < 0.01 # establish insignificant species for removal
    
    for(ssp in ssps){
      period="2001_2020"
      for(period in periods){
        for(gcm in unique(bgc_preds$GCM)){
          runs <- unique(suit[GCM==gcm & SSP==ssp, RUN])
          run=runs[1]
          for(run in runs){
            suit.proj <- suit[ID%in%include & GCM==gcm & SSP==ssp & RUN==run & PERIOD==period]
            suit.proj <- fractionize(suit.proj[order(ID), Feas.pred]) # second step to ensure order of IDs is sequential
            row <- which(PredSum.suit$GCM==gcm & PredSum.suit$SSP==ssp & PredSum.suit$RUN==run & PredSum.suit$PERIOD==period)
            PredSum.suit[row,col] <- round(sum(suit.proj))
            PredSum.spp[row,col] <- round(sum(suit.proj>0))
            PredSum.suit.home[row,col] <- round(sum(suit.proj[-outRange.ref]))
            PredSum.spp.home[row,col] <- round(sum((suit.proj>0)[-outRange.ref]))
            
            # print(run)
          }
          # print(gcm)
        }
        #===============================================================================
        # Write rasters of mean feasibility change and binary appearance across the ensemble
        if(small==F){
          suit.proj <- suit[SSP==ssp & PERIOD==period]
          suit.proj[is.na(Feas.ref), "Feas.ref"] <- 4
          suit.proj[is.na(Feas.pred), "Feas.pred"] <- 4
          suit.proj[, Feas.change := Feas.ref-Feas.pred]
          
          Projsuit <- dcast(suit.proj[RUN != "ensembleMean"], ID~GCM+RUN, value.var = "Feas.pred")
          Changesuit <- dcast(suit.proj[RUN != "ensembleMean"], ID~GCM+RUN, value.var = "Feas.change")
          
          # calculate ensemble mean suitability change. this isn't biased by missing suitability for exotic BGCs
          Changesuit.mean <- apply(as.data.frame(Changesuit[,!"ID"]), 1, mean, na.rm=T)
          
          values(X) <- NA
          values(X)[points_dat$id] <- Changesuit.mean
          # plot(X)
          writeRaster(X, paste(outdir, "/Spp.Changesuit", spp, edatope, ssp, period,"tif", sep="."),overwrite=TRUE)
          
          # binary appearance/disappearance
          outRange.ref.all <- which(fractionize(suit.hist[order(ID), Feas.ref])==0) #redo this without excluding non-THLB bgc units. 
          suit.ensemble <- as.matrix(Projsuit[,!"ID"])
          binary <- rep(0, dim(suit.hist)[1])
          binary[outRange.ref.all] <- NA
          binary[outRange.ref.all] <- apply(suit.ensemble[outRange.ref.all,], 1, function(x){return(if(sum(!is.na(x))==0) NA else if((sum(x<4, na.rm=T)/sum(!is.na(x)))>0) sum(x<4, na.rm=T)/sum(!is.na(x)) else NA)})
          binary[-outRange.ref.all] <- apply(suit.ensemble[-outRange.ref.all,], 1, function(x){return(0-sum(x==4, na.rm=T)/sum(!is.na(x)))})
          values(X) <- NA
          values(X)[points_dat$id] <- binary
          # plot(X)
          writeRaster(X, paste(outdir, "/Spp.binary", spp, edatope, ssp, period,"tif", sep="."),overwrite=TRUE)
        }

        # print(period)
      }
      # print(ssp)
    }
    print(paste(spp, " (", round(which(spps==spp)/length(spps)*100, 0), "%)", sep=""))
  }
  
  write.csv(PredSum.suit, paste(outdir, "/PredSum.suit", edatope,"csv", sep="."), row.names = F)
  write.csv(PredSum.spp, paste(outdir, "/PredSum.spp", edatope,"csv", sep="."), row.names = F)
  write.csv(PredSum.suit.home, paste(outdir, "/PredSum.suit.home", edatope,"csv", sep="."), row.names = F)
  write.csv(PredSum.spp.home, paste(outdir, "/PredSum.spp.home", edatope,"csv", sep="."), row.names = F)
  
  print(edatope)
}

