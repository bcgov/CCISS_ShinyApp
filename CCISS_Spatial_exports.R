## spatial climates!! ##
## Kiri Daust, Colin Mahoney, 2023

# =============================================================
# Issues:
# - need BGC projections for the 2001-2020 observed normals
# - need a function to calculate species suitability from a vector of BGCs. the current edatopicOverlap() function is specific to the mapdata_2km output
# - the future BGC projections do not have a consistent set of rast_ids, and a few thousand duplicate records. need to find out why. 
# - the function to calculate change in feasibility is currently uses mapped bgc as the reference bgc. it would be more correct to use the reference period predicted bgc as the reference bgc. 
# - ideally, we could have a function to supply a raster and return the biogeoclimatic projections for this raster. 
# =============================================================

library(data.table)
library(sf)
library(RPostgreSQL)
library(ccissdev)
library(pool)
library(RColorBrewer)
library(terra)
library(Rcpp)
library(climRdev)
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
  suitVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitVotes <- merge(suitVotes, datRot, by = c('SiteRef','FuturePeriod','SS_NoSpace','Spp'),all = T)
  suitRes <- suitVotes[,.(Curr = mean(Curr),NewSuit = mean(NewSuit), Improve = mean(Improve), Decline = mean(Decline)), by = .(SiteRef,FuturePeriod,Spp)]
  return(suitRes)
}

##general cciss function - takes as input reference BGC, predicted BGC, edatopic position
##bgc_preds must have these columns: c("ID", "GCM", "SSP", "RUN", "PERIOD", "BGC.pred", "BGC.ref")
cciss_basic <- function(bgc_preds, selected_edatope, selected_spp, suit_table){
  eda_table <- copy(E1)
  eda_table[,HasPos := if(any(Edatopic == selected_edatope)) T else F, by = .(SS_NoSpace)]
  eda_table <- eda_table[(HasPos),]
  eda_table <- eda_table[is.na(SpecialCode),]
  eda_table <- unique(eda_table[,.(BGC,SS_NoSpace)])
  setkey(eda_table, BGC)
  
  setkey(bgc_preds,BGC.ref)
  bgc_ss <- eda_table[bgc_preds, allow.cartesian = T]
  setnames(bgc_ss, old = c("BGC","SS_NoSpace"), new = c("BGC.ref", "SS.ref"))
  setkey(bgc_ss, BGC.pred)
  bgc_ss <- eda_table[bgc_ss, allow.cartesian = T]
  setnames(bgc_ss, old = c("BGC","SS_NoSpace"), new = c("BGC.pred", "SS.pred"))
  setorder(bgc_ss, ID, PERIOD, GCM, SSP, RUN)
  
  suit_table <- suit_table[Spp == selected_spp,]
  suit_table[,`:=`(BGC = NULL,
                   Spp = NULL)]
  setkey(bgc_ss, SS.ref)
  bgc_ss[suit_table, Feas.ref := i.Feasible, on = c(SS.ref = "SS_NoSpace")]
  setkey(bgc_ss, SS.pred)
  bgc_ss[suit_table, Feas.pred := i.Feasible, on = c(SS.pred = "SS_NoSpace")]
  
  feas_out <- bgc_ss[,.(Feas.ref = mean(Feas.ref), Feas.pred = mean(Feas.pred)),
                     by = .(ID, PERIOD,GCM,SSP,RUN, BGC.ref, BGC.pred)]
  
  return(feas_out)
}

#=================================================
## Use climr and rf model to predicts sunshine coast
##read in inputs
bnd <- st_read("spatial_files/bdy.Sunshine.shp") #boundary file
dem <- rast("../Common_Files/WNA_DEM_SRT_30m_cropped.tif") ##DEM - I'm using a 30 m one
#bgcs <- st_read("../Common_Files/WNA_BGC_v12_5Apr2022.gpkg") ##BGC map
##climr variables need for this model
vars_needed <- c("DD5","DD_0_at","DD_0_wt","PPT05","PPT06","PPT07","PPT08","PPT09","CMD","PPT_at","PPT_wt","CMD07","SHM", "AHM", "NFFD", "PAS", "CMI")

##read feasibility table from db
S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

##make raster
bnd <- vect(bnd)
bnd <- project(bnd,"epsg:3005")
bnd_rast <- rast(bnd,res = 600) ##600 meter raster
bnd_rast <- project(bnd_rast,"epsg:4326")
t1 <- crds(bnd_rast)
elev <- extract(dem,t1)
point_dat <- as.data.frame(cbind(t1,elev))
colnames(point_dat) <- c("x","y","elev")
point_dat$id <- seq_along(point_dat$x)

# ##attribute BGCs to points
# points_sf <- st_as_sf(point_dat, coords = c("x","y"), crs = 4326)
# points_sf <- st_transform(points_sf,3005)
# bgc_att <- st_join(points_sf, bgcs)
# bgc_att <- data.table(st_drop_geometry(bgc_att))
# bgc_att <- bgc_att[!is.na(BGC),]

##I'm running climr twice to avoid issues with RAM - might want to have a 
## built in routine if the input is too big
##Future periods
##this took about 15 mins on my laptop (100,000 points)

clim_245 <- climr_downscale(point_dat2,
                             which_normal = "auto",
                             gcm_models = c("ACCESS-ESM1-5","CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6","MPI-ESM1-2-HR", "MRI-ESM2-0"),
                             ssp = c("ssp245"),
                             gcm_period = c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100"),
                             max_run = 3L,
                             historic_period = "2001_2020",
                             return_normal = FALSE,
                             vars = vars_needed)
clim_245[is.na(GCM),GCM := "Observed"]

clim_126 <- climr_downscale(point_dat,
                             which_normal = "auto",
                            gcm_models = c("ACCESS-ESM1-5","CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6","MPI-ESM1-2-HR", "MRI-ESM2-0"),
                            ssp = c("ssp126"),
                             gcm_period = c("2001_2020","2021_2040", "2041_2060", "2061_2080", "2081_2100"),
                             max_run = 3L,
                             return_normal = FALSE,
                             vars = vars_needed)

##historic/current periods (including 2001-2020 modelled)
clim_historic <- climr_downscale(point_dat,
                             which_normal = "auto",
                             gcm_models = NULL,
                             return_normal = TRUE, ##1961-1990 period
                             vars = vars_needed)

# fwrite(clim_vars, "SunShineClim.csv")
#clim_vars <- fread("SunShineClim.csv")
#clim_vars <- rbind(clim_vars, clim_curr)
clim_dat <- rbind(clim_126,clim_245)
clim_dat <- clim_dat[RUN != "ensembleMean",] ##remove mean - or use just ensemble mean
addVars(clim_dat) ##derived variables
addVars(clim_historic)

load("../Common_Files/BGCModel_Extratrees_FullData.Rdata") ##load RF model
pred_vars <- BGCmodel[["forest"]][["independent.variable.names"]] ##required predictors

tile_predict(clim_dat,pred_vars) ##predict BGC!
tile_predict(clim_historic,pred_vars) ##predict for reference period
bgc_preds <- clim_dat[,.(ID,GCM,SSP,RUN,PERIOD,BGC.pred)] ##this now has all the raw predictions
rm(clim_245,clim_dat)
gc()

ref_preds <- clim_historic[,.(ID,BGC.pred)]
bgc_preds[ref_preds, BGC.ref := i.BGC.pred, on = "ID"]
#fwrite(bgc_preds,"Sunshine_BGCPreds.csv")

##'raw' feasibility - one feasibility for each model/ssp/run
feas_raw <- cciss_basic(bgc_preds, "C4","Fd", S1)

##summarised data
bgc_summary <- bgc_preds[,.(BGC.num = .N), by = .(ID, PERIOD,BGC.ref, BGC.pred)]
bgc_summary[,BGC.prop := BGC.num/sum(BGC.num), by = .(ID,PERIOD)]
setorder(bgc_summary,ID,PERIOD,BGC.pred)
bgc_summary[,BGC.num := NULL]
setcolorder(bgc_summary, c("ID","PERIOD","BGC.ref","BGC.pred","BGC.prop"))
setnames(bgc_summary, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))

##edatopic overlap - could do for all edatopes, but will take longer to run
selected_edatope = "C4"
eda_table <- copy(E1)
eda_table[,HasPos := if(any(Edatopic == selected_edatope)) T else F, by = .(SS_NoSpace)]
eda_table <- eda_table[(HasPos),]
sspreds <- edatopicOverlap(bgc_summary, eda_table, E1_Phase, onlyRegular = TRUE)

##calculate feasibility - can add as many species as desired
cciss_res <- cciss_full(sspreds, S1, c("Fd","Cw"))

##now just plotting some stuff
cciss_res <- cciss_res[Spp == "Fd",]
cciss_res[,FeasChange := Curr - NewSuit]
cciss_res <- cciss_res[FuturePeriod == "2061_2080",]
values(bnd_rast) <- NA
bnd_rast[cciss_res$SiteRef] <- cciss_res$FeasChange
#ensmean <- rast("SC_FeasChange_Fd_EnsMean.tif")
plot(bnd_rast)
#writeRaster(bnd_rast, "SC_FeasChange_Fd_Runs.tif")

##==============================================================================

##function for returning raw BGC predictions and site series predictions based on input spatial file
##cellsize is in meters, e.g. 1000 for 1km cells
# bgc_ss_spatial <- function(bnd, cellsize, dbCon, gcm_params){
#   bnd <- vect(bnd)
#   bnd <- project(bnd,"epsg:3005")
#   bnd_rast <- rast(bnd,res = cellsize)
#   bnd_vct <- as.polygons(bnd_rast)
#   bnd_cnt <- centroids(bnd_vct)
#   bnd_cnt <- st_as_sf(bnd_cnt)
#   bnd_cnt$id <- seq_along(bnd_cnt$geometry)
#   st_write(bnd_cnt, con, "temp_centroid", delete_layer = TRUE)
#   
#   
#   qry <- "select hex_grid.siteno hex_id, temp_centroid.id rast_id 
#   from hex_grid INNER JOIN temp_centroid
#   ON(ST_Intersects(temp_centroid.geometry, hex_grid.geom));"
#   message("Downloading Raw Data")
#   hex_pnts <- setDT(dbGetQuery(con, qry))
#   raw_bgc <- dbGetBGCPred(con, siteno = hex_pnts$hex_id)
#   raw_bgc[hex_pnts, rast_id := i.rast_id, on = c(siteno = "hex_id")]
#   message("Summarised Data...")
#   bgc <- dbGetCCISS(con, hex_pnts$hex_id, avg = F, modWeights = gcm_params)
#   bgc[,SiteRef := as.integer(SiteRef)]
#   bgc[hex_pnts, rast_id := i.rast_id, on = c(SiteRef = "hex_id")]
#   return(list(raster = bnd_rast, raw = raw_bgc, summary = bgc))
# }
# 
# 
# ###example use
# bnd <- st_read("spatial_files/bdy.Sunshine.shp")
# ##specify gcm parameters
# ##------------------------------------------------
# gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
#                                  "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
#                                  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
#                          weight = c(1,0,0,1,1,1,1,0,0,1,1,1,0))
# 
# rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
#                          weight = c(0.8,1,0.8,0))
# 
# gcm_params <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
# gcm_params[gcm_weight,wgcm := i.weight, on = "gcm"]
# gcm_params[rcp_weight,wrcp := i.weight, on = "rcp"]
# gcm_params[,weight := wgcm*wrcp]
# ##------------------------------------------------
# ## pull feasibility table from database
# 
# 
# ##run first function. Takes about 2 mins with cellsize 1000. 
# ##returns a list with reference raster, BGC predictions, and site series predictions
# out <- bgc_ss_spatial(bnd, cellsize = 1000, dbCon = con, gcm_params = gcm_params)
# 
# simple_edatopic <- function(bgc_preds, edatope, gcm_select, ssp_select, fp_select, eda_table = copy(ccissdev::E1)){
#   rawsub <- bgc_preds[gcm == gcm_select & futureperiod == fp_select & scenario == ssp_select,]
#   rawsub <- rawsub[,.(SiteRef = rast_id, FuturePeriod = futureperiod,BGC = bgc, BGC.pred = bgc_pred, BGC.prop = 1)]
#   eda_table[,HasPos := if(any(Edatopic == selected_edatope)) T else F, by = .(SS_NoSpace)]
#   eda_table <- eda_table[(HasPos),]
#   eda_table[,HasPos := NULL]
#   ss_preds <- edatopicOverlap(rawsub, eda_table, ccissdev::E1_Phase, onlyRegular = T)
#   return(ss_preds)
# }
# 
# simple_ss <- simple_edatopic(out$raw, edatope = "C4", gcm_select = "ACCESS-ESM1-5", ssp_select = "ssp370", fp_select = "2041")
# 
# cciss_res <- simple_cciss(simple_ss, suit = S1, spp_select = c("Fd"))
# 
# ##plot it
# fd_suit <- cciss_suit[Spp == "Fd",]
# fd_suit[,SuitChange := Curr - NewSuit]
# r <- out$raster
# r[fd_suit$SiteRef] <- fd_suit$SuitChange
# plot(r)
# 
# ##plot an example BGC prediction
# r <- out$raster
# raw <- out$raw
# temp <- raw[gcm == "ACCESS-ESM1-5" & scenario == "ssp370" & futureperiod == "2041",]
# temp[,bgc_pred := as.factor(bgc_pred)]
# r[temp$rast_id] <- temp$bgc_pred
# plot(r)
# 
# #====================================================================================
# ##add/retreat (figure 3b)
# add_retreat <- function(SSPred,suit,spp_select){
#   suit <- suit[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
#   suit <- unique(suit)
#   suit <- na.omit(suit)
#   SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
#   Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
#   SSPred <- na.omit(SSPred)
#   setkey(SSPred,SS.pred)
#   setkey(suit,SS_NoSpace)
#   suitMerge <- suit[SSPred, allow.cartesian = T]
#   #suitMerge <- na.omit(suitMerge)
#   setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
#   suit2 <- suit[,.(SS_NoSpace,Feasible)]
#   setnames(suit2, old = "Feasible",new = "OrigFeas")
#   suitMerge <- suit2[suitMerge, on = "SS_NoSpace",allow.cartesian =T]
#   suitMerge[OrigFeas > 3.5, OrigFeas := NA]
#   suitMerge[Feasible > 3.5, Feasible := NA]
#   suitMerge[,HasValue := if(any(!is.na(OrigFeas))|any(!is.na(Feasible))) T else F, by = .(SiteRef)]
#   suitMerge <- suitMerge[(HasValue),]
#   suitMerge <- suitMerge[,.(SiteRef,FuturePeriod,SS_NoSpace,OrigFeas,SS.pred,Feasible,SSprob)]
#   setnames(suitMerge, old = "Feasible", new = "PredFeas")
#   suitMerge[,Flag := NA_character_]
#   suitMerge[is.na(OrigFeas) & !is.na(PredFeas),Flag := "Expand"]
#   suitMerge[!is.na(OrigFeas) & is.na(PredFeas),Flag := "Retreat"]
#   suitMerge[!is.na(OrigFeas) & !is.na(PredFeas),Flag := "Same"]
#   suitMerge[is.na(OrigFeas) & is.na(PredFeas),Flag := "Same"]
#   suitMerge[,PropMod := sum(SSprob), by = .(SiteRef,FuturePeriod, Flag)]
#   suitMerge[,PropAll := sum(SSprob), by = .(SiteRef,FuturePeriod)]
#   suitMerge[,PropMod := PropMod/PropAll]
#   suitRes <- unique(suitMerge[,.(SiteRef,Flag,PropMod)])
#   suitRes[,SiteRef := as.integer(SiteRef)]
#   setkey(suitRes,SiteRef)
#   suitRes[Flag == "Retreat",PropMod := PropMod * -1]
# }
# 
# 
# ### -------------------------------------------------------
# ### -------------------------------------------------------
# ### common variables
# #lookup tables
# spps.lookup <- read.csv("./data-raw/data_tables/Tree speciesand codes_2.0_25Aug2021.csv")
# edatope.name <- c("Medium-Mesic", "Poor-Subxeric", "Rich-Hygric")
# BGCcolors <- read.csv("data-raw/data_tables/WNAv11_Zone_Colours.csv")
# 
# # base raster
# X <- raster("BC_Raster.tif")
# X <- raster::setValues(X,NA)
# 
# studyarea <- "BC"
# 
#   outdir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Shiny_Apps/ccsummary-cciss", sep="")
# 
# GCMs <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")
# 
# edatopes <- c("B2", "C4", "D6")
# edatope.names <- c("Poor-subxeric", "Medium-mesic", "Rich-hygric")
# 
# scenarios <- c("ssp126", "ssp245", "ssp370")
# scenario.names=c("SSP1-2.6", "SSP2-4.5", "RCP8.5")
# 
# proj.years <- c(2001, 2021, 2041, 2061, 2081)
# proj.year.names=c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")
# 
# #BGC color scheme
# BGCcolors.BC <- read.csv("data-raw/data_tables/BGCzone_Colorscheme.csv")
# BGCcolors <- read.csv("data-raw/data_tables/WNAv11_Zone_Colours.csv")
# BGCcolors.subzone <- read.csv("data-raw/data_tables/WNAv12_3_SubzoneCols.csv")
# BGCcolors$colour <- as.character(BGCcolors$colour)
# BGCcolors$colour[match(BGCcolors.BC$zone, BGCcolors$classification)] <- as.character(BGCcolors.BC$HEX) # reset BC colors to traditional BGC zone colors
# ColScheme <- factor(BGCcolors$colour, levels=BGCcolors$colour)
# levels.bgc <- BGCcolors.subzone[,1]
# levels.zone <- BGCcolors[,1]
# write.csv(levels.bgc, paste(outdir, "/data/levels.bgc.csv", sep="."), row.names = F)
# 
# 
# ### -------------------------------------------------------
# ### -------------------------------------------------------
# ### export rasters of biogeoclimatic projections
# 
# 
# ### -------------------------------------------------------
# ### export rasters of reference period BGC units
# 
# bgc <- setDT(dbGetQuery(con,paste0("select * from pts2km_current"))) 
# bgc$rast_id <- as.numeric(bgc$rast_id)
# str(bgc)
# 
# #reference BGC units
# bgc.ref <- unique(bgc[,c(1,2)])
# X <- raster::setValues(X,NA)
# X[bgc.ref$rast_id] <- factor(bgc.ref$bgc, levels=levels.bgc)
# plot(X)
# writeRaster(X, datatype="FLT4S", paste(outdir,"/data/bgc", studyarea, "ref.tif",sep = "."), overwrite=T)
# dim(bgc.ref)
# length(unique(bgc.ref$rast_id))
# 
# #reference BGC zones
# zone.ref <- rep(NA, dim(bgc.ref)[1])
# for(i in levels.zone){ zone.ref[grep(i,bgc.ref$bgc)] <- i }
# table(zone.ref)
# zone.ref <- factor(zone.ref, levels=levels.zone)
# X <- raster::setValues(X,NA)
# X[bgc.ref$rast_id] <- factor(zone.ref, levels=levels.zone)
# plot(X)
# writeRaster(X, datatype="FLT4S", paste(outdir,"/data/zone", studyarea, "ref.tif",sep = "."), overwrite=T)
# length(zone.ref)
# 
# # predicted BGC units for the reference period
# bgc.pred.ref <- unique(bgc[,c(1,3)])
# X <- raster::setValues(X,NA)
# X[bgc.pred.ref$rast_id] <- factor(bgc.pred.ref$bgc, levels=levels.bgc)
# plot(X)
# writeRaster(X, datatype="FLT4S", paste(outdir,"/data/bgc.pred", studyarea, "ref.tif",sep = "."), overwrite=T)
# dim(bgc.pred.ref)
# 
# # predicted BGC zones for the reference period
# zone.pred.ref <- rep(NA, dim(bgc.pred.ref)[1])
# for(i in levels.zone){ zone.pred.ref[grep(i,bgc.pred.ref$bgc)] <- i }
# table(zone.pred.ref)
# zone.pred.ref <- factor(zone.pred.ref, levels=levels.zone)
# X <- raster::setValues(X,NA)
# X[bgc.pred.ref$rast_id] <- factor(zone.pred.ref, levels=levels.zone)
# plot(X)
# writeRaster(X, datatype="FLT4S", paste(outdir,"/data/zone.pred", studyarea, "ref.tif",sep = "."), overwrite=T)
# length(zone.pred.ref)
# 
# #===============================================================================
# # create a reference BGC map for the app
# #===============================================================================
# 
# library(scales) # for alpha transparency
# png(filename=paste(outdir, "/www/refmap", studyarea,"zones.png",sep="."), type="cairo", units="in", width=4, height=4, pointsize=11, res=200)
# par(mar=c(0,0,0,0))
# X <- raster::setValues(X,NA)
# X[bgc.ref$rast_id] <- factor(zone.ref, levels=levels.zone)
# values(X)[1:length(levels.zone)] <- 1:length(levels.zone) # this is a patch that is necessary to get the color scheme right.
# plot(X, xaxt="n", yaxt="n", col=alpha(ColScheme, 1), legend=FALSE, axes=F, bty="n", legend.mar=0, box=FALSE) 
# values(X)[-(1:length(levels.zone))] <- NA # cover up the color bar
# image(X, add=T, col="white") # cover up the color bar
# # plot(bdy, add=T, lwd=1, col=NA)
# # box()
# dev.off()
# 
# png(filename=paste(outdir, "/www/refmap", studyarea,"variants.png",sep="."), type="cairo", units="in", width=4, height=4, pointsize=11, res=200)
# par(mar=c(0,0,0,0))
# X <- raster::setValues(X,NA)
# X[bgc.ref$rast_id] <- factor(bgc.ref$bgc, levels=levels.bgc)
# values(X)[1:length(BGCcolors.subzone$classification)] <- 1:length(BGCcolors.subzone$classification) # this is a patch that is necessary to get the color scheme right.
# plot(X, xaxt="n", yaxt="n", col=alpha(BGCcolors.subzone$colour, 1), legend=FALSE, axes=F, bty="n", legend.mar=0, box=FALSE) 
# values(X)[-(1:length(BGCcolors.subzone$classification))] <- NA # cover up the color bar
# image(X, add=T, col="white") # cover up the color bar
# # plot(bdy, add=T, lwd=1, col=NA)
# # box()
# dev.off()
# 
# #===============================================================================
# # BGC Projections for future periods
# #===============================================================================
# proj.year <- proj.years[2]
# for(proj.year in proj.years){
#   rast_ids <- vector() #the raster ids aren't unique and are slightly different for each layer, so i'm storing a list of all raster ids for later use in lining up the bgc projection vectors. 
#   scenario <- scenarios[2]
#   for(scenario in scenarios){
#   GCM <- GCMs[3]
#   for(GCM in GCMs){
#     
#     # write.csv(list(bgc.pred.proj$predictions), paste(outdir, "/data/bgc.pred",studyarea, GCM, scenario, proj.year,"csv", sep="."), row.names = F)
#     bgc <- setDT(dbGetQuery(con,paste0("select * from pts2km_future where futureperiod = '",proj.year.names[which(proj.years==proj.year)],"' and scenario = '",scenario,"' and gcm = '",GCM,"'"))) ##takes about 15 seconds
#     # str(bgc)
#     # length(unique(bgc$rast_id)) # not the same length as the reference period maps
#     # length(bgc$rast_id) # note there are duplicated rast_ids. 
#     bgc <- unique(bgc)
#     # str(bgc)
#     rast_ids <- c(rast_ids, bgc$rast_id)
#     
#     assign(paste("bgc.pred",GCM, scenario, proj.year,sep="."), bgc[,c(1,6)])
#     
#     # X <- raster::setValues(X,NA)
#     # X[bgc$rast_id] <- factor(bgc$bgc_pred, levels=levels.bgc)
#     # # plot(X)
#     # writeRaster(X, paste(outdir, "/data/bgc.pred", studyarea, GCM, scenario, proj.year,"tif", sep="."),overwrite=TRUE)
#     
#     print(GCM)
#   }
#   print(scenario)
#   }
#   assign(paste("rast_ids",proj.year,sep="."), unique(rast_ids))
#   print(proj.year)
# }
# 
# #check if rast_ids are the same 
# for(proj.year in proj.years){
#   print(length(get(paste("rast_ids",proj.year,sep="."))))
# }
# 
# #===============================================================================
# # Ensemble Mean bgc Projections for future periods
# #===============================================================================
# 
# zone.lookup <- levels.bgc
# for(i in levels.zone){ zone.lookup[grep(i,levels.bgc)] <- i }
# 
# # determine vote winner bgc and ensemble agreement (WARNING: takes about 1 minute per scenario/proj.year)
# for(scenario in scenarios){
# proj.year=proj.years[2]
# for(proj.year in proj.years){
#   rast_ids <- get(paste("rast_ids",proj.year,sep="."))
#   temp.bgc <- data.frame(rast_id = rast_ids, matrix(rep(NA, length(rast_ids)*length(GCMs)), nrow=length(rast_ids), ncol=length(GCMs)))
#   names(temp.bgc) <- c("rast_id", GCMs)
#   temp.zone <- temp.bgc
#   for(GCM in GCMs){
#     bgc.pred <- get(paste("bgc.pred",GCM, scenario, proj.year,sep="."))
# 
#     #add votes to votes matrix
#     temp.bgc[match(bgc.pred$rast_id, temp.bgc$rast_id),which(names(temp.bgc)==GCM)] <- bgc.pred$bgc_pred
#     temp.zone[match(bgc.pred$rast_id, temp.zone$rast_id),which(names(temp.zone)==GCM)] <- zone.lookup[match(bgc.pred$bgc_pred, levels.bgc)]
#     # print(GCM)
#   }
#   vote.winner <- function(x){return(names(which(table(x)==max(table(x))))[1])}
#   agreement <- function(x){return(max(table(x)))}
#   assign(paste("bgc.pred.ensemble", scenario, proj.year, sep="."), data.frame(rast_id = temp.bgc[1], bgc.pred = apply(temp.bgc[-1], 1, vote.winner)))
#   assign(paste("zone.pred.ensemble", scenario, proj.year, sep="."), data.frame(rast_id = temp.zone[1], bgc.pred = apply(temp.zone[-1], 1, vote.winner)))
#   # assign(paste("bgc.pred.agreement", scenario, proj.year, sep="."), apply(temp.bgc, 1, agreement))
#   # write.csv(get(paste("bgc.pred.ensemble", scenario, proj.year, sep=".")), paste(outdir, "/data/bgc.pred",studyarea, "ensemble", scenario, proj.year,"csv", sep="."), row.names = F)
#   # write.csv(agreement, paste(outdir, "/data/bgc.pred",studyarea, "agreement", scenario, proj.year,"csv", sep="."), row.names = F)
#   
#   X <- raster::setValues(X,NA)
#   X[temp.bgc$rast_id] <- factor(get(paste("bgc.pred.ensemble", scenario, proj.year, sep="."))[,2], levels=levels.bgc)
#   # plot(X)
#   writeRaster(X, paste(outdir, "/data/bgc.pred", studyarea, "ensemble", scenario, proj.year,"tif", sep="."),overwrite=TRUE)
#   X[temp.zone$rast_id] <- factor(get(paste("zone.pred.ensemble", scenario, proj.year, sep="."))[,2], levels=levels.zone)
#   # plot(X)
#   writeRaster(X, paste(outdir, "/data/zone.pred", studyarea, "ensemble", scenario, proj.year,"tif", sep="."),overwrite=TRUE)
#   
#   print(proj.year)
# }
# print(scenario)
# }
# 
# #===============================================================================
# # Make and export summary tables of bgc units for each future
# #===============================================================================
# 
# PredSum.bgc <- data.frame("GCM"="obs", "scenario"="obs", "proj.year"=1961, as.data.frame(table(bgc.pred.ref[,2], dnn=c("bgc.pred"))))
# PredSum.zone <- data.frame("GCM"="obs", "scenario"="obs", "proj.year"=1961, as.data.frame(table(zone.pred.ref, dnn=c("zone.pred"))))
# PredSum.bgc.home <- data.frame("GCM"="obs", "scenario"="obs", "proj.year"=1961, as.data.frame(table(bgc.pred.ref[which(bgc.pred.ref[,2] == bgc.pred.ref[,2]),2], dnn=c("bgc.pred"))))
# PredSum.zone.home <- data.frame("GCM"="obs", "scenario"="obs", "proj.year"=1961, as.data.frame(table(zone.pred.ref[which(zone.pred.ref == zone.pred.ref)], dnn=c("zone.pred"))))
# 
# # mapped bgc
#   PredSum.bgc <- rbind(PredSum.bgc, data.frame("GCM"="map", "scenario"="obs", "proj.year"=1961, as.data.frame(table(bgc.ref[,2], dnn=c("bgc.pred")))))
#   PredSum.zone <- rbind(PredSum.zone, data.frame("GCM"="map", "scenario"="obs", "proj.year"=1961, as.data.frame(table(zone.ref, dnn=c("zone.pred")))))
#   PredSum.bgc.home <- rbind(PredSum.bgc.home, data.frame("GCM"="map", "scenario"="obs", "proj.year"=1961, as.data.frame(table(bgc.ref[which(bgc.ref[,2] == bgc.pred.ref[,2]),2], dnn=c("bgc.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion
#   PredSum.zone.home <- rbind(PredSum.zone.home, data.frame("GCM"="map", "scenario"="obs", "proj.year"=1961, as.data.frame(table(zone.ref[which(zone.ref == zone.pred.ref)], dnn=c("zone.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion
#   
# #   # Historical bgc
# # hist.years=2001
# # hist.year=2001
# # for(hist.year in hist.years){
# #   bgc.pred <- bgc.pred.hist
# #   zone.pred <- rep(NA, length(bgc.pred))
# #   for(i in BGCcolors.BC$zone){ zone.pred[grep(i,bgc.pred)] <- i }
# #   PredSum.bgc <- rbind(PredSum.bgc, data.frame("GCM"="obs", "scenario"="obs", "proj.year"=2005, as.data.frame(table(bgc.pred))))
# #   PredSum.zone <- rbind(PredSum.zone, data.frame("GCM"="obs", "scenario"="obs", "proj.year"=2005, as.data.frame(table(zone.pred))))
# #   PredSum.bgc.home <- rbind(PredSum.bgc.home, data.frame("GCM"="obs", "scenario"="obs", "proj.year"=2005, as.data.frame(table(bgc.pred[which(bgc.pred == bgc.pred.ref)])))) #within home range of each bgc unit, for calcuations of persistence and expansion
# #   PredSum.zone.home <- rbind(PredSum.zone.home, data.frame("GCM"="obs", "scenario"="obs", "proj.year"=2005, as.data.frame(table(zone.pred[which(zone.pred == zone.pred.ref)])))) #within home range of each bgc unit, for calcuations of persistence and expansion
# #   print(hist.year)
# # }
# 
#   # Future bgc
#   for(scenario in scenarios){
#     for(proj.year in proj.years){
#       for(GCM in c("ensemble", GCMs)){
#         # bgc predictions
#         bgc.pred <- get(paste("bgc.pred",GCM, scenario, proj.year,sep="."))
# 
#         # create vectors of reference and projected that match on rast_id, since they don't have the same id list. using this method:  z <- rep(NA, length(y)); x <- c(1,2,3,7,8,9)+100; y <- 101:109; z[match(x,y)] <- x; z
#         rast_id <- unique(c(bgc.pred$rast_id, bgc.pred.ref$rast_id)) # create a common list of rast_ids for the reference and predicted bgc
#         bgc.temp <- rep(NA, length(rast_id)); bgc.temp.ref <- rep(NA, length(rast_id))
#         bgc.temp[match(bgc.pred$rast_id, rast_id)] <- bgc.pred$bgc_pred
#         bgc.temp.ref[match(bgc.pred.ref$rast_id, rast_id)] <- bgc.pred.ref$bgc_pred
#         # length(rast_id); length(bgc.pred.ref$bgc_pred); length(bgc.temp.ref); length(bgc.temp)
#         
#         # zone lists
#         zone.temp <- zone.lookup[match(bgc.temp, levels.bgc)]
#         zone.temp.ref <- zone.lookup[match(bgc.temp.ref, levels.bgc)]
#         
#         #summary tables
#         PredSum.bgc <- rbind(PredSum.bgc, data.frame("GCM"=GCM, "scenario"=scenario, "proj.year"=proj.year, as.data.frame(table(bgc.temp, dnn=c("bgc.pred")))))
#         PredSum.zone <- rbind(PredSum.zone, data.frame("GCM"=GCM, "scenario"=scenario, "proj.year"=proj.year, as.data.frame(table(zone.temp, dnn=c("zone.pred")))))
#         PredSum.bgc.home <- rbind(PredSum.bgc.home, data.frame("GCM"=GCM, "scenario"=scenario, "proj.year"=proj.year, as.data.frame(table(bgc.temp[which(bgc.temp == bgc.temp.ref)], dnn=c("bgc.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion
#         PredSum.zone.home <- rbind(PredSum.zone.home, data.frame("GCM"=GCM, "scenario"=scenario, "proj.year"=proj.year, as.data.frame(table(zone.temp[which(zone.temp == zone.temp.ref)], dnn=c("zone.pred"))))) #within home range of each bgc unit, for calcuations of persistence and expansion
#         # print(GCM)
#       }
#       print(proj.year)
#     }
#     print(scenario)
#   }
# 
# # write out summary of bgc units for each future. 
# PredSum.bgc.wide <- reshape(PredSum.bgc, idvar = c(names(PredSum.bgc)[1:3]), timevar = "bgc.pred", direction = "wide")
# PredSum.zone.wide <- reshape(PredSum.zone, idvar = c(names(PredSum.zone)[1:3]), timevar = "zone.pred", direction = "wide")
# PredSum.bgc.home.wide <- reshape(PredSum.bgc.home, idvar = c(names(PredSum.bgc.home)[1:3]), timevar = "bgc.pred", direction = "wide")
# PredSum.zone.home.wide <- reshape(PredSum.zone.home, idvar = c(names(PredSum.zone.home)[1:3]), timevar = "zone.pred", direction = "wide")
# names(PredSum.bgc.wide) <- gsub("Freq.","",names(PredSum.bgc.wide))
# names(PredSum.zone.wide) <- gsub("Freq.","",names(PredSum.zone.wide))
# names(PredSum.bgc.home.wide) <- gsub("Freq.","",names(PredSum.bgc.home.wide))
# names(PredSum.zone.home.wide) <- gsub("Freq.","",names(PredSum.zone.home.wide))
# write.csv(PredSum.bgc.wide, paste(outdir, "/data/PredSum.bgc",studyarea,"csv", sep="."), row.names = F)
# write.csv(PredSum.zone.wide, paste(outdir, "/data/PredSum.zone",studyarea,"csv", sep="."), row.names = F)
# write.csv(PredSum.bgc.home.wide, paste(outdir, "/data/PredSum.bgc.home",studyarea,"csv", sep="."), row.names = F)
# write.csv(PredSum.zone.home.wide, paste(outdir, "/data/PredSum.zone.home",studyarea,"csv", sep="."), row.names = F)
# 
# #===============================================================================
# #===============================================================================
# # Species Feasibility Projections
# #===============================================================================
# #===============================================================================
# 
# # complete this once the issues above have been resolved. 