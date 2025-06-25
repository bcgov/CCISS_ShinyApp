### Colin's summary exports

library(data.table)
library(ccissr)
library(climr)
library(terra)
library(bcmaps)
library(ranger)
library(RPostgres)
library(pool)

dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

# function for calculating fractional suitabilities. 
# ISSUE: RENAME SUIT TO FEAS THROUGHOUT THE SCRIPT
fractionize <- function(x){
  x[is.na(x)] <- 5 #set the NA values to suitability 5 (weights unsuitable a bit more heavily than suitable classes during averaging)
  x[x==4] <- 5 #set 4 to suitability 5
  x <- 1-(x-1)/4 #calculate as fractional suitability
  return(x)
}

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

dat <- bcmaps::nr_districts()

spps.lookup <- get(data("T1"))
edatope.name <- c("Medium-Mesic", "Poor-Subxeric", "Rich-Hygric")

gcms <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")

edatopes <- c("B2", "C4", "D6")
edatope.names <- c("Poor-subxeric", "Medium-mesic", "Rich-hygric")

# ssps <- c("ssp126", "ssp245")
# ssp.names=c("SSP1-2.6", "SSP2-4.5")
ssps <- c("ssp245")
ssp.names=c("SSP2-4.5")

periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100") 
period.names=c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

BGCcolors.BC <- read.csv("BGCzone_Colorscheme.csv") #TODO. make this a data object in ccissr or change the colors in the zone colours data object
BGCcolors <- as.data.frame(get(data("zones_colours_ref")))
BGCcolors.subzone <- as.data.frame(get(data("subzones_colours_ref")))
BGCcolors$colour <- as.character(BGCcolors$colour)
BGCcolors$colour[match(BGCcolors.BC$zone, BGCcolors$classification)] <- as.character(BGCcolors.BC$HEX) # reset BC colors to traditional BGC zone colors
ColScheme <- BGCcolors$colour
levels.bgc <- BGCcolors.subzone[,1]
levels.zone <- BGCcolors[,1]
zone.lookup <- levels.bgc
for(i in levels.zone){ zone.lookup[grep(i,levels.bgc)] <- i }

## attribute BGCs to points
#bgcs <- st_read("//objectstore2.nrs.bcgov/ffec/WNA_BGC/WNA_BGC_v12_5Apr2022.gpkg") ##BGC map. takes forever to download for some reason... maybe vpn?
bgcs <- st_read("../Common_Files/WNA_BGC_v13_15Nov2024.gpkg") ##BGC map.

####step 1
load("../Common_Files/BGC_RFresp.Rdata") ##load RF model
pred_vars <- BGC_RFresp[["forest"]][["independent.variable.names"]] ##required predictors
BGCmodel <- BGC_RFresp

dem.source <- rast("../Common_Files/composite_WNA_dem.tif")
dem.source <- aggregate(dem.source, fact = 3)

dbExecute(dbCon, "drop table if exists clim_change, predsum_bgc, predsum_zone, predsum_suit, predsum_spp, su_meta")

for(dist_i in seq_along(dat$ORG_UNIT)[-c(1:16)]){
  studyarea <- dat$ORG_UNIT[dist_i]
  bnd <- dat$geometry[dist_i]
  message("Processing ", studyarea)
  
  bnd <- vect(bnd)
  bnd <- project(bnd,"epsg:4326") # 
  
  dcrop <- crop(dem.source, bnd)
  # fct <- as.integer(sqrt(ncell(dcrop)/80000))
  # dem <- aggregate(dcrop, fct)
  dem <- project(dcrop,"epsg:4326") # 
  dem <- mask(dem,bnd)
  
  metadt <- data.table(studyarea = studyarea, res = res(dem)[1], mn_lat = mean(ext(dem)[3:4]), tot_area = ncell(dem[!is.na(dem)]))
  print(metadt)
  dbWriteTable(dbCon, "su_meta", metadt, row.names = F, append = T)
#}
  ## make the climr input file
  points_dat <- as.data.frame(dem, cells=T, xy=T)
  colnames(points_dat) <- c("id", "lon", "lat", "elev")
  points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input
  # values(X)[points_dat$id] <- points_dat$el ; plot(X)
  
  
  # library(bcmaps)
  # bgcs <- bec() ##BGC map from bcmaps package
  points_sf <- st_as_sf(points_dat, coords = c("lon", "lat"), crs = 4326)
  points_sf <- st_transform(points_sf,3005)
  bgc_att <- st_join(points_sf, bgcs)
  bgc_att <- data.table(st_drop_geometry(bgc_att))
  
  ### -------------------------------------------------------
  ### BGC Projections for reference period
  ### -------------------------------------------------------
  
  clim <- downscale(points_dat,
                    which_refmap  = "refmap_prism",
                    gcms  = NULL,
                    return_refperiod  = TRUE, 
                    vars = list_vars())
  addVars(clim)
  identity.grid <- data.table(id=clim$id, GCM=rep("obs", dim(clim)[1]), SSP=rep("obs", dim(clim)[1]), RUN=rep(NA, dim(clim)[1]), PERIOD=clim$PERIOD)
  ## calculate mean climate of study area for use in calculating change
  clim.refmean <- apply(as.data.frame(clim)[,-c(1:2)], 2, FUN=mean, na.rm=T)
  #write.csv(t(as.data.frame(clim.refmean)), paste(outdir, "/clim.refMean.csv", sep="."), row.names = F)
  
  #initiate the table to store the climate change values (zeros because this is the reference period)
  change <- data.frame("GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="1961_1990", as.data.frame(t(rep(0, length(clim.refmean)))))
  names(change)[-c(1:4)] <- names(clim.refmean)
  
  #clean the points_dat and clim tables of NAs
  points_dat <- points_dat[!is.nan(clim$CMI)|!is.na(clim$CMI),]
  clim <- clim[!is.nan(CMI)|!is.na(CMI),]
  
  tile_predict(clim,pred_vars=pred_vars) 
  bgc_preds_ref <- clim[,.(id,PERIOD,BGC.pred)] 
  
  ### -------------------------------------------------------
  ### BGC Projections for historical periods
  ### -------------------------------------------------------
  
  clim <- downscale(points_dat,
                    which_refmap  = "refmap_prism",
                    gcms  = NULL,
                    obs_periods = "2001_2020",
                    return_refperiod  = F, 
                    vars = list_vars())
  addVars(clim)
  
  ## calculate climate change
  clim.mean <- apply(as.data.frame(clim)[,-c(1:2)], 2, FUN=mean, na.rm=T)
  change.temp <- clim.mean - clim.refmean
  change <- rbind(change, data.frame("GCM"="obs", "SSP"="obs", "RUN"=NA, "PERIOD"="2001_2020", as.data.frame(t(change.temp))))
  
  # Predict BGC
  clim <- clim[!is.nan(CMI)|!is.na(CMI),]
  tile_predict(clim,pred_vars) 
  bgc_preds_hist <- clim[,.(id,PERIOD,BGC.pred)] 
  bgc_preds_hist[bgc_preds_ref, BGC.ref := i.BGC.pred, on = "id"]
  ### -------------------------------------------------------
  ### BGC Projections for future periods
  ### -------------------------------------------------------
  
  ssp=ssps[1]
  for(ssp in ssps){
    #period=periods[1]
    for(period in periods){
      
      # Climate data
      clim <- downscale(points_dat,
                        which_refmap  = "refmap_prism",
                        gcms = gcms,
                        ssps = ssp,
                        gcm_periods = period,
                        max_run = 3L,
                        return_refperiod = FALSE,
                        vars = list_vars())
      addVars(clim)
      unique(clim$GCM)
      
      ## calculate ensemble mean and append to clim
      clim.ensembleMean <- clim[RUN == "ensembleMean", lapply(.SD, mean), by = id, .SDcols = !(id:PERIOD)]
      identity <- data.table(
        id = clim.ensembleMean$id,
        GCM = rep("ensembleMean", dim(clim.ensembleMean)[1]),
        SSP = ssp, 
        RUN = rep("ensembleMean", dim(clim.ensembleMean)[1]), 
        PERIOD = period
      )
      clim.ensembleMean <- cbind(identity, clim.ensembleMean[,!"id"])
      clim <- rbind(clim, clim.ensembleMean)
      
      ## calculate mean climate change across study area [ISSUE: REFACTOR TO DATA.TABLE]
      clim.mean <- as.data.frame(clim[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = .(GCM, SSP, RUN, PERIOD), .SDcols = !(id:PERIOD)]) #mean value for each run across the study area. 
      change.temp <- sweep(clim.mean[,-c(1:4)], 2, clim.refmean, FUN='-') # subtract the reference period mean vector from each row. 
      change <- rbind(change, cbind(clim.mean[,c(1:4)], change.temp)) # append to the mean change table. 
      
      ## BGC projections 
      clim <- clim[!is.nan(CMI)|!is.na(CMI),]
      tile_predict(clim,pred_vars) ##predict BGC!
      bgc_preds_temp <- clim[,.(id,GCM,SSP,RUN,PERIOD,BGC.pred)] ##this now has all the raw predictions
      bgc_preds_temp[bgc_preds_ref, BGC.ref := i.BGC.pred, on = "id"]
      
      # append the predictions to the predictions table
      bgc_preds <- if(period==periods[1] & ssp==ssps[1]) bgc_preds_temp else rbind(bgc_preds, bgc_preds_temp)
      
      print(period)
    }
    print(ssp)
  }
  
  setDT(change)
  change_long <- melt(change, id.vars = c("GCM","SSP","RUN","PERIOD"), value.name = "value", variable.name = "var")
  setnames(change_long, c("gcm","ssp","run","period","var","value"))
  change_long[,studyarea := studyarea]
  dbWriteTable(dbCon, "clim_change", change_long, row.names = FALSE, append = TRUE)
  
  #mean_change_ls[[studyarea]] <- change
  #write.csv(change, paste(outdir, "/clim.meanChange.csv", sep="."), row.names = F)
  
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
          bgc.pred <- bgc.pred[order(id), BGC.pred] # extra step here just to ensure that the values are in order of ascending id. 
          
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

  ##subzones
  setDT(PredSum.bgc)
  setDT(PredSum.bgc.home)
  setnames(PredSum.bgc, c("index","gcm","ssp","run","period","bgc_pred","freq"))
  setnames(PredSum.bgc.home, c("index","gcm","ssp","run","period","bgc_pred","freq"))
  PredSum.bgc[,home := FALSE]
  PredSum.bgc.home[,home := TRUE]
  predsum_bgc <- rbind(PredSum.bgc,PredSum.bgc.home)
  predsum_bgc <- predsum_bgc[!is.na(bgc_pred),]
  predsum_bgc[,studyarea := studyarea]
  dbWriteTable(dbCon, "predsum_bgc",predsum_bgc, row.names = FALSE, append = TRUE)
  
  ##zones
  setDT(PredSum.zone)
  setDT(PredSum.zone.home)
  setnames(PredSum.zone, c("index","gcm","ssp","run","period","zone_pred","freq"))
  setnames(PredSum.zone.home, c("index","gcm","ssp","run","period","zone_pred","freq"))
  PredSum.zone[,home := FALSE]
  PredSum.zone.home[,home := TRUE]
  predsum_zone <- rbind(PredSum.zone,PredSum.zone.home)
  predsum_zone <- predsum_zone[!is.na(zone_pred),]
  predsum_zone[,studyarea := studyarea]
  dbWriteTable(dbCon, "predsum_zone",predsum_zone, row.names = FALSE, append = TRUE)
  
  #===============================================================================
  #===============================================================================
  # Species Feasibility Projections
  #===============================================================================
  #===============================================================================
  
  ##read feasibility table from db
  data(S1)
  S1 <- S1[,.(bgc,ss_nospace, spp, newfeas)]
  setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))
  
  # select the species to run the analysis on
  spps <- unique(S1$Spp)
  spps <- spps[-which(spps=="X")]
  spps.candidate <- spps.lookup$TreeCode[-which(spps.lookup$Exclude=="x")]
  spps <- as.character(spps[which(spps%in%spps.candidate)] )
  # spps <- c("Pl", "Fd", "Cw", "Ep", "Dr", "Hw", "Mb", "Pw", "Ss", "Ba", "Yc", "Hm")
  
  ## non-THLB BGCs for exclusion from area summaries 
  # ## ISSUE: ADD LAKES TO THIS, OR REMOVE LAKE CELLS FROM THE ANALYSIS ENTIRELY
  # BGCs_notin_THLB <- data("BGCs_notin_THLB") # this won't work until the pull request is merged into the loaded version of ccissr. 
  # exclude <- bgc_att$id[which(bgc_att$MAP_LABEL%in%BGCs_notin_THLB$BGC[which(BGCs_notin_THLB$Exlude=="x")])]
  # include <- if(length(exclude)>0) bgc_att$id[-which(bgc_att$id%in%exclude)] else bgc_att$id
  #dbExecute(dbCon, "drop table predsum_suit")
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
      #suit.ref <- suit.hist[id%in%include]
      suit.ref <- fractionize(suit.hist[order(id), Feas.ref]) # second step to ensure order of ids is sequential
      outRange.ref <- which(suit.ref==0)
      row <- which(PredSum.suit$GCM=="obs" & PredSum.suit$PERIOD=="1961_1990")
      col <- which(names(PredSum.suit)==spp)
      PredSum.suit[row,col] <- round(sum(suit.ref))
      PredSum.spp[row,col] <- round(sum(suit.ref>0))
      PredSum.suit.home[row,col] <- round(sum(suit.ref[-outRange.ref]))
      PredSum.spp.home[row,col] <- round(sum((suit.ref>0)[-outRange.ref]))
      
      #recent observed climate
      suit.proj <- suit.hist#[id%in%include]
      suit.proj <- fractionize(suit.proj[order(id), Feas.pred]) # second step to ensure order of ids is sequential
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
              suit.proj <- suit[GCM==gcm & SSP==ssp & RUN==run & PERIOD==period,] #id%in%include & 
              suit.proj <- fractionize(suit.proj[order(id), Feas.pred]) # second step to ensure order of ids is sequential
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
          
          # print(period)
        }
        # print(ssp)
      }
      print(paste(spp, " (", round(which(spps==spp)/length(spps)*100, 0), "%)", sep=""))
    }
    
    ##spp
    setDT(PredSum.spp)
    setDT(PredSum.spp.home)
    PredSum.spp.long <- melt(PredSum.spp, id.vars = c("index",  "GCM",    "SSP" ,   "RUN"   , "PERIOD"), 
                             variable.name = "spp",value.name = "area")
    PredSum.spp.home.long <- melt(PredSum.spp.home, id.vars = c("index",  "GCM",    "SSP" ,   "RUN"   , "PERIOD"), 
                                  variable.name = "spp",value.name = "area")
    setnames(PredSum.spp.long, c("index","gcm","ssp","run","period","spp","area"))
    setnames(PredSum.spp.home.long, c("index","gcm","ssp","run","period","spp","area"))
    PredSum.spp.long[,home := FALSE]
    PredSum.spp.home.long[,home := TRUE]
    predsum_spp <- rbind(PredSum.spp.long,PredSum.spp.home.long)
    predsum_spp[,`:=`(studyarea = studyarea, edatope = edatope)]
    dbWriteTable(dbCon, "predsum_spp",predsum_spp, row.names = FALSE, append = TRUE)
    
    ##suit
    setDT(PredSum.suit)
    setDT(PredSum.suit.home)
    PredSum.suit.long <- melt(PredSum.suit, id.vars = c("index",  "GCM",    "SSP" ,   "RUN"   , "PERIOD"), 
                              variable.name = "spp",value.name = "area")
    PredSum.suit.home.long <- melt(PredSum.suit.home, id.vars = c("index",  "GCM",    "SSP" ,   "RUN"   , "PERIOD"), 
                                   variable.name = "spp",value.name = "area")
    setnames(PredSum.suit.long, c("index","gcm","ssp","run","period","spp","area"))
    setnames(PredSum.suit.home.long, c("index","gcm","ssp","run","period","spp","area"))
    PredSum.suit.long[,home := FALSE]
    PredSum.suit.home.long[,home := TRUE]
    predsum_suit <- rbind(PredSum.suit.long,PredSum.suit.home.long)
    predsum_suit[,`:=`(studyarea = studyarea, edatope = edatope)]
    dbWriteTable(dbCon, "predsum_suit",predsum_suit, row.names = FALSE, append = TRUE)
    
    print(edatope)
  }
  message("Done",studyarea)
}
