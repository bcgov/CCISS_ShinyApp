## spatial climates
library(data.table)
library(sf)
library(RPostgreSQL)
library(dplyr)
library(foreach)
library(rmapshaper)
library(tictoc)
library(rasterVis)
library(raster)
library(ccissdev)

##connect to cciss database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", 
                 host = "138.197.168.220",
                 password = "PowerOfBEC", port = 5432, 
                 dbname = "cciss")

##projected bgc maps
##make projected bgc maps
scn <- "ssp245";fp <- "2021-2040";gcm <- "EC-Earth3" ##select options
dat <- dbGetQuery(con,paste0("select rast_id, bgc_pred from pts2km_future where gcm = '",gcm,"' and scenario = '",
                             scn,"' and futureperiod = '",fp,"'"))
setDT(dat)
bgcs <- unique(dat$bgc_pred)
bgcID <- data.table(bgc = bgcs, id = 1:length(bgcs))
cols <- subzones_colours_ref
dat[cols,Col := i.colour, on = c(bgc_pred = "classification")]
dat[bgcID,bgcID := i.id, on = c(bgc_pred = "bgc")]
X <- raster("BC_Raster.tif")

X <- raster::setValues(X,NA)
X[dat$rast_id] <- dat$bgcID
X2 <- ratify(X)
rat <- as.data.table(levels(X2)[[1]])
rat[dat,`:=`(bgc = i.bgc_pred, col = i.Col), on = c(ID = "bgcID")]
outline <- st_read(con,query = "select * from bc_outline")
pdf(file=paste0("./BGCFuturesMaps/BGC_Projections",gcm,fp,scn,".pdf"), width=6.5, height=7, pointsize=10)
plot(X2,col = rat$col,legend = FALSE,axes = FALSE, box = FALSE, main = paste0(gcm," (",fp,", ",scn,")"))
plot(outline, col = NA, add = T)
dev.off()

##cciss feasibility
##script to process 4km subsampled data and create feasibility ratings
##adjust gcm weight or rcp weight below
gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,1,0,1,1,1,1,0,1,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]
modWeights <- all_weight

##function to process data in postgres
dbGetCCISS_4km <- function(con, period = "2041-2060", modWeights){
  
  modWeights[,comb := paste0("('",gcm,"','",rcp,"',",weight,")")]
  weights <- paste(modWeights$comb,collapse = ",")
  groupby = "rast_id"
  ##cciss_future is now test_future  
  cciss_sql <- paste0("
  WITH cciss AS (
    SELECT rast_id,
           futureperiod,
           bgc,
           bgc_pred,
           w.weight
    FROM pts2km_future
    JOIN (values ",weights,") 
    AS w(gcm,scenario,weight)
    ON pts2km_future.gcm = w.gcm AND pts2km_future.scenario = w.scenario
    WHERE futureperiod IN ('",period,"')
  
  ), cciss_count_den AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           SUM(weight) w
    FROM cciss
    GROUP BY ", groupby, ", futureperiod
  
  ), cciss_count_num AS (
  
    SELECT ", groupby, " siteref,
           futureperiod,
           bgc,
           bgc_pred,
           SUM(weight) w
    FROM cciss
    GROUP BY ", groupby, ", futureperiod, bgc, bgc_pred
  
  )
  
  SELECT cast(a.siteref as text) siteref,
         a.futureperiod,
         a.bgc,
         a.bgc_pred,
         a.w/cast(b.w as float) bgc_prop
  FROM cciss_count_num a
  JOIN cciss_count_den b
    ON a.siteref = b.siteref
   AND a.futureperiod = b.futureperiod
   WHERE a.w <> 0
  ")
  
  dat <- setDT(RPostgres::dbGetQuery(con, cciss_sql))
  
  setnames(dat, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  dat <- unique(dat) ##should fix database so not necessary
  #print(dat)
  return(dat)
}

##adapted feasibility function
ccissMap <- function(SSPred,suit){
  ### generate raw feasibility ratios
  
  suit <- suit[,.(BGC,SS_NoSpace,Spp,Feasible)]
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
  suitVotes[,FuturePeriod := as.integer(FuturePeriod)]
  suitVotes[Curr > 3.5, Curr := 4]
  colNms <- c("1","2","3","X")
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,NewSuit := round(`1`+(`2`*2)+(`3`*3)+(X*5))]
  suitVotes <- suitVotes[,.(SiteRef,FuturePeriod,SS_NoSpace,Spp,Curr,NewSuit)]
  return(suitVotes)
}

##pull bgc data
timeperiods <- c("2041-2060")
bgc <- dbGetCCISS_4km(con,timeperiods,all_weight) ##takes about 1.5 mins

edaZonal <- E1[grep("01$|h$|00$",SS_NoSpace),]
##edatopic overlap
SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase) ##takes about 30 seconds
SSPreds <- SSPreds[grep("01$|h$|00$",SS_NoSpace),]
newFeas <- ccissMap(SSPreds,S1) ##~ 15 seconds
feasCols <- data.table(Feas = c(1,2,3,4,5),Col = c("limegreen", "deepskyblue", "gold", "grey","grey"))
X <- raster("BC_Raster.tif")
outline <- st_read(con,query = "select * from bc_outline")
##loop through species
for(spp in c("Cw","Fd","Sx","Pl")){
  sppFeas <- newFeas[Spp == spp,]
  sppFeas <- unique(sppFeas,by = "SiteRef")
  sppFeas[,SiteRef := as.integer(SiteRef)]
  sppFeas <- sppFeas[,.(SiteRef,NewSuit)]
  X <- raster::setValues(X,NA)
  X[sppFeas$SiteRef] <- sppFeas$NewSuit
  X2 <- ratify(X)
  rat <- as.data.table(levels(X2)[[1]])
  rat[feasCols,`:=`(col = i.Col), on = c(ID = "Feas")]
  
  pdf(file=paste("./FeasibilityMaps/Feasibility",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
  plot(X2,col = rat$col,legend = FALSE,axes = FALSE, box = FALSE, main = paste0(spp," (",timeperiods,")"))
  plot(outline, col = NA, add = T)
  dev.off()
}


