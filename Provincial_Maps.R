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

##some setup
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", 
                 host = "138.197.168.220",
                 password = "PowerOfBEC", port = 5432, 
                 dbname = "cciss")
X <- raster("BC_Raster.tif")
X <- raster::setValues(X,NA)
outline <- st_read(con,query = "select * from bc_outline")

##code to check that none have the same predictions

allSites <- dbGetQuery(con,"select distinct rast_id from pts2km_future")
selectSites <- sample(allSites$rast_id, size = 500, replace = F)
dat <- dbGetQuery(con,paste0("select * from pts2km_future where rast_id IN (",
                             paste(selectSites,collapse = ","),") and futureperiod = '2041-2060' and scenario = 'ssp245'"))
setDT(dat)
dat <- dcast(dat,rast_id ~ gcm,value.var = "bgc_pred", fun.aggregate = function(x)x[1])
mods <- names(dat)[-1]
dat[,rast_id := NULL]

for(i in 1:(length(mods)-1)){
  for(j in (i+1):length(mods)){
    if(all(dat[,..i] == dat[,..j])){
      cat("Predictions", mods[i],"and",mods[j], "are identical!")
    }
    cat("Models:",mods[i],mods[j],"\n")
    temp <- dat[,..i] == dat[,..j]
    print(table(temp))
  }
}
fwrite(dat, "./GCM_BEC_agreement.csv")

##check for weird suitability values in the same edatopic position
edaPos <- "C4"
suit <- S1
suit <- suit[,.(BGC,SS_NoSpace,Spp,Feasible)]
suit <- unique(suit)
suit <- na.omit(suit)
edaSub <- E1[Edatopic == edaPos,.(SS_NoSpace,SpecialCode)]
dat <- suit[edaSub,on = "SS_NoSpace"]
setorder(dat,Spp,SS_NoSpace)
dat2 <- dat[,.(NumSites = .N, Range = max(Feasible) - min(Feasible), Avg = mean(Feasible)),
            by = .(Spp,BGC)]

dat[,SS_NoSpace := paste0(SS_NoSpace,": ",Feasible)]
dat[,SSNum := seq_along(SS_NoSpace), by = .(Spp,BGC)]
dat <- dcast(dat, BGC + Spp ~ SSNum, value.var = "SS_NoSpace")
setnames(dat,c("BGC","Spp","SS1","SS2","SS3","SS4","SS5"))

datAll <- dat2[dat, on = c("BGC","Spp")]
fwrite(dat2,"FeasibilityStatsC4.csv")


##########################################################

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

X[dat$rast_id] <- dat$bgcID
X2 <- ratify(X)
rat <- as.data.table(levels(X2)[[1]])
rat[dat,`:=`(bgc = i.bgc_pred, col = i.Col), on = c(ID = "bgcID")]
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
ccissMap <- function(SSPred,suit,spp_select){
  ### generate raw feasibility ratios
  
  suit <- suit[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
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
  suitVotes[Curr > 3.5, Curr := 4]
  colNms <- c("1","2","3","X")
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitRes <- suitVotes[,.(Curr = mean(Curr),NewSuit = mean(NewSuit)), by = .(SiteRef)]
  return(suitRes)
}

##figure 3c (mean change in feasibiltiy)
library(RColorBrewer)
breakpoints <- seq(-3,3,0.5); length(breakpoints)
labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey80", brewer.pal(11,"RdBu")[c(7,8,8,9,10,11)]); length(ColScheme)

timeperiods <- "2041-2060"
bgc <- dbGetCCISS_4km(con,timeperiods,all_weight) ##takes about 1.5 mins
edaPos <- "C4"
edaZonal <- E1[Edatopic == edaPos,]
##edatopic overlap
SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase) ##takes about 30 seconds
#SSPreds <- SSPreds[grep("01$|h$|00$",SS_NoSpace),] ##note that all below plots are reusing this SSPreds data

for(spp in c("Cw","Fd","Sx","Pl", "Yc")){ ##ignore warnings
  cat("Plotting ",spp,"\n")
  newFeas <- ccissMap(SSPreds,S1,spp) ##~ 15 seconds
  newFeas[NewSuit > 3.49, NewSuit := 4]
  newFeas[,FeasChange := Curr - NewSuit]
  newFeas <- unique(newFeas, by = "SiteRef")
  newFeas[,SiteRef := as.integer(SiteRef)]
  newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]
  feasVals <- newFeas[,.(SiteRef,FeasChange)]
  X <- raster::setValues(X,NA)
  X[feasVals$SiteRef] <- feasVals$FeasChange
  png(file=paste("./FeasibilityMaps/MeanChange",timeperiods,spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
  ##pdf(file=paste("./FeasibilityMaps/MeanChange",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
  image(X,xlab = NA,ylab = NA, xaxt="n", yaxt="n", col=ColScheme, 
        breaks=breakpoints, maxpixels= ncell(X),
        main = paste0(T1[TreeCode == spp,EnglishName]," (",spp,")\nSite Type: ",edaPos))
  plot(outline, add=T, border="black",col = NA, lwd=0.4)
  
  par(xpd=T)
  xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000
  rect(xl,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
  text(rep(xr-10000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
  text(xl-30000, mean(c(yb,yt))-30000, paste("Mean change\nin feasibility (", "2050s", ")", sep=""), srt=90, pos=3, cex=0.9, font=2)
  dev.off()
}

##add/retreat (figure 3b)
add_retreat <- function(SSPred,suit,spp_select){
  suit <- suit[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  #suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suit2 <- suit[,.(SS_NoSpace,Feasible)]
  setnames(suit2, old = "Feasible",new = "OrigFeas")
  suitMerge <- suit2[suitMerge, on = "SS_NoSpace"]
  suitMerge[OrigFeas > 3.5, OrigFeas := NA]
  suitMerge[Feasible > 3.5, Feasible := NA]
  suitMerge[,HasValue := if(any(!is.na(OrigFeas))|any(!is.na(Feasible))) T else F, by = .(SiteRef)]
  suitMerge <- suitMerge[(HasValue),]
  suitMerge <- suitMerge[,.(SiteRef,FuturePeriod,SS_NoSpace,OrigFeas,SS.pred,Feasible,SSprob)]
  setnames(suitMerge, old = "Feasible", new = "PredFeas")
  suitMerge[,Flag := NA_character_]
  suitMerge[is.na(OrigFeas) & !is.na(PredFeas),Flag := "Expand"]
  suitMerge[!is.na(OrigFeas) & is.na(PredFeas),Flag := "Retreat"]
  suitMerge[!is.na(OrigFeas) & !is.na(PredFeas),Flag := "Same"]
  suitMerge[is.na(OrigFeas) & is.na(PredFeas),Flag := "Same"]
  suitMerge[,PropMod := sum(SSprob), by = .(SiteRef,Flag)]
  suitMerge[,PropAll := sum(SSprob), by = .(SiteRef)]
  suitMerge[,PropMod := PropMod/PropAll]
  suitRes <- unique(suitMerge[,.(SiteRef,Flag,PropMod)])
  suitRes[,SiteRef := as.integer(SiteRef)]
  setkey(suitRes,SiteRef)
  suitRes[Flag == "Retreat",PropMod := PropMod * -1]
}

breakpoints <- seq(-1,1,0.2); length(breakpoints)
labels <- c("Retreat", "Expansion")
ColScheme <- c(brewer.pal(11,"RdBu")[c(1:4)], "grey90", brewer.pal(11,"RdBu")[c(7:11)]); length(ColScheme)


for(spp in c("Cw","Fd","Sx","Pl", "Yc")){ ##ignore warnings
  cat("Plotting ",spp,"\n")
  addret <- add_retreat(SSPreds,S1,spp) ##~ 15 seconds
  addret[Flag == "Same",PropMod := 0]
  addret <- addret[addret[, .I[which.max(abs(PropMod))], by= .(SiteRef)]$V1]
  X <- raster::setValues(X,NA)
  X[addret$SiteRef] <- addret$PropMod
  png(file=paste("./FeasibilityMaps/Add_Retreat",timeperiods,spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
  #pdf(file=paste("./FeasibilityMaps/Add_Retreat",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
  image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n", 
        col=ColScheme, breaks=breakpoints, maxpixels= ncell(X),
        main = paste0(T1[TreeCode == spp,EnglishName]," (",spp,")\nSite Type: ",edaPos))
  plot(outline, add=T, border="black",col = NA, lwd=0.4)
  
  par(xpd=T)
  
  #xl <- 325000; yb <- 900000; xr <- 425000; yt <- 1525000
  xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000
  rect(xl,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
  text(rep(xr+10000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(3,9)],labels,pos=4,cex=0.9,font=0.8, srt=90)
  text(rep(xr-20000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(1,8,15)],c("100%", "0%", "100%"),pos=4,cex=0.8,font=1)
  text(xl-30000, mean(c(yb,yt))-30000, paste("Change to feasible/unfeasible\n(", timeperiods, ") % of GCMs", sep=""), srt=90, pos=3, cex=0.9, font=2)
  dev.off()
}

################### straight predicted feasibility maps #####################
feasCols <- data.table(Feas = c(1,2,3,4,5),Col = c("limegreen", "deepskyblue", "gold", "grey","grey"))
X <- raster("BC_Raster.tif")
outline <- st_read(con,query = "select * from bc_outline")
##loop through species
for(spp in c("Cw","Fd","Sx","Pl")){
  sppFeas <- ccissMap(SSPreds,S1,spp) ##~ 15 seconds
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
