## spatial climates
##cciss feasibility
##script to process 4km subsampled data and create feasibility ratings

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
source("./_functions/_dbGetCCISS_4km.R")
source("./_functions/_meanFeasibilityMap.R")
source("./_functions/_add_retreat_map.R")
source("./_functions/_BlobOverlap.R")

##some setup
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", 
                 host = "138.197.168.220",
                 password = "PowerOfBEC", port = 5432, 
                 dbname = "cciss")
X <- raster("BC_Raster.tif")
X <- raster::setValues(X,NA)
outline <- st_read(con,query = "select * from bc_outline")


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



##figure 3c (mean change in feasibility by edatopic position)
library(RColorBrewer)
breakpoints <- seq(-3,3,0.5); length(breakpoints)
labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey50", brewer.pal(11,"RdBu")[c(7,8,8,9,10,11)]); length(ColScheme)

timeperiods <- "2041-2060"
edaPos <- "C4"
edaZonal <- E1[Edatopic == edaPos,]

bgc <- dbGetCCISS_4km(con,timeperiods,all_weight) ##takes about 1.5 mins
##edatopic overlap
SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase) ##takes about 30 seconds
#SSPreds <- SSPreds[grep("01$|h$|00$",SS_NoSpace),] ##note that all below plots are reusing this SSPreds data

for(spp in c("Cw","Yc", "Oa", "Yp")){ ##ignore warnings"Fd","Sx","Pl", ,"Oa", "Yp"
  cat("Plotting ",spp,"\n")
  newFeas <- meanFeasibilityMap(SSPreds,S1,spp) ##~ 15 seconds
  newFeas[NewSuit > 3.49, NewSuit := 4]
  newFeas[,FeasChange := Curr - NewSuit]
  newFeas <- unique(newFeas, by = "SiteRef")
  newFeas[,SiteRef := as.integer(SiteRef)]
  ##newFeas <- newFeas[Curr %in% c(1,2,3),] ##uncomment this line to only show where currently feasible
  newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]
  feasVals <- newFeas[,.(SiteRef,FeasChange)]
  X <- raster::setValues(X,NA)
  X[feasVals$SiteRef] <- feasVals$FeasChange
  png(file=paste("./FeasibilityMaps/MeanChange",timeperiods,spp,edaPos,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
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

####Range expansion and retreat maps (continure from run above)
breakpoints <- seq(-1,1,0.2); length(breakpoints)
labels <- c("Retreat", "Expansion")
ColScheme <- c(brewer.pal(11,"RdBu")[c(1:4)], "grey50", brewer.pal(11,"RdBu")[c(7:11)]); length(ColScheme)


for(spp in c("Cw","Yc", "Oa", "Yp")){ ##ignore warnings"Fd","Sx","Pl",
  cat("Plotting ",spp,"\n")
  addret <- add_retreat_map(SSPreds,S1,spp) ##~ 15 seconds
  addret[Flag == "Same",PropMod := 0]
  addret <- addret[addret[, .I[which.max(abs(PropMod))], by= .(SiteRef)]$V1]
  X <- raster::setValues(X,NA)
  X[addret$SiteRef] <- addret$PropMod
  png(file=paste("./FeasibilityMaps/Add_Retreat",timeperiods,spp,edaPos,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
  #pdf(file=paste("./FeasibilityMaps/Add_Retreat",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
  image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n", 
        col=ColScheme, breaks=breakpoints, maxpixels= ncell(X),
        main = paste0(T1[TreeCode == spp,EnglishName]," (",spp,")\nSite Type:" ,edaPos, "\nTime Period:", timeperiods))
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



##show driest edatopic maps

timeperiods <- "2041-2060"
spp <- "Cw"
feas_cutoff <- 3

feas_cutoff <- feas_cutoff+0.5

bgc <- dbGetCCISS_4km(con,timeperiods,all_weight) ##takes about 1.5 mins
edaBlobs <- fread("EdaBlobs.csv")


blobOut <- blobOverlap(bgc,edaBlobs,E1,E1_Phase,S1,spp) ##takes ~ 30 seconds
##average by bgc
blobBGC <- blobOut[,.(Current = mean(CurrentFeas), Future = mean(FutureFeas)),
                   by = .(BGC,Blob)]
##raster version first
setorder(blobOut,SiteRef,BGC,Blob)
blobOut[,SMR := substr(Blob,1,1)]
blobOut <- blobOut[,.(Current = min(CurrentFeas), Future = min(FutureFeas)),
                   by = .(SiteRef,BGC,SMR)]
blobFut <- blobOut[Future <= feas_cutoff, .(SiteRef,BGC, SMR, Current)]
blobFut[,SMR := as.numeric(SMR)]
blobFut <- blobFut[,.(MinSMR = min(SMR)), by = .(SiteRef)]
blobFut[,SiteRef := as.integer(SiteRef)]

X <- raster::setValues(X,NA)
X[blobFut$SiteRef] <- blobFut$MinSMR
breakpoints <- c(0,1.9,3.9,5.9,7.9,10)
ColScheme <- c("#c70808","#cc5200","#ebc81a","#069414","#0013e0")
png(file=paste("./FeasibilityMaps/BlobSuit_Raster_E",as.integer(feas_cutoff),"Cutoff",timeperiods,spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
##pdf(file=paste("./FeasibilityMaps/MeanChange",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
image(X,xlab = NA,ylab = NA, xaxt="n", yaxt="n", col=ColScheme,
      breaks=breakpoints, maxpixels= ncell(X),
      main = paste0("Driest Feasible SMR"," (",spp,")"))
plot(outline, add=T, border="black",col = NA, lwd=0.4)
dev.off()

##now current BGC
setorder(blobBGC,BGC,Blob)
blobBGC[,SMR := substr(Blob,1,1)]
blobBGC <- blobBGC[,.(Current = min(Current), Future = min(Future)),
                   by = .(BGC,SMR)]

blobCurr <- blobBGC[Current <= feas_cutoff, .(BGC, SMR, Current)]
blobCurr[,SMR := as.numeric(SMR)]
blobCurr <- blobCurr[,.(MinSMR = min(SMR)), by = .(BGC)]

blobFut <- blobBGC[Future <= feas_cutoff, .(BGC, SMR, Future)]
blobFut[,SMR := as.numeric(SMR)]
blobFut <- blobFut[,.(MinSMR = min(SMR)), by = .(BGC)]

edaCols <- data.table(SMR = c(0,2,4,6,8),Col = c("#f71302","#695027","#ebc81a","#069414","#0013e0"))#"#c70808"
require(ggplot2)
colScale <- scale_fill_manual(name = "Driest Feasible rSMR", 
                              values = c("#f71302" = "#f71302","#695027" = "#695027",
                                         "#ebc81a" = "#ebc81a","#069414" = "#069414","#0013e0" = "#0013e0"), 
                              labels = c("0","1-2","3-4","5-6","7"))

blobCurr[edaCols, Col := i.Col, on = c(MinSMR = "SMR")]
#bgcMap_full <- st_read("~/CommonTables/BC_BGCv12_Published_clipped.gpkg")
bgcMap_full <- st_read("D:/CommonTables/BGC_maps/BC_BGCv12_Published_clipped.gpkg")
bgcMap <- as.data.table(bgcMap_full["BGC"])
bgcMap[blobCurr, Col := i.Col, on = "BGC"]
bgcMap <- bgcMap[!is.na(Col),]
bgcMap <- st_as_sf(bgcMap)


png(file=paste("./FeasibilityMaps/EdaByBGC_Current",spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)#
plot(bgcMap["BGC"],col = bgcMap$Col,lty = 0,main = paste0("Edatopic Feasibility for ",spp," (Current)"))
plot(outline, col = NA, lwd=0.4, add = T)
legend(x = "bottomleft",
       legend = labels,
       fill = ColScheme,
       title = "Driest Feasible rSMR")

png(file=paste("./FeasibilityMaps/EdaByBGC_Current",spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
ggplot(bgcMap) +
  geom_sf(aes(fill = Col), col = NA)+
  colScale +
  geom_sf(data = outline, fill = NA, col = "black")
# + theme_bw() + ##uncomment to have blank background
# theme(panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank(),
#       panel.background = element_blank(),
#       axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.y=element_blank(),
#       axis.ticks.y=element_blank()) 

dev.off()

##future bgc
blobFut[edaCols, Col := i.Col, on = c(MinSMR = "SMR")]
bgcMap <- as.data.table(bgcMap_full["BGC"])
bgcMap[blobFut, Col := i.Col, on = "BGC"]
bgcMap <- bgcMap[!is.na(Col),]
bgcMap <- st_as_sf(bgcMap)

bgcMap$Col <- as.factor(bgcMap$Col)
png(file=paste("./FeasibilityMaps/EdaByBGC",timeperiods,spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
ggplot(bgcMap) +
  geom_sf(aes(fill = Col), col = NA)+
  colScale +
  geom_sf(data = outline, fill = NA, col = "black")
  # + theme_bw() + ##uncomment to have blank background
  # theme(panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank()) 
dev.off()

# labels <- c("0","1-2","3-4","5-6","7")
# ColScheme <- c("#c70808","#cc5200","#ebc81a","#069414","#0013e0")
# png(file=paste("./FeasibilityMaps/EdaByBGC_",timeperiods,spp,".png",sep = "_"), type="cairo", units="in", width=6.5, height=7, pointsize=10, res=800)
# plot(bgcMap["BGC"],col = bgcMap$Col,lty = 0, main = paste0("Edatopic Feasibility for ",spp," (",timeperiods,")"))
# plot(outline, col = NA, lwd=0.4, add = T)
# legend(x = "bottomleft",
#        legend = labels,
#        fill = ColScheme,
#        title = "Driest Feasible rSMR")
# dev.off()
# 
# ################### straight predicted feasibility maps #####################
# feasCols <- data.table(Feas = c(1,2,3,4,5),Col = c("limegreen", "deepskyblue", "gold", "grey","grey"))
# X <- raster("BC_Raster.tif")
# outline <- st_read(con,query = "select * from bc_outline")
# ##loop through species
# for(spp in c("Cw","Fd","Sx","Pl")){
#   sppFeas <- ccissMap(SSPreds,S1,spp) ##~ 15 seconds
#   sppFeas <- unique(sppFeas,by = "SiteRef")
#   sppFeas[,SiteRef := as.integer(SiteRef)]
#   sppFeas <- sppFeas[,.(SiteRef,NewSuit)]
#   X <- raster::setValues(X,NA)
#   X[sppFeas$SiteRef] <- sppFeas$NewSuit
#   X2 <- ratify(X)
#   rat <- as.data.table(levels(X2)[[1]])
#   rat[feasCols,`:=`(col = i.Col), on = c(ID = "Feas")]
#   
#   pdf(file=paste("./FeasibilityMaps/Feasibility",timeperiods,spp,".pdf",sep = "_"), width=6.5, height=7, pointsize=10)
#   plot(X2,col = rat$col,legend = FALSE,axes = FALSE, box = FALSE, main = paste0(spp," (",timeperiods,")"))
#   plot(outline, col = NA, add = T)
#   dev.off()
# }
#######################

##code to check that none have the same predictions####################

# allSites <- dbGetQuery(con,"select distinct rast_id from pts2km_future")
# selectSites <- sample(allSites$rast_id, size = 500, replace = F)
# dat <- dbGetQuery(con,paste0("select * from pts2km_future where rast_id IN (",
#                              paste(selectSites,collapse = ","),") and futureperiod = '2041-2060' and scenario = 'ssp245'"))
# setDT(dat)
# dat <- dcast(dat,rast_id ~ gcm,value.var = "bgc_pred", fun.aggregate = function(x)x[1])
# mods <- names(dat)[-1]
# dat[,rast_id := NULL]
# 
# for(i in 1:(length(mods)-1)){
#   for(j in (i+1):length(mods)){
#     if(all(dat[,..i] == dat[,..j])){
#       cat("Predictions", mods[i],"and",mods[j], "are identical!")
#     }
#     cat("Models:",mods[i],mods[j],"\n")
#     temp <- dat[,..i] == dat[,..j]
#     print(table(temp))
#   }
# }
# fwrite(dat, "./GCM_BEC_agreement.csv")

##check for weird suitability values in the same edatopic position
# edaPos <- "C4"
# suit <- S1
# suit <- suit[,.(BGC,SS_NoSpace,Spp,Feasible)]
# suit <- unique(suit)
# suit <- na.omit(suit)
# edaSub <- E1[Edatopic == edaPos,.(SS_NoSpace,SpecialCode)]
# dat <- suit[edaSub,on = "SS_NoSpace"]
# setorder(dat,Spp,SS_NoSpace)
# dat2 <- dat[,.(NumSites = .N, Range = max(Feasible) - min(Feasible), Avg = mean(Feasible)),
#             by = .(Spp,BGC)]
# 
# dat[,SS_NoSpace := paste0(SS_NoSpace,": ",Feasible)]
# dat[,SSNum := seq_along(SS_NoSpace), by = .(Spp,BGC)]
# dat <- dcast(dat, BGC + Spp ~ SSNum, value.var = "SS_NoSpace")
# setnames(dat,c("BGC","Spp","SS1","SS2","SS3","SS4","SS5"))
# 
# datAll <- dat2[dat, on = c("BGC","Spp")]
# fwrite(dat2,"FeasibilityStatsC4.csv")


##########################################################