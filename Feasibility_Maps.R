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
library(RPostgreSQL)
library(sf)
library(pool)
library(RColorBrewer)

##some setup
con <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

#lookup tables
spps.lookup <- read.csv("./data-raw/data_tables/Tree speciesand codes_2.0_25Aug2021.csv")
edatope.name <- c("Medium-Mesic", "Poor-Subxeric", "Rich-Hygric")
BGCcolors <- read.csv("data-raw/data_tables/WNAv11_Zone_Colours.csv")

# base raster
X <- raster("BC_Raster.tif")
X <- raster::setValues(X,NA)

#Feasibility tables
outline <- st_read(con,query = "select * from bc_outline")
S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

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
  suitMerge <- suit2[suitMerge, on = "SS_NoSpace",allow.cartesian =T]
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
  suitMerge[,PropMod := sum(SSprob), by = .(SiteRef,FuturePeriod, Flag)]
  suitMerge[,PropAll := sum(SSprob), by = .(SiteRef,FuturePeriod)]
  suitMerge[,PropMod := PropMod/PropAll]
  suitRes <- unique(suitMerge[,.(SiteRef,Flag,PropMod)])
  suitRes[,SiteRef := as.integer(SiteRef)]
  setkey(suitRes,SiteRef)
  suitRes[Flag == "Retreat",PropMod := PropMod * -1]
}


# ##gcm and rcp weight
# gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3",
#                                  "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
#                                  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
#                          weight = c(0,0,0,1,1,0,0,0,0,0,0,1,0))
# #weight = c(1,1,0,0,1,1,1,0,1,1,1,1,0))
# rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"),
#                          weight = c(0,1,1,0))
# 
# all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
# all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
# all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
# all_weight[,weight := wgcm*wrcp]
# modWeights <- all_weight

# choices to iterate through
spps <- c("Pl", "Sx", "Fd", "Cw","Ba", "Bl", "Bg", "Yc", "Pw", "Hm", "Lw", "Hw", "Py", "Dr", "Ep", "At")
edas <- c("C4", "B2", "D6")
timeperiods <- c(2001, 2021, 2041, 2061, 2081)
timeperiod.names <- c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

###load up bgc predictions data
timeperiod <- "2041"
for(timeperiod in timeperiods[-1]){
  bgc <- setDT(dbGetQuery(con,paste0("select * from mapdata_2km where futureperiod = '",timeperiod,"'"))) ##takes about 15 seconds
  setnames(bgc, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  # str(bgc)
  
  #BGC zones
  bgc.ref <- unique(bgc[,c(1,3)])
  bgc.ref$SiteRef <- as.numeric(bgc.ref$SiteRef)
  zones <- c("CDF", "CWH", "MH", "ESSF", "MS", "IDF", "PP", "BG", "ICH", "SBPS", "SBS", "BWBS", "SWB", "CMA", "IMA", "BAFA")
  zone <- rep(NA, dim(bgc.ref)[1])
  for(i in zones){ zone[grep(i,bgc.ref$BGC)] <- i }
  table(zone)
  zone <- factor(zone, levels=zones)
  
  # loop through edatope and species
  eda <- "C4"
  for(eda in edas){
    spp <- "Bl"
    for(spp in spps){ ##ignore warnings,"Fd","Sx","Pl", "Yc", "Yc", "Oa", "Yp"
      cat("Plotting ",spp, eda,"\n")
      
      edaTemp <- data.table::copy(E1)
      edaTemp <- edaTemp[is.na(SpecialCode),]
      
      edaTemp[,HasPos := if(any(Edatopic == eda)) T else F, by = .(SS_NoSpace)]
      edaZonal <- edaTemp[(HasPos),]
      edaZonal[,HasPos := NULL]
      ##edatopic overlap
      SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase,onlyRegular = TRUE) ##takes about 30 seconds
      
      # ## ------------------------------------
      # ## 3 panel map with add/retreat and mean feasibility change
      # 
      # #initialise plot
      # png(file=paste("./FeasibilityMaps/Three_Panel",spp,eda,timeperiod,"png",sep = "."), type="cairo", units="in", width=6.5, height=2.9, pointsize=9, res=300)
      # 
      # par(plt=c(0,1,0,1), bg="white")
      # plot(0, col="white", xaxt="n", yaxt="n", xlab="", ylab="")
      # Common <- as.character(spps.lookup$EnglishName[which(spps.lookup$TreeCode==spp)])
      # Latin <- as.character(spps.lookup$ScientificName[which(spps.lookup$TreeCode==spp)])
      # # panel <- paste("(", LETTERS[which(spps==spp)],")", sep="")
      # mtext(if(spp%in%spps.lookup$TreeCode) bquote(bold(.(spp))~"-"~.(Common)) else bquote(bold(.(spp))),
      #       side=3, line=-1.25, adj=0.01, cex=0.8, font=2)
      # # mtext(if(spp%in%spps.lookup$TreeCode) bquote(.(panel)~bold(.(spp))~"-"~.(Common)~"("*italic(.(Latin)*")")) else bquote(.(panel)~bold(.(spp))),
      # #       side=3, line=-1.75, adj=0.01, cex=0.8, font=2)
      # mtext(paste("Site type: ", eda, " (", edatope.name[which(eda==edas)], ")", sep=""), side=3, line=-2., adj=0.01, cex=0.7, font=1)
      # 
      # ##=================================
      # ###historic suitability
      # newFeas <- ccissMap(SSPreds,S1,spp)##~ 15 seconds
      # newFeas[NewSuit > 4, NewSuit := 4]
      # newFeas[,FeasChange := Curr - NewSuit]
      # newFeas <- unique(newFeas, by = "SiteRef")
      # newFeas[,SiteRef := as.integer(SiteRef)]
      # ##newFeas <- newFeas[Curr %in% c(1,2,3),] ##uncomment this line to only show where currently feasible
      # newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]
      # 
      # X <- raster::setValues(X,NA)
      # X[newFeas$SiteRef] <- newFeas$Curr
      # breakseq <- c(0.5,1.5,2.5,3.5,5)
      # ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")
      # 
      # par(plt = c(0, 0.3, 0, 0.6),new = TRUE, xpd = TRUE)
      # 
      # image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n", 
      #       col=ColScheme, breaks=breakseq, maxpixels= ncell(X),asp = 1)
      # plot(outline, add=T, border="black",col = NA, lwd=0.4)
      # legend("topleft", legend=c("1 (primary)", "2 (secondary)", "3 (tertiary)"), 
      #        fill=ColScheme, bty="n", cex=0.8, title="Historical feasibility", inset=c(0,-0.3))
      # mtext(paste("(", letters[1],")", sep=""), side=3, line=-2.75, adj=0.05, cex=0.8, font=2)
      # 
      # ##=================================
      # ##add/retreat
      # breakpoints <- seq(-1,1,0.2); length(breakpoints)
      # labels <- c("Retreat", "Expand")
      # ColScheme <- c(brewer.pal(11,"RdBu")[c(1:4)], "grey90", brewer.pal(11,"RdBu")[c(7:11)]); length(ColScheme)
      # addret <- add_retreat(SSPreds,S1,spp) ##~ 15 seconds
      # addret[Flag == "Same",PropMod := 0]
      # addret <- addret[addret[, .I[which.max(abs(PropMod))], by= .(SiteRef)]$V1]
      # X <- raster::setValues(X,NA)
      # X[addret$SiteRef] <- addret$PropMod
      # 
      # par(plt = c(0.25,0.75,0,1),xpd = TRUE, new = TRUE)
      # image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n", 
      #       col=ColScheme, breaks=breakpoints, maxpixels= ncell(X),asp = 1,
      #       main = paste0(T1[TreeCode == spp,EnglishName]," (",spp,")\nSite Type: ",eda, "\nTime Period: ",timeperiod.names[which(timeperiods==timeperiod)]))
      # plot(outline, add=T, border="black",col = NA, lwd=0.4)
      # 
      # xl <- 325000; yb <- 900000; xr <- 400000; yt <- 1525000 #xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000
      # rect(xl,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
      # text(rep(xr+10000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(3,9)],labels,pos=4,cex=0.9,font=0.8, srt=90)
      # text(rep(xr-20000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(1,8,15)],c("100%", "0%", "100%"),pos=4,cex=0.8,font=1)
      # text(xl-30000, mean(c(yb,yt))-30000, paste("Change to feasible/unfeasible\n(", timeperiod.names[which(timeperiods==timeperiod)], "); % of GCMs", sep=""), srt=90, pos=3, cex=0.85, font=2)
      # mtext(paste("(", letters[2],")", sep=""), side=3, line=-2.75, adj=0.095, cex=0.8, font=2)
      # 
      # ##=================================
      # ##mean feasibility change
      # feasVals <- newFeas[,.(SiteRef,FeasChange)]
      # X <- raster::setValues(X,NA)
      # X[feasVals$SiteRef] <- feasVals$FeasChange
      # 
      # breakpoints <- seq(-3,3,0.5); length(breakpoints)
      # labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
      # ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", brewer.pal(11,"RdBu")[c(7,8,8,9,10,11)]);
      # 
      # par(plt = c(0.6, 0.95, 0.25, 1), xpd = TRUE, new = TRUE)
      # image(X,xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme, 
      #       breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      # plot(outline, add=T, border="black",col = NA, lwd=0.4)
      # 
      # xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000
      # rect(xl,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
      # text(rep(xr-10000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
      # text(xl-30000, mean(c(yb,yt))-30000, paste("Mean change\nin feasibility (", timeperiod.names[which(timeperiods==timeperiod)], ")", sep=""), srt=90, pos=3, cex=0.85, font=2)
      # par(xpd=F)
      # mtext(paste("(", letters[3],")", sep=""), side=3, line=-3.25, adj=0.1, cex=0.8, font=2)
      # 
      # ##=================================
      # ## Summary by zone
      # 
      # # par(mar=c(0,0,0,0), plt = c(0.77, 0.995, 0.001, 0.31), new = TRUE, mgp=c(1.25,0.15,0))
      # # plot(0, xlim=c(0,1), ylim=c(0,1), col="white", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
      # FeasChange <- values(X)[bgc.ref$SiteRef]
      # FeasChange[is.na(FeasChange)] <- 0
      # par(mar=c(4.5,2,0.1,0.1), plt = c(0.79, 0.995, 0.1, 0.275), new = TRUE, mgp=c(1.25,0.15,0))
      # ylim=c(-3,3)
      # xlim=c(1, length(levels(droplevels(zone))))
      # z <- boxplot(FeasChange~zone, ylab="", vertical = TRUE, plot=F)
      # for(i in 1:length(levels(zone))){
      #   temp <- FeasChange[which(zone==levels(zone)[i])]
      #   z$stats[c(1,5), i] <- quantile(temp[!is.na(temp)],c(0.05, 0.95))
      # }
      # bxp(z, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      # lines(c(-99,99), c(0,0), lwd=2, col="darkgrey")
      # bxp(z, add=T, boxfill = as.character(BGCcolors$colour[match(levels(zone), BGCcolors$classification)]), xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      # axis(1, at=1:length(levels(zone)), levels(zone), tick=F, las=2, cex.axis=0.65)
      # axis(2,at=seq(ylim[1], ylim[2], 3), seq(ylim[1], ylim[2], 3), las=2, tck=0)
      # mtext("Mean change in feasibility", side=3, line=0.1, adj=.975, cex=0.65, font=2)
      # mtext(paste("(", letters[4],")", sep=""), side=3, line=1, adj=0.975, cex=0.8, font=2)
      # 
      #     dev.off()
      
      ## ------------------------------------------------------------
      ## 2 panel map
      #initialise plot
      thirdcolor <- "Khaki1Gold"
      png(file=paste("./FeasibilityMaps/Two_Panel",spp,eda,timeperiod,"png",sep = "."), type="cairo", units="in", width=6.5, height=5, pointsize=12, res=300)
      
      par(plt=c(0,1,0,1), bg="white")
      plot(0, col="white", xaxt="n", yaxt="n", xlab="", ylab="")
      Common <- as.character(spps.lookup$EnglishName[which(spps.lookup$TreeCode==spp)])
      Latin <- as.character(spps.lookup$ScientificName[which(spps.lookup$TreeCode==spp)])
      # panel <- paste("(", LETTERS[which(spps==spp)],")", sep="")
      mtext(if(spp%in%spps.lookup$TreeCode) bquote(bold(.(spp))~"-"~.(Common)) else bquote(bold(.(spp))),
            side=3, line=-2.5, adj=0.01, cex=0.9, font=2)
      # mtext(if(spp%in%spps.lookup$TreeCode) bquote(.(panel)~bold(.(spp))~"-"~.(Common)~"("*italic(.(Latin)*")")) else bquote(.(panel)~bold(.(spp))),
      #       side=3, line=-1.75, adj=0.01, cex=0.8, font=2)
      mtext(paste("Site type: ", eda, " (", edatope.name[which(eda==edas)], ")", sep=""), side=3, line=-3.5, adj=0.01, cex=0.8, font=1)
      mtext(paste("Time period: ", timeperiod.names[which(timeperiods==timeperiod)], sep=""), side=3, line=-4.5, adj=0.01, cex=0.8, font=1)
      
      ##=================================
      ###historic suitability
      newFeas <- ccissMap(SSPreds,S1,spp)##~ 15 seconds
      newFeas[NewSuit > 4, NewSuit := 4]
      newFeas[,FeasChange := Curr - NewSuit]
      newFeas <- unique(newFeas, by = "SiteRef")
      newFeas[,SiteRef := as.integer(SiteRef)]
      ##newFeas <- newFeas[Curr %in% c(1,2,3),] ##uncomment this line to only show where currently feasible
      newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]
      
      X <- raster::setValues(X,NA)
      X[newFeas$SiteRef] <- newFeas$Curr
      breakseq <- c(0.5,1.5,2.5,3.5,5)
      ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")
      
      par(plt = c(0, 0.5, 0.05, 0.6),new = TRUE, xpd = TRUE)
      
      image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n", 
            col=ColScheme, breaks=breakseq, maxpixels= ncell(X),asp = 1)
      plot(outline, add=T, border="black",col = NA, lwd=0.4)
      legend("topleft", legend=c("1 (primary)", "2 (secondary)", "3 (tertiary)"), 
             fill=ColScheme, bty="n", cex=0.8, title="Historical feasibility", inset=c(0,-0.3))
      # mtext(paste("(", letters[1],")", sep=""), side=3, line=-2.75, adj=0.05, cex=0.8, font=2)
      
      
      ##=================================
      ##mean feasibility change
      feasVals <- newFeas[,.(SiteRef,FeasChange)]
      X <- raster::setValues(X,NA)
      X[feasVals$SiteRef] <- feasVals$FeasChange
      X2 <- raster::setValues(X,NA)
      X2[feasVals$SiteRef[newFeas$Curr==4]] <- newFeas$FeasChange[newFeas$Curr==4]
      X3 <- raster::setValues(X,NA)
      values(X3)[feasVals$SiteRef[newFeas$Curr<4 & newFeas$NewSuit>3.5]] <- 1
      
      breakpoints <- seq(-3,3,0.5); length(breakpoints)
      labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
      ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", "grey90", brewer.pal(11,"RdBu")[c(7,8,9,10,11)]);
      # ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", c("beige", "khaki1", "khaki2", "khaki3", "khaki4", "darkolivegreen"));
      # ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("beige", "yellow", "black"))(6));
      # ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("beige", "khaki1", "yellow"))(6));
      # ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("grey95", "beige", "khaki1", "yellow2", "gold"))(6));
      ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("grey90", "khaki1", "gold"))(6));
      ColScheme3 <- 1
      
      par(plt = c(0.25, 0.95, 0.175, 1), xpd = TRUE, new = TRUE)
      image(X,xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme, breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      image(X2, add=T, xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme2, breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      image(X3, add=T, xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme3, maxpixels= ncell(X), asp = 1)
      plot(outline, add=T, border="black",col = NA, lwd=0.4)
      
      xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000; xadj <- 10000
      y.int <- (yt-yb)/length(ColScheme)
      rect(xl+xadj,  head(seq(yb,yt,y.int),-1),  xr,  tail(seq(yb,yt,y.int),-1),  col=ColScheme)
      rect(xl-diff(c(xl+xadj, xr)),  head(seq(yb,yt,y.int),-1),  xl-xadj,  tail(seq(yb,yt,y.int),-1),  col=ColScheme2)
      rect(xl-diff(c(xl+xadj, xr)),  yb,  xl-xadj,  (yb+yt)/2,  col="white")
      text(xl-diff(c(xl+xadj, xr))/2, yb+(yt-yb)/4, "Expansion", srt=90, cex=0.85, font=1)
      text(rep(xr-10000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
      text(xl-diff(c(xl+xadj, xr))-30000, mean(c(yb,yt))-30000, paste("Mean change in feasibility", sep=""), srt=90, pos=3, cex=0.85, font=2)
      rect(xl+xadj,  yb-y.int-20000,  xr,  yb-20000,  col="black")
      text(xr, yb-y.int/2-30000, "Loss", pos=4, cex=0.8, font=1)
      par(xpd=F)
      # mtext(paste("(", letters[3],")", sep=""), side=3, line=-3.25, adj=0.1, cex=0.8, font=2)
      
      ##=================================
      ## Summary by zone
      
      # par(mar=c(0,0,0,0), plt = c(0.77, 0.995, 0.001, 0.31), new = TRUE, mgp=c(1.25,0.15,0))
      # plot(0, xlim=c(0,1), ylim=c(0,1), col="white", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
      FeasChange <- values(X)[bgc.ref$SiteRef]
      FeasChange[is.na(FeasChange)] <- 0
      par(mar=c(4.5,2,0.1,0.1), plt = c(0.7, 0.995, 0.08, 0.2), new = TRUE, mgp=c(1.25,0.15,0))
      ylim=c(-3,3)
      xlim=c(1, length(levels(droplevels(zone))))
      z <- boxplot(FeasChange~zone, ylab="", vertical = TRUE, plot=F)
      for(i in 1:length(levels(zone))){
        temp <- FeasChange[which(zone==levels(zone)[i])]
        z$stats[c(1,5), i] <- quantile(temp[!is.na(temp)],c(0.05, 0.95))
      }
      bxp(z, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      lines(c(-99,99), c(0,0), lwd=2, col="darkgrey")
      bxp(z, add=T, boxfill = as.character(BGCcolors$colour[match(levels(zone), BGCcolors$classification)]), xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      axis(1, at=1:length(levels(zone)), levels(zone), tick=F, las=2, cex.axis=0.65)
      axis(2,at=seq(ylim[1], ylim[2], 3), seq(ylim[1], ylim[2], 3), las=2, tck=0)
      mtext("Mean change in feasibility", side=3, line=0.1, adj=.975, cex=0.65, font=2)
      # mtext(paste("(", letters[4],")", sep=""), side=3, line=1, adj=0.975, cex=0.8, font=2)
      
      dev.off()

      ## ------------------------------------
      ## 3 panel map with mean feasibility change and model agreement

      #initialise plot
      png(file=paste("./FeasibilityMaps/Three_Panel",spp,eda,timeperiod,"png",sep = "."), type="cairo", units="in", width=6.5, height=2.9, pointsize=9, res=300)

      par(plt=c(0,1,0,1), bg="white")
      plot(0, col="white", xaxt="n", yaxt="n", xlab="", ylab="")
      Common <- as.character(spps.lookup$EnglishName[which(spps.lookup$TreeCode==spp)])
      Latin <- as.character(spps.lookup$ScientificName[which(spps.lookup$TreeCode==spp)])
      # panel <- paste("(", LETTERS[which(spps==spp)],")", sep="")
      mtext(if(spp%in%spps.lookup$TreeCode) bquote(bold(.(spp))~"-"~.(Common)) else bquote(bold(.(spp))),
            side=3, line=-1.25, adj=0.01, cex=0.8, font=2)
      # mtext(if(spp%in%spps.lookup$TreeCode) bquote(.(panel)~bold(.(spp))~"-"~.(Common)~"("*italic(.(Latin)*")")) else bquote(.(panel)~bold(.(spp))),
      #       side=3, line=-1.75, adj=0.01, cex=0.8, font=2)
      mtext(paste("Site type: ", eda, " (", edatope.name[which(eda==edas)], ")", sep=""), side=3, line=-2., adj=0.01, cex=0.7, font=1)

      ##=================================
      ###historic suitability
      newFeas <- ccissMap(SSPreds,S1,spp)##~ 15 seconds
      newFeas[NewSuit > 4, NewSuit := 4]
      newFeas[,FeasChange := Curr - NewSuit]
      newFeas <- unique(newFeas, by = "SiteRef")
      newFeas[,SiteRef := as.integer(SiteRef)]
      ##newFeas <- newFeas[Curr %in% c(1,2,3),] ##uncomment this line to only show where currently feasible
      newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]

      X <- raster::setValues(X,NA)
      X[newFeas$SiteRef] <- newFeas$Curr
      breakseq <- c(0.5,1.5,2.5,3.5,5)
      ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")

      par(plt = c(0, 0.3, 0, 0.6),new = TRUE, xpd = TRUE)

      image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n",
            col=ColScheme, breaks=breakseq, maxpixels= ncell(X),asp = 1)
      plot(outline, add=T, border="black",col = NA, lwd=0.4)
      legend("topleft", legend=c("1 (primary)", "2 (secondary)", "3 (tertiary)"),
             fill=ColScheme, bty="n", cex=0.8, title="Historical feasibility", inset=c(0,-0.3))
      mtext(paste("(", letters[1],")", sep=""), side=3, line=-2.75, adj=0.05, cex=0.8, font=2)

      ##=================================
      ##mean feasibility change
      feasVals <- newFeas[,.(SiteRef,FeasChange)]
      X <- raster::setValues(X,NA)
      X[feasVals$SiteRef] <- feasVals$FeasChange
      X2 <- raster::setValues(X,NA)
      X2[feasVals$SiteRef[newFeas$Curr==4]] <- newFeas$FeasChange[newFeas$Curr==4]
      X3 <- raster::setValues(X,NA)
      values(X3)[feasVals$SiteRef[newFeas$Curr<4 & newFeas$NewSuit>3.5]] <- 1
      
      breakpoints <- seq(-3,3,0.5); length(breakpoints)
      labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
      ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", "grey90", brewer.pal(11,"RdBu")[c(7,8,9,10,11)]);
      ColScheme2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("grey90", "khaki1", "gold"))(6));
      ColScheme3 <- 1
      
      par(plt = c(0.25,0.75,0,1),xpd = TRUE, new = TRUE)
      image(X,xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme, breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      image(X2, add=T, xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme2, breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      image(X3, add=T, xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme3, maxpixels= ncell(X), asp = 1)
      plot(outline, add=T, border="black",col = NA, lwd=0.4)

      xl <- 325000; yb <- 900000; xr <- 400000; yt <- 1525000; xadj <- 10000
      rect(xl+xadj,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
      rect(xl-diff(c(xl+xadj, xr)),  head(seq(yb,yt,(yt-yb)/length(ColScheme2)),-1),  xl-xadj,  tail(seq(yb,yt,(yt-yb)/length(ColScheme2)),-1),  col=ColScheme2)
      rect(xl-diff(c(xl+xadj, xr)),  yb,  xl-xadj,  (yb+yt)/2,  col="white")
      text(xl-diff(c(xl+xadj, xr))/2, yb+(yt-yb)/4, "Expansion", srt=90, cex=0.85, font=1)
      text(rep(xr-10000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
      text(xl-diff(c(xl+xadj, xr))-30000, mean(c(yb,yt))-30000, paste("Mean change in feasibility", sep=""), srt=90, pos=3, cex=0.85, font=2)
      mtext(paste("(", letters[2],")", sep=""), side=3, line=-2.75, adj=0.095, cex=0.8, font=2)

      ##=================================
      ## Model Agreement (NOT MODIFIED YET)
      feasVals <- newFeas[,.(SiteRef,FeasChange)]
      X <- raster::setValues(X,NA)
      X[feasVals$SiteRef] <- feasVals$FeasChange

      breakpoints <- seq(-3,3,0.5); length(breakpoints)
      labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
      ColScheme <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", brewer.pal(11,"RdBu")[c(7,8,8,9,10,11)]);

      par(plt = c(0.6, 0.95, 0.25, 1), xpd = TRUE, new = TRUE)
      image(X,xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme,
            breaks=breakpoints, maxpixels= ncell(X), asp = 1)
      plot(outline, add=T, border="black",col = NA, lwd=0.4)

      xl <- 1600000; yb <- 1000000; xr <- 1700000; yt <- 1700000
      rect(xl,  head(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  xr,  tail(seq(yb,yt,(yt-yb)/length(ColScheme)),-1),  col=ColScheme)
      text(rep(xr-10000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
      text(xl-30000, mean(c(yb,yt))-30000, paste("Mean change\nin feasibility (", timeperiod.names[which(timeperiods==timeperiod)], ")", sep=""), srt=90, pos=3, cex=0.85, font=2)
      par(xpd=F)
      mtext(paste("(", letters[3],")", sep=""), side=3, line=-3.25, adj=0.1, cex=0.8, font=2)

      ##=================================
      ## Summary by zone

      # par(mar=c(0,0,0,0), plt = c(0.77, 0.995, 0.001, 0.31), new = TRUE, mgp=c(1.25,0.15,0))
      # plot(0, xlim=c(0,1), ylim=c(0,1), col="white", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
      FeasChange <- values(X)[bgc.ref$SiteRef]
      FeasChange[is.na(FeasChange)] <- 0
      par(mar=c(4.5,2,0.1,0.1), plt = c(0.79, 0.995, 0.1, 0.275), new = TRUE, mgp=c(1.25,0.15,0))
      ylim=c(-3,3)
      xlim=c(1, length(levels(droplevels(zone))))
      z <- boxplot(FeasChange~zone, ylab="", vertical = TRUE, plot=F)
      for(i in 1:length(levels(zone))){
        temp <- FeasChange[which(zone==levels(zone)[i])]
        z$stats[c(1,5), i] <- quantile(temp[!is.na(temp)],c(0.05, 0.95))
      }
      bxp(z, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      lines(c(-99,99), c(0,0), lwd=2, col="darkgrey")
      bxp(z, add=T, boxfill = as.character(BGCcolors$colour[match(levels(zone), BGCcolors$classification)]), xaxt="n", yaxt="n", xaxs="i", ylab="", pch=0,outline=FALSE)
      axis(1, at=1:length(levels(zone)), levels(zone), tick=F, las=2, cex.axis=0.65)
      axis(2,at=seq(ylim[1], ylim[2], 3), seq(ylim[1], ylim[2], 3), las=2, tck=0)
      mtext("Mean change in feasibility", side=3, line=0.1, adj=.975, cex=0.65, font=2)
      mtext(paste("(", letters[4],")", sep=""), side=3, line=1, adj=0.975, cex=0.8, font=2)

          dev.off()
      
            print(spp)
    }
    print(eda)
  }
  print(timeperiod)
}

### -------------------------------------------------------
### -------------------------------------------------------
### export rasters of mean feasibility change
timeperiod <- "2041"
for(timeperiod in timeperiods){
  bgc <- setDT(dbGetQuery(con,paste0("select * from mapdata_2km where futureperiod = '",timeperiod,"'"))) ##takes about 15 seconds
  setnames(bgc, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
  # str(bgc)
  
  #BGC zones
  bgc.ref <- unique(bgc[,c(1,3)])
  bgc.ref$SiteRef <- as.numeric(bgc.ref$SiteRef)

  # loop through edatope and species
  eda <- "C4"
  for(eda in edas){
    spp <- "Bl"
    for(spp in spps){ ##ignore warnings,"Fd","Sx","Pl", "Yc", "Yc", "Oa", "Yp"
      edaTemp <- data.table::copy(E1)
      edaTemp <- edaTemp[is.na(SpecialCode),]
      
      edaTemp[,HasPos := if(any(Edatopic == eda)) T else F, by = .(SS_NoSpace)]
      edaZonal <- edaTemp[(HasPos),]
      edaZonal[,HasPos := NULL]
      ##edatopic overlap
      SSPreds <- edatopicOverlap(bgc,edaZonal,E1_Phase,onlyRegular = TRUE) ##takes about 30 seconds
      
      newFeas <- ccissMap(SSPreds,S1,spp)##~ 15 seconds
      newFeas[NewSuit > 4, NewSuit := 4]
      newFeas[,FeasChange := Curr - NewSuit]
      newFeas <- unique(newFeas, by = "SiteRef")
      newFeas[,SiteRef := as.integer(SiteRef)]
      ##newFeas <- newFeas[Curr %in% c(1,2,3),] ##uncomment this line to only show where currently feasible
      newFeas <- newFeas[!(Curr == 4 & FeasChange == 0),]
      
      # ##=================================
      # ###historic suitability
      # X <- raster::setValues(X,NA)
      # X[newFeas$SiteRef] <- newFeas$Curr
      # writeRaster(X, datatype="FLT4S", paste("./FeasibilityMaps/rasters/HistoricFeas",spp,eda,"tif",sep = "."), overwrite=T)
      
      ##=================================
      ##mean feasibility change
      feasVals <- newFeas[,.(SiteRef,FeasChange)]
      X <- raster::setValues(X,NA)
      X[feasVals$SiteRef] <- feasVals$FeasChange
      X[feasVals$SiteRef[newFeas$Curr==4]] <- newFeas$FeasChange[newFeas$Curr==4]+10
      values(X)[feasVals$SiteRef[newFeas$Curr<4 & newFeas$NewSuit>3.5]] <- -10
      X <- round(X, digits = 1)
      writeRaster(X, datatype="FLT4S", paste("./FeasibilityMaps/rasters/FeasChange",spp,eda,timeperiod,"tif",sep = "."), overwrite=T)
      
      print(spp)
    }
    print(eda)
  }
  print(timeperiod)
}

