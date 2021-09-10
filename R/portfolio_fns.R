### Portfolio functions
### Kiri Daust

#' Clean Portfolio Data

#' @details What the function does
#' @return What the function returns
#' @import data.table
#' @import foreach
#' @export
cleanData <- function(SSPredAll,SIBEC,SuitTable,SNum,Trees,timePer,selectBGC){
  SSPred <- SSPredAll[SiteNo == SNum,] ###subset
  
  ##Merge SIBEC data
  SIBEC <- SIBEC[TreeSpp %in% Trees,]
  SSPred <- SSPred[SSPred$FuturePeriod %in% timePer,]
  SSPred <- SSPred[,.(FuturePeriod, SS_NoSpace,SS.pred, SSprob)]
  SSPred <- SIBEC[SSPred, on = c(SS_NoSpace = "SS.pred")]
  setnames(SSPred, old = c("SS_NoSpace","i.SS_NoSpace"), new = c("SS.pred","SS_NoSpace"))
  
  ###Add rows for species with missing SI - mostly US units here
  add <- foreach(Year = unique(SSPred$FuturePeriod), .combine = rbind) %do%{
    byYear <- SSPred[SSPred$FuturePeriod == Year,]
    foreach(SS = unique(byYear$SS.pred), .combine = rbind) %do%{
      bySS <- byYear[byYear$SS.pred == SS,]
      missing <- Trees[!Trees %in% bySS$TreeSpp]
      new <- bySS[rep(1,length(missing)),]
      new$TreeSpp <- missing
      new
    }
  }
  if(!is.null(add)){
    if(nrow(add) > 0){
      add$MeanPlotSiteIndex <- 5 ##Set missing SI
      SSPred <- rbind(SSPred, add)
    }
  } 
  
  
  SSPred <- SSPred[!is.na(SSPred$TreeSpp),]
  setnames(SSPred,old = "TreeSpp", new = "Spp")
  
  ##Add suitability
  SSPred[SuitTable, Suitability := i.Suitability, on = c(SS.pred = "SS_NoSpace", "Spp")]
  SSPred$Suitability[is.na(SSPred$Suitability)] <- 5
  
  temp <- SIBEC[SS_NoSpace == selectBGC,]
  if(nrow(temp) == 0){
    return(NULL)
  }
  
  ##Summarise data- average SI and Suit weighted by SSProb
  SS.sum <- SSPred[,.(MeanSI = sum(MeanPlotSiteIndex*(SSprob/sum(SSprob))),
                      MeanSuit = round(sum(Suitability*(SSprob/sum(SSprob))), digits = 0)),
                   by = .(Spp,FuturePeriod)]
  
  SS.sum <- SS.sum[FuturePeriod %in% timePer,]
  ###not sure what we were doing here?
  SS.sum[MeanSuit == 4, MeanSI := 5]
  SS.sum[MeanSuit == 5, MeanSI := 5]
  SS.sum[MeanSuit == 5, MeanSuit := 4]
  SS.sum[MeanSI == 0, MeanSI := 5]
  SS.sum <- unique(SS.sum)
  setorder(SS.sum,Spp,FuturePeriod)
  return(SS.sum)
}

#' Portfolio Subset
#' @export
edatopicSubset <- function(SSPredOrig, eda, pos = "Zonal"){
  if(pos == "Zonal"){
    SSPredFull <- SSPredOrig[grep("01",SSPredOrig$SS_NoSpace),]
  }else{
    edaSub <- eda[Edatopic == pos,]
    SSPredFull <- SSPredOrig[SS_NoSpace %in% edaSub$SS_NoSpace,]
  }
  SSPredFull[,BGC_analysis := gsub("/.*","", SS_NoSpace)]
  SSPredFull <- SSPredFull[,c("MergedBGC", "SS.pred", "SSprob", "SS_NoSpace", 
                              "FuturePeriod", "SiteNo","BGC_analysis")]
  return(SSPredFull)
}

#' Get Climate Summary Data
#' @importFrom RPostgres dbGetQuery
#' @export

dbGetClimSum <- function(con,BGC,Scn){
  cat("in get clim sum")
  climVarFut <- RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_fut where bgc in ('"
                                       ,BGC,"') and period in ('2021-2040','2041-2060','2061-2080') 
                                  and stat = 'mean'
                                  and climvar in ('CMD','Tmin_sp','Tmax_sm') and scenario = '",Scn,"'"))
  climVarFut <- setDT(climVarFut)
  
  climVarCurr <- setDT(RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_curr where bgc in ('"
                                        ,BGC,"') and period in ('1991 - 2020') 
                                  and stat in ('st.dev.Ann','mean') 
                                  and climvar in ('CMD','Tmin_sp','Tmax_sm')")))
  climVarCurr[stat == 'st.dev.Ann',stat := "stdev"]
  climVarSD <- climVarCurr[stat == "stdev",]
  climVarCurr <- climVarCurr[stat != "stdev",]
  climVar <- rbind(climVarCurr,climVarFut)
  climVar[,period := as.numeric(substr(period,1,4))]
  return(list(Mean = climVar,SD = climVarSD))
}

#' Simulate Climate for Portfolio
#' @export

simulateClimate <- function(climInfo){ ##package function
  climParams <- list()
  simResults <- data.table()
  climVar <- climInfo$Mean
  climVarSD <- climInfo$SD
  for(cvar in c("CMD","Tmin_sp","Tmax_sm")){
    climSub <- climVar[climvar == cvar,.(value = mean(value)), by = .(period)]
    climSD <- climVarSD[climvar == cvar,value]
    ##table of means by period
    dat <- data.table(Year = c(2000,2025,2055,2085),Mean = climSub$value)
    s <- approx(dat$Year, dat$Mean, n = 101) ##smooth
    
    ##simulate using mean and variance
    res <- numeric()
    for(i in 1:101){
      res[i] <- rnorm(1,mean = s$y[i],sd = climSD)
    }
    temp <- data.table(Year = 2000:2100, Value = res)
    temp[,Var := cvar]
    simResults <- rbind(simResults,temp,fill = T)
  }
  simResults <- dcast(simResults,Year ~ Var, value.var = "Value")
  return(simResults)
}

#' Species Limits
#' @import foreach
#' @importFrom RPostgres dbGetQuery
#' @export

dbGetSppLimits <- function(con,SuitTable,Trees){
  cat("in get limits")
  sppLimits <- foreach(spp = Trees, .combine = rbind) %do% {##package function
    temp <- SuitTable[Suitability == 1 & Spp == spp,] ##what units is Fd 1?
    sppUnits <- unique(temp$BGC)
    
    climSum <- RPostgres::dbGetQuery(con, paste0("select bgc,period,stat,climvar,value from szsum_curr where bgc in ('"
                                      ,paste(sppUnits,collapse = "','"),"') and period = '1991 - 2020' 
                                    and climvar in ('CMD','Tmin_sp','Tmax_sm')"))
    climSum <- setDT(climSum)
    climSum2 <- climSum[,.(Min = min(value),Max = max(value)),
                        by = .(stat,climvar)]
    climSum3 <- data.table(CMDMin = climSum2[stat == "mean" & climvar == "CMD",Min],
                           CMDMax = climSum2[stat == "mean" & climvar == "CMD",Max],
                           Tlow = climSum2[stat == "mean" & climvar == "Tmin_sp",Min],
                           Thigh = climSum2[stat == "mean" & climvar == "Tmax_sm",Max])
    climSum3[,Spp := spp]
    climSum3
  }
}

#' Run species Portfolio
#' @import foreach
#' @import scales
#' @import magrittr
#' @importFrom dplyr mutate 
#' @export

run_portfolio <- function(SiteList,climVar,SSPredAll,SIBEC,SuitTable,Trees,
                          TimePeriods,selectBGC,SuitProb,returnValue,sppLimits,
                          minAccept,boundDat,ProbPest){
  cat("in portfolio")
  nSpp <- length(Trees)
  treeList <- Trees
  ss_sum_save <- data.table()
  simOut <- data.table()
  it = 0
  allSitesSpp <- foreach(SNum = SiteList, .combine = rbind) %do% {
                           it = it+1
                           ##simulate climate
                           simResults <- simulateClimate(climVar)
                           SS.sum <- cleanData(SSPredAll,SIBEC,SuitTable,SNum, Trees, 
                                               timePer = TimePeriods,selectBGC = selectBGC)
                           ss_sum_save <- rbind(ss_sum_save,SS.sum, fill = T)
                           if(any(is.na(SS.sum$MeanSuit))){
                             warning("Missing Suitability in unit ",
                                     BGC,", sitenumber ",SNum," for ",
                                     SS.sum$Spp[is.na(SS.sum$MeanSuit)], ": They will be filled with suit = 4")
                             SS.sum$MeanSuit[is.na(SS.sum$MeanSuit)] <- 4
                           }
                           SS.sum[,FuturePeriod := as.numeric(FuturePeriod)]
                           if(length(TimePeriods) == 1){
                             temp <- SS.sum
                             temp$FuturePeriod <- SS.sum$FuturePeriod[1]+85
                             SS.sum <- rbind(SS.sum, temp)
                           }
                           
                           if(!is.null(SS.sum)){
                             cat(".")
                             output <- data.table("year" = seq(2000,2100,1))
                             
                             for (k in 1:nSpp){ ##for each tree
                               DatSpp <- SS.sum[Spp == treeList[k],]
                               dat <- data.table("Period" = rescale(as.numeric(DatSpp$FuturePeriod), 
                                                                    to = c(2000,2085)), 
                                                 "SIBEC" = DatSpp$MeanSI/50, "Suit" = DatSpp$MeanSuit)
                               
                               dat <- merge(dat, SuitProb, by = "Suit")
                               s <- approx(dat$Period, dat$SIBEC, n = 101) ##Smooth SI
                               p <- approx(dat$Period, dat$ProbDead, n = 101) ###Smooth Prob Dead
                               m <- approx(dat$Period, dat$NoMort, n = 101) ##Smooth No Mort
                               r <- approx(dat$Period, dat$Suit, n = 101)
                               ##r <- approx(dat$Period, dat$RuinSeverity, n = 101)
                               
                               ###data frame of annual data
                               annualDat <- data.table("Growth" = s[["y"]], "MeanDead" = p[["y"]], "NoMort" = m[["y"]], "Suit" = r[["y"]]) ##create working data
                               annualDat <- cbind(simResults,annualDat)
                               limits <- sppLimits[Spp == treeList[k],]
                               Returns <- SimGrowth(DF = annualDat,
                                                       cmdMin = limits[[1]],cmdMax = limits[[2]],
                                                       tempMin = limits[[3]],tempMax = limits[[4]],climLoss = 0.005)
                               tmpR <- c(0,Returns)
                               assets <- Returns - tmpR[-length(tmpR)]
                               temp <- data.frame(Spp = treeList[k], 
                                                  Year = 1:101, It = it, Returns = Returns)
                               simOut <- rbind(simOut,temp,fill = T)
                               output <- cbind(output, assets)
                             } ## for each tree species
                             
                             colnames(output) <- c("Year", treeList)
                             
                             ####Portfolio#######################################
                             returns <- output
                             returns[,Year := NULL]
                             ###only include species with mean return > 1 in portfolio
                             use <- colnames(returns)[colMeans(returns) > quantile(colMeans(returns),0.25)] ###should probably be higher
                             use <- use[use %in% colnames(covMat)]
                             if(length(use) > 1){
                               returns <- returns[,..use]
                               #print(use)
                               #sigma2 <- cor(returns) ###to create cov mat from returns
                               sigma2 <- covMat[use,use]
                               #print(colnames(sigma2))
                               ef <- optimise_portfolio(returns, sigma2, boundDat,minAccept) 
                               setnames(ef,old = c("frontier_sd","return","sharpe"),
                                        new = c("Sd","RealRet","Sharpe"))
                               ef[,Return := 1:20]
                               
                               eff_front2 <- ef
                               eff_front2[,RealRet := RealRet/max(RealRet)]
                               eff_front2[,SiteNo := SNum]
                               melt(eff_front2,id.vars = c("SiteNo", "Return"),variable.name = "Spp")
                             }else NULL
                           }else{NULL}
  }
  
  ##preprocess for plotting
  BGC <- selectBGC
  efAll <- allSitesSpp
  efAll <- dcast(efAll,Return ~ Spp, fun.aggregate = function(x){sum(x)/(length(SiteList))})
  efAll <- na.omit(efAll)
  #efAll$RealRet <- efAll$RealRet/max(efAll$RealRet) ##standardise return
  RetCurve <- approx(efAll$RealRet,efAll$Sd,xout = returnValue)
  ret90 <- RetCurve$y
  maxSharpe <- efAll[Sharpe == max(Sharpe),!c("Return","Sharpe")]
  maxSPos <- maxSharpe$Sd
  maxSharpe <- t(maxSharpe) %>% as.data.frame() %>% 
    mutate(Spp = rownames(.)) %>% set_colnames(c("Sharpe_Opt","Spp"))
  ret90Props <- efAll[which.min(abs(RealRet - returnValue)),-c("Return","Sharpe")]
  ret90Props <- t(ret90Props) %>% as.data.frame() %>% 
    mutate(Spp = rownames(.)) %>% set_colnames(c("Set_Return","Spp"))
  maxSharpe$SSCurrent <- selectBGC
  maxSharpe$Unit <- selectBGC
  maxSharpe$Set_Return <- ret90Props$Set_Return
  maxSharpe$Set_Return[maxSharpe$Spp == "Sd"] <- ret90
  setDT(maxSharpe)
  efAll <- efAll[,-c("Return","Sharpe")]
  efAll <- melt(efAll, id.vars = "Sd")
  efAll$Unit <- BGC
  efAll <- efAll[is.finite(Sd),]
  cat("return from port fun")
  return(list(raw = efAll,summary = maxSharpe,ssdat = ss_sum_save,simulated = simOut))
}

#' Plot Efficient Frontier
#' @import ggplot2
#' @import ggthemes
#' @export
ef_plot <- function(efAll,intDat,colScale){
  # efAll <- outAll$GraphDat
  # intDat <- outAll$MaxS
  efAll$variable <- factor(efAll$variable, levels = sort(unique(as.character(efAll$variable))))
  ggplot(efAll[efAll$variable != "RealRet",],aes(x = Sd, y = value,group = variable))+
    geom_area(aes(fill = variable), size = 0.00001, col = "grey50", stat = "identity")+
    colScale +
    geom_vline(data = intDat[intDat$Spp == "Sd",], aes(xintercept = Sharpe_Opt,colour = "blue"), 
               linetype = "twodash", size = .75)+
    geom_vline(data = intDat[intDat$Spp == "Sd",], aes(xintercept = Set_Return,colour = "grey52"),
               linetype = "dashed", size = .75)+
    geom_line(data = efAll[efAll$variable == "RealRet",], 
              aes(x = Sd, y = value,colour = "black"),linetype = "F1",size = .75)+
    scale_colour_identity(name = "", guide = 'legend', labels = c("Return","MaxSharpe","SetReturn"))+
    scale_x_reverse() +
    xlab("Max Return --> Minimized Risk")+
    ylab("Portfolio Ratio")+
    guides(fill=guide_legend("Species"))+
    theme_few()+
    facet_wrap(.~Unit, scales = "free_x")
}