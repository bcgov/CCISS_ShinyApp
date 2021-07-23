makeColScale <- function(Trees){
  cols <- TreeCols
  myPal <- cols$HexColour
  names(myPal) <- cols$TreeCode
  myColours <- data.table(TreeCode = Trees)
  myColours <- cols[myColours, on = "TreeCode"]
  myColours <- myColours[!is.na(HexColour),]
  pal <- myColours$HexColour
  names(pal) <- myColours$TreeCode
  colScale <- scale_fill_manual(name = "variable", values = pal)
  return(colScale)
}

observeEvent(input$generate_portfolio,{
  timePeriods <- input$time_periods
  returnValue <- input$return_level
  Trees <- treeList <- input$tree_species
  minAccept <- input$min_accept
  ProbPest <- input$prob_pest
  FutScn <- input$fut_scn
  #EdaPos <- input$eda_pos
  SiteList <- uData$pts$Site
  ##setup climate_data connection
  poolclim <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "bgc_climate_data",
    host = Sys.getenv("BCGOV_HOST"),
    port = 5432, 
    user = Sys.getenv("BCGOV_USR"),
    password = Sys.getenv("BCGOV_PWD")
  )
  onStop(function() {
    poolClose(poolclim)
  })
  
  #Trees <- treeList <- c("Py","Fd","At","Pl","Sx","Bl","Cw","Hw","Pw","Ss","Lw","Ba","Hm","Dr","Mb")
  SuitProb <- data.frame("Suit" = c(1,2,3,4), "ProbDead" = c(0.1,0.5,1,4), "NoMort" = c(95,85,75,50))
  boundDat <- data.table(Spp = Trees)
  boundDat[,`:=`(minWt = 0, maxWt = 1)]
  #timePeriods = c(1961,1991,2021,2041,2061)
  #returnValue = 0.9
  #FutScn <- "ssp370"
  #minAccept <- 0.01
  SuitTable <- copy(S1)
  setnames(SuitTable,old = "Feasible",new = "Suitability",skip_absent = T)
  
  # siteids <- c(6487982,6484391,6484900,6485410,6485920)
  # bgcDat <- dbGetCCISS(pool,siteids,avg = F, scn = "ssp370")
  # sspreds <- edatopicOverlap(bgcDat,Edatope = E1) ##reuse from uData$sspreds
  # SSPredOrig <- sspreds
  
  SSPredOrig <- copy(uData$eda_out) ##migtht need to think what happens if multiple scenarios selected
  SSPredOrig[,allOverlap := NULL]
  setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
  SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]
  
  SSPredFull <- edatopicSubset(SSPredOrig,eda,pos = "Zonal") ##this should be changeable
  nSpp <- length(treeList)
  Units <- unique(SSPredFull$BGC_analysis)
  BGC = Units[1]
  SSPredBGC <- SSPredFull[BGC_analysis == BGC,-("BGC_analysis")]
  SSList <- unique(SSPredBGC$SS_NoSpace)
  selectBGC = SSList[1] ##this is where we change it to allow multiple BGCs
  SSPredAll <- SSPredBGC[SSPredBGC$SS_NoSpace == selectBGC,]
  SiteList <- unique(SSPredAll$SiteNo)
  SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]
  
  climVar <- dbGetClimSum(poolclim,BGC,FutScn)
  sppLimits <- dbGetSppLimits(poolclim,SuitTable,Trees)
  
  SL <- SiteList
  numTimes <- as.integer(10/length(SL))
  SL <- rep(SL, each = numTimes)
  port_results <- run_portfolio(SL,climVar,SSPredAll,SIBEC,SuitTable,
                                Trees,timePeriods,selectBGC,SuitProb,returnValue,sppLimits,minAccept,boundDat)
  print("Done Portfolio")
  uData$port_results <- port_results
  portFlag("Done Port")
})

output$efficient_frontier <- renderPlot({
  input$prob_pest
  portFlag()
  print("Oof")
  if(is.null(uData$port_results)) return(NULL)
  
  colScale <- makeColScale(input$tree_species)
  dat <- copy(uData$port_results)
  print("About to Plot")
  print(ef_plot(dat$raw,dat$summary,colScale))
})


