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

##update possible species
observeEvent(input$port_bgc,{
  if(input$generate_results > 0){
    suitTrees <- copy(uData$cciss_summary)
    #print(colnames(suitTrees))
    suitTrees <- suitTrees[NewSuit %in% c(1,2,3,4),.(Spp, BGC = ZoneSubzone)]
    suitTrees <- unique(suitTrees)
    tree_opts <- suitTrees[BGC == input$port_bgc,Spp]
    tree_opts <- tree_opts[tree_opts != "Ac"]
    updateSelectInput(inputId = "tree_species",
                      choices = tree_opts,selected = tree_opts)
  }
 
})

output$setbounds <- renderRHandsontable({
  Trees <- input$tree_species
  boundDat <- data.table(Spp = Trees)
  boundDat[,`:=`(minWt = 0, maxWt = 1)]
  rhandsontable(boundDat)
})

observeEvent(input$generate_portfolio,{
  timePeriods <- input$port_length
  returnValue <- input$return_level
  Trees <- treeList <- input$tree_species
  minAccept <- input$min_accept
  ProbPest <- input$prob_pest
  BGC <- input$port_bgc
  siteLoc <- input$port_ss
  FutScn <- "ssp245"
  #EdaPos <- input$eda_pos
  SiteList <- uData$pts$Site
  boundDat <- hot_to_r(input$setbounds)
  ##setup climate_data connection
  allPeriods <- c(1961,1991,2021,2041,2061,2081)
  selectPer <- which(allPeriods == timePeriods)
  timePeriods <- allPeriods[1:selectPer]
  
  withProgress(message = "Optimising...", detail = "Lots of calculations...", {
    #Trees <- treeList <- c("Py","Fd","At","Pl","Sx","Bl","Cw","Hw","Pw","Ss","Lw","Ba","Hm","Dr","Mb")
    SuitProb <- data.frame("Suit" = c(1,2,3,4), "ProbDead" = c(0.1,0.5,1,4), "NoMort" = c(95,85,75,50))
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
    
    SSPredOrig <- copy(uData$eda_out)
    SSPredOrig[,allOverlap := NULL]
    setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
    SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]
    
    SSPredFull <- edatopicSubset(SSPredOrig,E1,pos = siteLoc) ##this should be changeable
    nSpp <- length(treeList)
    Units <- unique(SSPredFull$BGC_analysis)
    SSPredBGC <- SSPredFull[BGC_analysis == BGC,-("BGC_analysis")]
    SSList <- unique(SSPredBGC$SS_NoSpace)
    selectBGC = SSList[1] ##this is where we change it to allow multiple BGCs
    SSPredAll <- SSPredBGC[SSPredBGC$SS_NoSpace == selectBGC,]
    SiteList <- unique(SSPredAll$SiteNo)
    SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]
    
    incProgress()
    climVar <- dbGetClimSum(poolclim,BGC,FutScn)
    sppLimits <- dbGetSppLimits(poolclim,SuitTable,Trees)
    incProgress()
    
    SL <- SiteList
    numTimes <- as.integer(25/length(SL))
    SL <- rep(SL, each = numTimes)
    port_results <- run_portfolio(SL,climVar,SSPredAll,SIBEC,SuitTable,
                                  Trees,timePeriods,selectBGC,SuitProb,returnValue,
                                  sppLimits,minAccept,boundDat,ProbPest)
    incProgress(amount = 0.6)
    print("Done Portfolio")
    #print(port_results$raw)
    portfolio_results$data <- port_results
  })
  
})

output$efficient_frontier <- renderPlot({
  if(is.null(portfolio_results$data)) return(NULL)
  
  colScale <- makeColScale(input$tree_species)
  dat <- copy(portfolio_results$data)
  print(ef_plot(dat$raw,dat$summary,colScale))
})

output$port_table <- renderTable({
  if(is.null(portfolio_results$data)) return(NULL)
  dat <- copy(portfolio_results$data)
  print(dat$simulated)
  temp <- dat$summary
  temp <- temp[!Spp %chin% c("RealRet","Sd"),.(Spp,Sharpe_Opt,Set_Return)]
  setnames(temp,c("Species","Sharpe","SetReturn"))
  temp
})

output$port_sssum <- renderDT({
  if(is.null(portfolio_results$data)) return(NULL)
  dat <- copy(portfolio_results$data$ssdat)
  dat <- melt(dat, id.vars = c("Spp","FuturePeriod"))
  dat2 <- dcast(dat, Spp + FuturePeriod ~ variable, fun.aggregate = mean)
  setnames(dat2, old = "FuturePeriod",new = "Period")
  formatRound(datatable(dat2,caption = "Mean SI and Feasibility"),columns = "MeanSI")
})

observeEvent(input$show_sssum,{
  showModal(modalDialog(
    h2("Site Index and Feasibility Values"),
    DTOutput("port_sssum"),
    size = "l"
  ))
})
