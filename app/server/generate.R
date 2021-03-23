observeEvent(input$generate_results, priority = 100, {
  
  ticker <- tic("Save Map Inputs")
  # On generate click, we are taking a snapshot of the current points
  # and calculating results. All relevant results will be stored in the
  # userdata environment for further reuse. User has the ability to update
  # results on demand instead of on app state change. This reduce the load
  # on the app and give some room in case computation get more costly
  # in the future.
  
  # Input from the app
  avg            <- uData$avg            <- as.logical(input$aggregation)
  rcp            <- uData$rcp            <- input$rcp_scenario
  pts            <- uData$pts            <- userpoints$dt
  
  # Results from processing
  tic("Fetch CCISS Data from DB", ticker)
  bgc            <- uData$bgc            <- bgc(pool, pts$Site, avg, rcp)
  tic("Process CCISS data", ticker)
  cciss          <- uData$cciss          <- cciss(bgc)
  tic("Format CCISS Results", ticker)
  cciss_results  <- uData$cciss_results  <- cciss_results(cciss, pts, avg)
  tic("Format CCISS Summary", ticker)
  cciss_summary  <- uData$cciss_summary  <- cciss_summary(cciss, pts, avg)
  
  
  # UI select choices
  tic("Determine UI choices", ticker)
  ssa <- sort(unique(c(cciss_results$`Site Series`, cciss_summary$`Site Series`)))
  names(ssa) <- paste(
    ssa,
    stocking_standards$SiteSeriesName[match(ssa, stocking_standards$SS_NoSpace)]
  )
  siteseries_all <- uData$siteseries_all <- ssa
  siterefs       <- uData$siterefs       <- unique(bgc$SiteRef)
  if (!isTRUE(avg)) {
    # ordering choices to match order in points table and create a name for each choice
    siterefs <- pts[Site %in% unique(bgc$SiteRef),
      {x <- Site; names(x) <- paste(ID, Site, sep = " / "); return(x)}
    ]
  }
  
  # Dynamic UI select choices that depends on previous select choice
  siteref <- head(siterefs, 1)
  siteseries <- {
    x <- c(cciss_results[SiteRef == siteref]$`Site Series`, cciss_summary[SiteRef == siteref]$`Site Series`)
    x <- unique(x)
    x <- sort(x)
  }
  
  tic("Populate UI choices", ticker)
  updateSelectInput(inputId = "siteref_feas", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "siteref_bgc_fut", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "siteref_silv", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "site_series_feas", choices = siteseries, selected = head(siteseries, 1))
  updateSelectInput(inputId = "site_series_silv", choices = siteseries, selected = head(siteseries, 1))
  updateCheckboxGroupInput(inputId = "report_filter",choices = siteseries_all, selected = siteseries_all)
  
  # Use ui injected javascript to show download button and hide generate button
  tic("Inject javascript", ticker)
  session$sendCustomMessage(type="jsCode", list(code= "$('#download_span').show()"))
  session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
  updateActionButton(inputId = "generate_results", label = "Refresh results")
  
  tocker <- toc(ticker)
  
  # Render models info + timings in About
  output$modelsinfo <- renderTable({models_info}, )
  output$timings <- plotly::renderPlotly({
    tocker
  })
})

generateState <- function() {
  # This prevent the generate button from being enabled when
  # points do not have valid geometry. There is another
  # validation in new_points to make sure the newly
  # added points are located inside the cciss geometry.
  # Only valid points are used to calculated
  if (nrow(userpoints$dt[!is.na(Long) & !is.na(Lat)])) {
    session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', false)"))
  } else {
    session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
  }
}

observeEvent(userpoints$dt, {generateState()})
observeEvent(input$aggregation, {generateState()})
observeEvent(input$rcp_scenario, {generateState()})
