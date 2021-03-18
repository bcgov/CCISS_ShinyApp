observeEvent(input$generate_results, priority = 100, {
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
  bgc            <- uData$bgc            <- bgc(pool, pts$Site, avg, rcp)
  cciss          <- uData$cciss          <- cciss(bgc)
  cciss_summary  <- uData$cciss_summary  <- cciss_summary(cciss)
  cciss_detailed <- uData$cciss_detailed <- cciss_detailed(cciss)
  
  # UI select choices
  siteseries_all <- uData$siteseries_all <- sort(
    unique(c(cciss_detailed$`Site Series`, cciss_summary$`Site Series`))
  )
  siterefs       <- uData$siterefs       <- unique(bgc$SiteRef)
  if (!isTRUE(avg)) {
    # ordering choices to match order in points table and create a name for each choice
    siterefs <- pts[Site %in% unique(bgc$SiteRef),
      {x <- Site; names(x) <- paste(ID, Site, sep = " / "); return(x)}
    ]
  }
  
  # Dynamic UI select choices that depends on previous select choice
  siteref <- head(siterefs, 1)
  siteseries <- sort(
    unique(
      c(cciss_detailed[SiteRef == siteref]$`Site Series`,
        cciss_summary[SiteRef == siteref]$`Site Series`
      )
    )
  )
  updateSelectInput(inputId = "siteref_feas", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "siteref_bgc_fut", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "site_series_feas", choices = siteseries)
  updateCheckboxGroupInput(inputId = "report_filter",choices = siteseries_all, selected = siteseries_all)
  
  
  # Use ui injected javascript to show download button and hide generate button
  session$sendCustomMessage(type="jsCode", list(code= "$('#download_span').show()"))
  session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
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