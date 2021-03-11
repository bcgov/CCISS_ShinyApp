bgc_react <- reactive({
  avg <- input$aggregation
  rcp <- input$rcp_scenario
  # to invalidate reactive with points table change
  # and force this value and all others that depends
  # on it to recompute
  pts_react <- input$points_table_rows_all
  
  pts <- uData$points[!is.na(Longitude) & !is.na(Latitude), list(Longitude, Latitude)]
  if (nrow(pts) == 0) return(NULL)
  withProgress({
    incProgress(1)
    res <- dbGetCCISS(pool, pts, as.logical(avg), rcp)
    incProgress(2)
  }, min = 0, max = 2, message = "Fetching BGC futures", detail = "waiting for db")
  return(res)
})

output$bgc_site_ref_select <- renderUI({
  b <- bgc_react()
  avg <- input$aggregation
  rcp <- paste(input$rcp_scenario, collapse = ", ")
  if (is.null(b)) return(span("Add points to generate BGC Futures"))
  list(
    radioButtons("current_siteref", label = "Results:", choices = unique(b$SiteRef)),
    span("Averaged: ", br(), avg, br(), br(),
         "RCP scenratio: ", br(), rcp)
  )
})

observeEvent(input$current_siteref, priority = 100, {
  output$bgc_futures <- renderUI({
    b <- bgc_react()
    siteref <- input$current_siteref
    if (is.null(b)) return(NULL)
    b <- b[SiteRef == siteref]
    bgc <- unique(b$BGC)
    list(span(paste("Current BGC :", bgc)), br(), br(),
         plotly::plotlyOutput("bgc_futures_plot", width = "100%", height = "100%"))
  })
})

observeEvent(input$current_siteref, priority = 50, {
  b <- bgc_react()
  siteref <- input$current_siteref
  if (is.null(b)) return(NULL)
  output$bgc_futures_plot <- renderPlotly({
    b <- b[SiteRef == siteref]
    bgc <- unique(b$BGC)
    cm_bcg_fplot(b[BGC == bgc]) %>% plotly::layout(margin = list(b = 125))
  })
})