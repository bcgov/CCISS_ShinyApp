output$bgc_site_ref_select <- radio_select_siteref("bgc")

observeEvent(input$current_siteref_bgc, priority = 100, {
  output$bgc_futures <- renderUI({
    b <- bgc_react()
    siteref <- input$current_siteref_bgc
    if (is.null(b)) return(NULL)
    b <- b[SiteRef == siteref]
    bgc <- unique(b$BGC)
    output$current_futures_bgc <- renderText(bgc)
    plotly::plotlyOutput("bgc_futures_plot", width = "100%", height = "100%")
  })
})

observeEvent(input$current_siteref_bgc, priority = 50, {
  withProgress({
    b <- bgc_react()
    incProgress(1, message = "Plotting BGC futures", detail = "")
    siteref <- input$current_siteref_bgc
    if (is.null(b)) return(NULL)
    output$bgc_futures_plot <- plotly::renderPlotly({
      b <- b[SiteRef == siteref]
      bgc <- unique(b$BGC)
      cm_bcg_fplot(b[BGC == bgc])
    })
    incProgress(2, message = "Done", detail = "")
  }, min = 0, max = 2, message = "Fetching BGC futures", detail = "waiting for db")
})