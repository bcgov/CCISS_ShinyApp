# `siteref_bgc_fut` Sites select update in generate.R at the same time

output$current_bgc_fut <- renderText({
  siteref <- input$siteref_bgc_fut
  if (is.null(uData$bgc)) return(NULL)
  uData$bgc[SiteRef == siteref, unique(BGC)]
})

output$bgc_fut_plot <- plotly::renderPlotly({
  siteref <- input$siteref_bgc_fut
  if (is.null(uData$bgc)) return(NULL)
  bgc_fut_plotly(uData$bgc[SiteRef == siteref])
})