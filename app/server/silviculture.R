observeEvent(input$siteref_silv, priority = 50, {
  if (is.null(uData$cciss_detailed) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_silv
  siteseries <- sort(
    unique(
      c(uData$cciss_detailed[SiteRef == siteref]$`Site Series`,
        uData$cciss_summary[SiteRef == siteref]$`Site Series`
      )
    )
  )
  updateSelectInput(inputId = "site_series_silv", choices = siteseries)
})

output$silviculture_block <- renderUI({
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  cciss_detailed <- uData$cciss_detailed
  if (is.null(cciss_detailed)) return(NULL)
  standardblocks(siteref, siteserie, cciss_detailed)
})