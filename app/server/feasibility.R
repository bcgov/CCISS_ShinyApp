observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_feas
  siteseries <- sort(
    unique(
      c(uData$cciss_results[SiteRef == siteref]$`Site Series`,
        uData$cciss_summary[SiteRef == siteref]$`Site Series`
      )
    )
  )
  updateSelectInput(inputId = "site_series_feas", choices = siteseries)
})

output$summary_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_summary <- uData$cciss_summary
  if (is.null(cciss_summary)) return(NULL)
  cciss_summary <- cciss_summary[SiteRef == siteref & `Site Series` %in% siteserie, -c("SiteRef")]
  DT::datatable(cciss_summary, escape = FALSE, rownames = FALSE, options = list(
    scrollCollapse = FALSE, lengthChange = FALSE, autoWidth = TRUE, columnDefs = list(
      list(className = 'dt-center', targets = 2:10), list(width = "130px", targets = 1)
    )
  ))
})

output$results_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results <- cciss_results[SiteRef == siteref & `Site Series` %in% siteserie]
  if (feas_filter == "a") {
    cciss_results <- cciss_results[MeanSuit < 4]
  } else if (feas_filter == "f") {
    cciss_results <- cciss_results[`Projected Feasibility` %chin% c("1", "2", "3")]
  }
  cciss_results <- cciss_results[, -c("SiteRef", "MeanSuit", "Region", "ZoneSubzone",
                                        "SS_NoSpace", "Spp", "OrderSuit")]
  DT::datatable(cciss_results, escape = FALSE, rownames = FALSE, options = list(
    scrollCollapse = FALSE, lengthChange = FALSE, autoWidth = TRUE, columnDefs = list(
      list(className = 'dt-center', targets = 2:6), list(width = "10%", targets = 4:6)
    )
  ))
})
