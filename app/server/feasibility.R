observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_detailed) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_feas
  siteseries <- sort(
    unique(
      c(uData$cciss_detailed[SiteRef == siteref]$`Site Series`,
        uData$cciss_summary[SiteRef == siteref]$`Site Series`
      )
    )
  )
  updateSelectInput(inputId = "site_series_feas", choices = siteseries)
})

output$summary_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteseries <- input$site_series_feas
  cciss_summary <- uData$cciss_summary
  if (is.null(cciss_summary)) return(NULL)
  cciss_summary <- cciss_summary[SiteRef == siteref & `Site Series` %in% siteseries]
  DT::datatable(
    cciss_summary[, SiteRef := NULL], escape = FALSE, rownames = FALSE,
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = 2:10)
      ),
      scrollCollapse = FALSE, lengthChange = FALSE
    )
  )
})

output$detailed_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteseries <- input$site_series_feas
  cciss_detailed <- uData$cciss_detailed
  if (is.null(cciss_detailed)) return(NULL)
  cciss_detailed <- cciss_detailed[SiteRef == siteref & `Site Series` %in% siteseries]
  DT::datatable(
    cciss_detailed[, SiteRef := NULL], escape = FALSE, rownames = FALSE,
    options = list(
      columnDefs = list(
        list(className = 'dt-center', targets = 2:6),
        list(width = "10%", targets = 4:6)
      ),
      scrollCollapse = FALSE, lengthChange = FALSE, autoWidth = TRUE
    )
  )
})
