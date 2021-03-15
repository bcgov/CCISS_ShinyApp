output$ss_site_ref_select <- radio_select_siteref("ss")
output$ss_legend <- renderUI({
  list(
    span("Legend"),
    br(),
    suitability_legend()
  )
})

observeEvent(input$current_siteref_ss, priority = 100, {
  output$species_suitability_summary <- renderUI({
    cciss_summary <- cciss_summary()
    if (is.null(cciss_summary)) return(NULL)
    DT::DTOutput("species_suitability_summary_dt", width = "100%", height = "100%")
  })
  output$species_suitability_detailed <- renderUI({
    cciss_raw <- cciss_raw()
    if (is.null(cciss_raw)) return(NULL)
    DT::DTOutput("species_suitability_detailed_dt", width = "100%", height = "100%")
  })
})

observeEvent(input$current_siteref_ss, priority = 50, {
  output$ss_select_series <- renderUI({
    ret <- NULL
    withProgress({
      cciss_summary <- cciss_summary()
      cciss_raw <- cciss_raw()
      incProgress(1, message = "Rendering table", detail = "")
      if (is.null(cciss_summary) || is.null(cciss_raw)) return(NULL)
      siteref <- input$current_siteref_ss
      siteseries <- sort(
        unique(
          c(cciss_raw[SiteRef == siteref]$`Site Series`,
            cciss_summary[SiteRef == siteref]$`Site Series`
          )
        )
      )
      ret <- list(br(), 
        selectInput(
          "select_site_series",
          label = "Site Series",
          choices = siteseries
      ))
      incProgress(2, message = "Done", detail = "")
    }, min = 0, max = 2, message = "CCISS", detail = "processing...")
    return(ret)
  })
})

observeEvent(input$select_site_series, {
  cciss_summary <- cciss_summary()
  cciss_raw <- cciss_raw()
  if (is.null(cciss_summary) || is.null(cciss_raw)) return(NULL)
  siteref <- input$current_siteref_ss
  siteseries <- input$select_site_series
  output$species_suitability_summary_dt <- DT::renderDataTable({
    cciss_summary <- cciss_summary[SiteRef == siteref & `Site Series` %in% siteseries]
    DT::datatable(
      cciss_summary[, SiteRef := NULL], escape = FALSE, rownames = FALSE,
      options = list(
        columnDefs = list(
          list(className = 'dt-center', targets = 2:10)
        ),
        scrollY="100vh",
        scrollCollapse = FALSE
      )
    )
  })
  output$species_suitability_detailed_dt <- DT::renderDataTable({
    cciss_raw <- cciss_raw[SiteRef == siteref & `Site Series` %in% siteseries]
    DT::datatable(
      cciss_raw[, SiteRef := NULL], escape = FALSE, rownames = FALSE,
      options = list(
        columnDefs = list(
          list(className = 'dt-center', targets = 2:6)
        ),
        scrollY="100vh",
        scrollCollapse = FALSE
      )
    )
  })
})

