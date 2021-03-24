# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

# CCISS Summary of results
output$summary_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_summary <- uData$cciss_summary
  if (is.null(cciss_summary)) return(NULL)
  cciss_summary_dt(cciss_summary, siteref, siteserie)
})

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_summary_dt <- function(data, siteref, siteserie, app = TRUE) {
  data <- data[SiteRef == siteref & `Site Series` %in% siteserie, -c("SiteRef")]
  if (isTRUE(app)) {
    DT::datatable(data, escape = FALSE, rownames = FALSE, options = list(info = FALSE,
      scrollCollapse = FALSE, lengthChange = FALSE, ordering = FALSE, autoWidth = TRUE, 
      searching = FALSE, pageLength = nrow(data), paging = FALSE, columnDefs = list(
        list(className = 'dt-center', targets = 2:10), list(width = "130px", targets = 1)
      )
    ))
  } else {
    tableHTML::tableHTML(data, escape = FALSE, rownames = FALSE, border = 0) %>%
      tableHTML::add_css_column(css = list("text-align", "center"), columns = 3:11)
  }
}
uData$cciss_summary_dt <- cciss_summary_dt

# CCISS Results
output$results_feas <- DT::renderDataTable({
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results_dt(cciss_results, siteref, siteserie, feas_filter)
})

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_results_dt <- function(data, siteref, siteserie, filter, app = TRUE) {
  if (filter == "a") {
    data <- data[MeanSuit < 4]
  } else if (filter == "f") {
    data <- data[`Projected Feasibility` %chin% c("1", "2", "3")]
  }
  data <- data[SiteRef == siteref & `Site Series` %in% siteserie,
               -c("SiteRef", "MeanSuit", "Region", "ZoneSubzone", "SS_NoSpace", "Spp", "OrderSuit")]
  if (isTRUE(app)) {
    DT::datatable(data, escape = FALSE, rownames = FALSE, options = list(info = FALSE,
      scrollCollapse = FALSE, lengthChange = FALSE, ordering = FALSE, autoWidth = TRUE, 
      searching = FALSE, pageLength = nrow(data), paging = FALSE, columnDefs = list(
        list(className = 'dt-center', targets = 2:6), list(width = "10%", targets = 4:6)
      )
    ))
  } else {
    tableHTML::tableHTML(data, escape = FALSE, rownames = FALSE, border = 0) %>%
      tableHTML::add_css_column(css = list("text-align", "center"), columns = 3:7)
  }
}
uData$cciss_results_dt <- cciss_results_dt
