# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

# CCISS Summary of results
output$summary_feas <- function() {
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_summary <- uData$cciss_summary
  if (is.null(cciss_summary)) return(NULL)
  cciss_summary_dt(cciss_summary, siteref, siteserie)
}

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_summary_dt <- function(data, siteref, siteserie, format = "html") {
  data <- data[SiteRef == siteref & SS_NoSpace %in% siteserie,
               list(Species, CFSuitability, ProjFeas, Flag, Period, FutProjFeas, FailRisk)]
  knitr::kable(
    data, format = format, align = c("l","c","c","c","c","c","c"), escape = FALSE,
    col.names = c("Tree Species", "Chief Forester Recommended Suitability",
                  "Projected Feasibility", "Flag", "Period",
                  "Future Projected Feasibility", "Fail Risk"),
    table.attr = 'class="table table-hover"')
}
uData$cciss_summary_dt <- cciss_summary_dt

# CCISS Results
output$results_feas <- function() {
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results_dt(cciss_results, siteref, siteserie, feas_filter)
}

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_results_dt <- function(data, siteref, siteserie, filter, format = "html") {
  if (filter == "a") {
    data <- data[MeanSuit < 4 | CFSuitability %chin% c("1", "2", "3")]
  } else if (filter == "f") {
    data <- data[ProjFeas %chin% c("1", "2", "3") | CFSuitability %chin% c("1", "2", "3")]
  }
  data <- data[SiteRef == siteref & SS_NoSpace %in% siteserie,
               list(Species, Period, PredFeasSVG, CFSuitability, ProjFeas, MidRotTrend)]
  knitr::kable(
    data, format = format, align = c("l","c","c","c","c","c"), escape = FALSE,
    col.names = c("Tree Species", "Period", "Predicted Feasibility",
                  "Chief Forester Recommended Suitability", "Projected Feasibility",
                  "Continuing Trend at Mid Rotation (2040-2070)"),
    table.attr = 'class="table table-hover"')
}
uData$cciss_results_dt <- cciss_results_dt
