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
    col.names = c("Tree Species", "CFRG Suitability",
                  "Projected Feasibility", "Flag", "Period",
                  "Future Projected Feasibility", "Fail Risk"),
    table.attr = 'class="table table-hover"')
}
uData$cciss_summary_dt <- cciss_summary_dt

# CCISS Results
output$results_feas <- function() {
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  update_flag()
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results_dt(cciss_results, siteref, siteserie, feas_filter)
}

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_results_dt <- function(data, siteref, siteserie, filter, format = "html") {
  if (filter == "a") {
    for(i in c("NewSuit_1991","NewSuit_2021","NewSuit_2041","NewSuit_2061","NewSuit_2081")){ ##set NA to X
      data[is.na(get(i)), (i) := 4]
    }
    data <- data[(NewSuit_1991+NewSuit_2021+NewSuit_2041+NewSuit_2061+NewSuit_2081) < 24,]
  } else if (filter == "f") {
    data <- data[Curr %in% c(1,2,3),]
  }
  
  data <- data[SiteRef == siteref & SS_NoSpace %in% siteserie,
               list(Species, Period, PredFeasSVG, CFSuitability, Curr, EstabFeas, MidRotSVG, Risk60, Risk80)]
  
  for(i in c("Curr","EstabFeas","Risk60","Risk80")){ ##set NA to X
    data[is.na(get(i)), (i) := "X"]
  }
    
  tempTable <- knitr::kable(
    data, format = format, align = c("l","c","c","c","c","c","c","c","c"), escape = FALSE,
    col.names = c("Tree Species", "Period", "Modelled Feasibility",
                  "CFRG", "Ecological","Establishment",
                  "Trend to Rotation","60 Year", "80 Year"),
    table.attr = 'class="table table-hover"') %>%
    add_header_above(c(" " = 2, "Raw Votes" = 1, "Historic Feasibility" = 2, 
                       "Predicted Feasibility" = 2, "Future Risk" = 2))
}
uData$cciss_results_dt <- cciss_results_dt
