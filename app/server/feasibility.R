# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

# CCISS Results
output$results_feas <- function() {
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  update_flag()
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results_dt(cciss_results, siteref, siteserie, feas_filter)
}

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_results_dt <- function(data, siteref, siteserie, filter, format = "html") {
  if(filter == "a"){
    for(i in c("NewSuit_1991","NewSuit_2021","NewSuit_2041","NewSuit_2061","NewSuit_2081")){ ##set NA to X
      data[is.na(get(i)), (i) := 4]
    }
    data <- data[(NewSuit_1991+NewSuit_2021+NewSuit_2041+NewSuit_2061+NewSuit_2081) < 24,]
  }else if (filter == "f"){
    data <- data[Curr %in% c(1,2,3) | CFSuitability %in% c(1,2,3) | ccissFeas %in% c(1,2,3),]
  }
  data <- data[!is.na(Trend),]
  data[,Trend := cell_spec(Trend,"html", color = fifelse(Improve > 67,'green',fifelse(Improve < 33, "red","purple")))]
  #data[,Period := cell_spec(Period,font_size = 12)]
  data <- data[SiteRef == siteref & SS_NoSpace %in% siteserie,
               .(Species, Period, PredFeasSVG, CFSuitability, Curr, EstabFeas, ccissFeas, Trend)]
  data[,Curr := as.character(Curr)]
  data[Curr == 4,Curr := "X"]
  for(i in c("Curr","EstabFeas")){ ##set NA to X
    data[is.na(get(i)), (i) := "X"]
  }
  data[Curr != "X", Curr := paste0("E", Curr)]
  data[!EstabFeas %in% c("X","Trial"), EstabFeas := paste0("E", EstabFeas)]
  data[ccissFeas != "X", ccissFeas := paste0("E", ccissFeas)]
  # data[,lapply(.SD, function(x){cell_spec(x,font_size = 24)}),
  #      .SDcols = c("CFSuitability","Curr","EstabFeas","ccissFeas")]
  
  tempTable <- knitr::kable(
    data, format = format, align = c("l","c","c","c","c","c","c","c"), escape = FALSE,
    col.names = c("Tree Species", "Period", "Modelled Feasibility",
                  "CFRG", "Environmental","Establishment",
                  "Future (cciss)","Improve/Same:Decline/Unsuitable"),
    table.attr = 'class="table table-hover"') %>%
    add_header_above(c(" " = 2, "Raw Votes" = 1, "Historic" = 2, 
                       "Projected Feasibility" = 2, "Trend" = 1)) %>%
    column_spec(4:8,bold = T, extra_css = "vertical-align:middle;") %>%
    kable_styling(full_width = F, font_size = 16) ##14
}
uData$cciss_results_dt <- cciss_results_dt
