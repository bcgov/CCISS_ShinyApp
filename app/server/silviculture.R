observeEvent(input$siteref_silv, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_silv
  siteseries <- sort(
    unique(
      c(uData$cciss_results[SiteRef == siteref]$`Site Series`,
        uData$cciss_summary[SiteRef == siteref]$`Site Series`
      )
    )
  )
  updateSelectInput(inputId = "site_series_silv", choices = siteseries)
})

output$silviculture_block <- renderUI({
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  cciss_results <- uData$cciss_results
  if (is.null(cciss_results)) return(NULL)
  standardblocks(siteref, siteserie, cciss_results)
})

# Ref template for other tables
silv_ref_dt <- function(dt) {
  DT::datatable(dt, rownames = FALSE, width = "100%", height = "auto", 
                options = list(
                  scrollCollapse = FALSE, lengthChange = FALSE, searching = FALSE,
                  pageLength = nrow(dt), columnDefs = list(
                    list(className = 'dt-center', targets = (seq_len(ncol(dt))-1)[-3])
                  )
                )
  )
}

output$silvics_tol_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  cciss_results <- cciss_results[SiteRef == siteref & `Site Series` %in% siteserie]
  if (silv_filter == "f") {
    dt <- silvics_tol[`Tree Code` %in% cciss_results[`Projected Feasibility` %chin% c("1", "2", "3"), Spp]]
  } else {
    dt <- silvics_tol
  }
  silv_ref_dt(dt)
})

output$silvics_resist_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  cciss_results <- cciss_results[SiteRef == siteref & `Site Series` %in% siteserie]
  if (silv_filter == "f") {
    dt <- silvics_resist[`Tree Code` %in% cciss_results[`Projected Feasibility` %chin% c("1", "2", "3"), Spp]]
  } else {
    dt <- silvics_resist
  }
  silv_ref_dt(dt)
})

output$silvics_regen_dt <- DT::renderDT({
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  cciss_results <- uData$cciss_results
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  cciss_results <- cciss_results[SiteRef == siteref & `Site Series` %in% siteserie]
  if (silv_filter == "f") {
    dt <- silvics_regen[`Tree Code` %in% cciss_results[`Projected Feasibility` %chin% c("1", "2", "3"), Spp]]
  } else {
    dt <- silvics_regen
  }
  silv_ref_dt(dt)
})

output$silvics_mature_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  cciss_results <- cciss_results[SiteRef == siteref & `Site Series` %in% siteserie]
  if (silv_filter == "f") {
    dt <- silvics_mature[`Tree Code` %in% cciss_results[`Projected Feasibility` %chin% c("1", "2", "3"), Spp]]
  } else {
    dt <- silvics_mature
  }
  silv_ref_dt(dt)
})
