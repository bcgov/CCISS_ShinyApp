output$downloadUIfilter <- renderUI({
  withProgress({
    cciss_summary <- cciss_summary()
    cciss_raw <- cciss_raw()
    if (is.null(cciss_raw) || is.null(cciss_summary)) {
      return(NULL)
    }
    incProgress(1)
    siteseries <- sort(unique(c(cciss_raw$`Site Series`, cciss_summary$`Site Series`)))
    selectInput(
      "report_site_series_filter",
      label = "Filter Site Series (default all)",
      choices = siteseries,
      multiple = TRUE,
      selected = siteseries
    ) 
  }, min = 0, max = 2, value = 1, message = "Processing", detail = "...")
})

output$downloadUI <- renderUI({
  withProgress({
    bgc <- bgc_react()
    if (is.null(bgc)) {
      return(span("Add points to generate report."))
    }
    style <- "max-width: 300px; width:100%; height: 40px !important;"
    cciss_summary <- cciss_summary()
    cciss_raw <- cciss_raw()
    incProgress(1)
    siteseries <- sort(unique(c(cciss_raw$`Site Series`, cciss_summary$`Site Series`)))
    list(
      textInput("report_name", "Name", value = "report"),
      selectInput("report_format", "Format", c("html", "pdf")),
      downloadButton("report_download", "Download", icon = icon("download"), style = style)
    ) 
  }, min = 0, max = 2, value = 1, message = "Processing", detail = "...")
})

output$report_download <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function() {
    paste(input$report_name, input$report_format, sep = ".")
  },
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("./server/report.Rmd", tempReport, overwrite = TRUE)
    # Set up parameters to pass to Rmd document
    params <- list(points = uData$points)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      output_format = paste(input$report_format, "document", sep = "_"),
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)