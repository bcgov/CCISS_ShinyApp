# See generate.R file for `report_filter`  filter update

# Tells the app to hide the download button on load. It will show once points are added
session$sendCustomMessage(type="jsCode", list(code= "$('#download_report_span').hide()"))
session$sendCustomMessage(type="jsCode", list(code= "$('#download_data_span').hide()"))

observeEvent(input$report_filter_all,{
  updateCheckboxGroupInput(inputId = "report_filter", selected = uData$siteseries_all)
})

observeEvent(input$report_filter_none,{
  updateCheckboxGroupInput(inputId = "report_filter",  selected = character())
})

output$report_download <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function() {
    paste(input$report_name, input$report_format, sep = ".")
  },
  content = function(file) {
    uData$site_series_filter <- input$report_filter
    withProgress(min = 0, max = 2, value = 1, message = "Processing report", {

      if (input$report_format == "html") {
        reportmd <- "reporthtml.Rmd"
      } else if (input$report_format == "pdf") {
        reportmd <- "reportpdf.Rmd"
      } else {
        # Non known format return NULL
        return(NULL)
      }
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), reportmd)
      file.copy(file.path("./server", reportmd), tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      
      params <- list(userdata = uData)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      if (input$report_format == "html") {
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      } else if (input$report_format == "pdf") {
        has_chrome <- tryCatch({
          pagedown::find_chrome()
          TRUE
        }, error = function(e) {FALSE})
        if (has_chrome) {
          htmlreport <- rmarkdown::render(
            tempReport, params = params, envir = new.env(parent = globalenv()))
          pagedown::chrome_print(input = htmlreport, output = file, timeout = 120)
        } else {
          showModal(
            modalDialog(
              title = "PDF Rendering",
              paste("Chromium could not be found on this host."),
              easyClose = TRUE
            )
          )
        }
      }
    })
  }
)

output$data_download <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = function() {
    paste("cciss_export", input$data_format, sep = ".")
  },
  content = function(file) {
    if (input$data_format == "rds") {
      # rds only returns R serialized data
      uData$site_series_filter <- input$report_filter
      saveRDS(uData, file)
    } else if (input$data_format == "csv") {
      fwrite(uData$cciss_results[, -c("PredFeasSVG", "MidRotTrend")], file)
    }
  }
)