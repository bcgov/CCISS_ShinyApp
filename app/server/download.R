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
    uData$feasibility_filter <- input$report_filter_feas
    uData$pool <- pool
    withProgress(min = 0, max = 4, value = 1, message = "Processing report", {

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
      
      incProgress(1, message = "Generate document")
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      if (input$report_format == "html") {
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        incProgress(2, message = "Done")
      } else if (input$report_format == "pdf") {
        has_chrome <- tryCatch({
          pagedown::find_chrome()
          TRUE
        }, error = function(e) {FALSE})
        if (has_chrome) {
          htmlreport <- rmarkdown::render(
            tempReport, params = params, envir = new.env(parent = globalenv()))
          incProgress(1, message = "Print document to PDF")
          pagedown::chrome_print(input = htmlreport, output = file, timeout = 120,
                                 extra_args = c("--disable-gpu", "--no-sandbox"))
          incProgress(1, message = "Done")
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
  filename = function() {
    paste("cciss_export", "zip", sep = ".")
  },
  content = function(file) {
    if (input$data_format == "rds") {
      # rds only returns R serialized data
      uData$site_series_filter <- input$report_filter
      saveRDS(uData, "cciss_export.rds")
      zip(file,c("cciss_export.rds"))
    } else if (input$data_format == "csv") {
      #browser()
      dat_export <- uData$cciss_results[, -c("PredFeasSVG")]
      if(is.null(uData$bgc_select)){
        id_vals <- uData$pts[,.(ID,Site)]
        dat_export[id_vals, ID := i.ID, on = c(SiteRef = "Site")]
        setcolorder(dat_export,"ID")
      }
      fwrite(dat_export, "cciss_export.csv")
      zip(file,c("cciss_export.csv","cciss_metadata.csv","README.txt"))
    }
  }
)