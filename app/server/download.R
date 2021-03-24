# See generate.R file for `report_filter`  filter update

# Tells the app to hide the download button on load. It will show once points are added
session$sendCustomMessage(type="jsCode", list(code= "$('#download_span').hide()"))

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
    
    if (input$report_format == "rds") {
      # rds only returns R serialized data
      saveRDS(uData, file)
      
    } else {
      
      withProgress(min = 0, max = 2, value = 1, message = "Processing report", {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("./server/report.Rmd", tempReport, overwrite = TRUE)
        file.copy("./server/www", tempReport, recursive = TRUE, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        
        params <- list(userdata = uData)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
      
    }
  }
)