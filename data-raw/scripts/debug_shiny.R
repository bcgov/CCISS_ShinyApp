# Basic shiny app to debug components by using browser() and debug() statements
# without rmarkdown and styling overhead

library(shiny)

suppressPackageStartupMessages({
  library(bccciss)
  library(shiny)
  library(leaflet)
  library(DT)
  library(RPostgres)
  library(plotly)
  library(pool)
})
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
bcgov_tileserver <- Sys.getenv("BCGOV_TILESERVER")
bcgov_tilelayer <- Sys.getenv("BCGOV_TILELAYER")
curr_wd <- getwd()
setwd("./app/")

ui <- fixedPage(
  # Actions on points
  actionButton("add_button", "Add", icon("plus")),
  actionButton("delete_button", "Delete", icon("trash-alt")),
  actionButton("upload_button", "Upload", icon("upload")),
  actionButton("clear_button", "Clear", icon("broom")),
  
  # Points
  DT::DTOutput("points_table", width="100%"),
  actionButton("generate_results", label = "Generate results", icon = icon("plus-square")),
  
  
  # Control results
  splitLayout(
    checkboxGroupInput(
      "rcp_scenario",
      "RCP Scenario:",
      selected = "rcp45",
      c("4.5 W/m2" = "rcp45", "8.5 W/m2" = "rcp85")
    ),
    radioButtons(
      "aggregation",
      "Multiple Point Aggregation:",
      c("Individual" = "FALSE", "Averaged by BGC Zone" = "TRUE")
    )
  ),
  
  leafletOutput("bec_map"),
  
  selectInput("siteref_feas", label = "Sites:", choices = character()),
  selectInput("site_series_feas", label = "Site Series", choices = character()),
  span("Averaged:"),
  textOutput("avg_feas", inline = TRUE),
  span("RCP Scenario:"),
  textOutput("rcp_feas", inline = TRUE),
  span("Legend"),
  HTML(
    paste0(
      '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
      c("limegreen", "deepskyblue", "gold", "grey"),
      '"><span>&nbsp;',
      c("Primary", "Secondary", "Tertiary", "Not Suitable"),
      '</span>',
      collapse = "<br />"
    )
  ),
  
  DT::DTOutput("results_feas"),
  DT::DTOutput("summary_feas"),
  
  selectInput("siteref_bgc_fut", label = "Sites:", choices = character()),
  textOutput("current_bgc_fut", inline = TRUE),
  plotly::plotlyOutput("bgc_fut_plot"),

  textInput("report_name", "Name", value = "report"),
  selectInput("report_format", "Format", c("html", "pdf")),
  span(downloadButton("report_download", "Download", style = "max-width: 300px; width:100%; height: 40px !important;"), id = "download_span"),
  tags$script(HTML('
  Shiny.addCustomMessageHandler("jsCode",
    function(message) {
      console.log(message)
      eval(message.code);
    }
  );'
  ))
)

server <- function(input, output, session) {
  source("./server/common.R", local = TRUE)
  source("./server/generate.R", local = TRUE)
  source("./server/points.R", local = TRUE)
  source("./server/map.R", local = TRUE)
  source("./server/feasibility.R", local = TRUE)
  source("./server/futures.R", local = TRUE)
  source("./server/download.R", local = TRUE)
  
  onStop(function() {
    poolClose(pool)
    setwd(curr_wd)
  })
}

shinyApp(ui, server)
