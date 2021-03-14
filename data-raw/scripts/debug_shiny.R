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
  actionButton("add_button", "Add", icon("plus")),
  actionButton("delete_button", "Delete", icon("trash-alt")),
  actionButton("upload_button", "Upload", icon("upload")),
  actionButton("clear_button", "Clear", icon("broom")),
  DT::DTOutput("points_table", width="100%"),
  selectInput("rcp_scenario", "RCP Scenario:", selected = "rcp45", c("4.5" = "rcp45", "8.5" = "rcp85"), multiple = TRUE, width = 150),
  radioButtons("aggregation", "Multiple Point Aggregation:", c("Individual" = "FALSE", "Averaged by BGC Zone" = "TRUE"), inline = TRUE),
  leafletOutput("bec_map"),
  uiOutput("ss_site_ref_select"),
  uiOutput("ss_legend"),
  uiOutput("species_suitability_summary"),
  uiOutput("species_suitability_detailed"),
  uiOutput("bgc_site_ref_select"),
  uiOutput("bgc_futures"),
  plotly::plotlyOutput("bgc_futures_plot"),
  uiOutput("downloadUI")
)

server <- function(input, output, session) {
  source("./server/common.R", local = TRUE)
  source("./server/points.R", local = TRUE)
  source("./server/map.R", local = TRUE)
  source("./server/suitability.R", local = TRUE)
  source("./server/futures.R", local = TRUE)
  source("./server/download.R", local = TRUE)
  
  onStop(function() {
    poolClose(pool)
    setwd(curr_wd)
  })
}

shinyApp(ui, server)
