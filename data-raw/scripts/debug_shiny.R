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
if (is.null(bcgov_tileserver) || is.null(bcgov_tilelayer)) stop("tileserver env not set")
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

curr_wd <- getwd()
setwd("./app/")

dl_style <- "max-width: 300px; width:100%; height: 40px !important;"

ui <- fixedPage(
  # Actions on points
  actionButton("upload_button", "Upload", icon("upload"), width=120),
  actionButton("add_dialog", "Add", icon("plus"), width=120),
  actionButton("delete_button", "Delete", icon("trash-alt"), width=120),
  actionButton("clear_button", "Clear", icon("broom"), width=120),
  
  # Points
  DT::DTOutput("points_table", width="100%"),
  
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
  
  # Generate results
  actionButton("generate_results", label = "Generate results", icon = icon("plus-square"),
               style = "width:100%; background-color:#003366; color: #FFF"),
  
  
  leafletOutput("bec_map"),
  
  selectInput("siteref_feas", label = "Sites:", choices = character()),
  selectInput("site_series_feas", label = "Site Series", choices = character()),
  selectInput("filter_feas", label = "Feasibility", choices = c("All" = "a", "Feasible Only" = "f")),
  
  tableOutput("results_feas"),
  tableOutput("summary_feas"),
  
  selectInput("siteref_silv", label = "Sites:", choices = character()),
  selectInput("site_series_silv", label = "Site Series", choices = character()),
  selectInput("filter_silv", label = "Tree Species", choices = c("Feasible Species" = "f", "All Species" = "a")),
  
  uiOutput("silviculture_block"),
  tableOutput("silvics_tol_dt"),
  
  tableOutput("silvics_resist_dt"),
  tableOutput("silvics_regen_dt"),
  tableOutput("silvics_mature_dt"),
  
  selectInput("siteref_bgc_fut", label = "Sites:", choices = character()),
  
  plotly::plotlyOutput("bgc_fut_plot"),
  
  actionButton("report_filter_all", label = "Check All", icon = icon("check")),
  actionButton("report_filter_none", label = "Uncheck All", icon = icon("ban")),
  div(
    checkboxGroupInput("report_filter", label = "Site Series", choices = character(), width = 400),
    style = "line-height: 1.5; color: #222; background-color: #fff; margin: 10px 0px 10px 0px; padding: 0px 10px 0px 10px"
  ),
  
  splitLayout(
    div(
      textInput("report_name", "Name", value = "report"),
      radioButtons("report_format", "Format", c("html", "pdf"), inline = TRUE),
      span(downloadButton("report_download", "Download", style = dl_style), id = "download_report_span")
    ),
    div(
      radioButtons("data_format", "Data Format", c("csv", "rds"), inline = TRUE),
      span(downloadButton("data_download", "Download Data", style = dl_style), id = "download_data_span")
    ) 
  ),
  div(
    tableOutput("modelsinfo"),
    plotly::plotlyOutput("timings", width = "100%")
  ),
  tableOutput("shinyinfo"),
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
  # bslib::bs_themer()
  # Reusing Shiny session userData environment
  uData <- session$userData
  source("./server/generate.R", local = TRUE)
  source("./server/points.R", local = TRUE)
  source("./server/map.R", local = TRUE)
  source("./server/feasibility.R", local = TRUE)
  source("./server/silviculture.R", local = TRUE)
  source("./server/futures.R", local = TRUE)
  source("./server/download.R", local = TRUE)
  output$shinyinfo <- function() {
    app_info <- data.table(
      Information = c("Shiny host","Database host","Tiles source","Tileserver","Github","Copyright","License"),
      Value = c(system("hostname", intern = TRUE), Sys.getenv("BCGOV_HOST"), Sys.getenv("BCGOV_TILESERVER"),
                strsplit(Sys.getenv("BCGOV_TILESERVER"), split = "data")[[1]][1],
                as.character(tags$a(href = "https://github.com/bcgov/CCISS_ShinyApp", "bcgov/CCISS_ShinyApp")),
                paste(format(Sys.Date(), "%Y"), "Province of British Columbia"),
                as.character(tags$a(href = "http://www.apache.org/licenses/LICENSE-2.0", "Apache 2.0 LICENSE")))
    )
    knitr::kable(app_info, format = "html", table.attr = 'class="table table-hover table-centered"', escape = FALSE)
  }
  
  onStop(function() {
    poolClose(pool)
    setwd(curr_wd)
  })
}

shinyApp(ui, server)
