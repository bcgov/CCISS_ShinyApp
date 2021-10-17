# CCISS UI
navbarPage(
  title = "CCISS",
  theme = bslib::bs_theme(
    version = 4,
    bootswatch = "yeti",
    primary = "#003366"
  ),
  windowTitle = "Climate Change Informed Species Selection Tool",
  # Select sites ----
  tabPanel(
    title = "SELECT SITES", value = "sites", icon = icon("map"),
    sidebarLayout(
      # Inputs
      sidebarPanel(
        wellPanel(
          splitLayout(
            actionButton("sesh_params","Adjust Parameters",icon = icon("sliders-h")),
            
            radioButtons(
              "aggregation",
              "Multiple Point Aggregation:",
              c("Individual" = "FALSE", "Averaged by BGC Subzone" = "TRUE"),
              selected = "TRUE"
            )
          ),
          style = "padding: 5px 5px 5px 5px; margin:0%"
        ),
        hr(style = "border-top: 1px solid #8f0e7e;"),
        h5("Add Points Using One of the 3 Methods Below"),
        h6("1. Enter Lat/Long or Click on Map to Add Points"),
        p("Click on the map to add points or use 'Enter New' to manually add a specific lat/long coordinate"),
        # Points
        DT::DTOutput("points_table", width="100%"),
        actionButton("add_dialog", "Enter New", icon("plus"), width=120),
        actionButton("delete_button", "Selected", icon("trash-alt"), width=120),
        actionButton("clear_button", "Clear All", icon("broom"), width=120),
        hr(style = "border-top: 1px solid #8f0e7e;"),
        h6("2. Generate summary using preselected points by BGC or BGC+Distict:"),
        tags$p("Click BGC on map to use points across an entire BGC subzone/variant or only BGCs within a Forest District"),
        splitLayout(
          radioButtons(
            "preselected",
            label = NULL,
            choiceNames =  c("None", "BGC","District then BGC"),
            choiceValues = c("N","BGC","BGC_Dist"),
            inline = T
          ),
          actionButton("clear_highlight","Clear Selections"),
          cellWidths = c("67%", "33%")
        ),
        textOutput("bgc_click_show"),
        textOutput("dist_click_show"),
        hr(style = "border-top: 1px solid #8f0e7e;"),
        h6("3. Upload a CSV file contain points of interests"),
        tags$p("Upload a csv file with columns named ID1, Latitude, and Longitude (negative values) with your points of interest."),
        # Actions on points
        actionButton("upload_button", "Upload CSV", icon("upload"), 
                     style = "width:100%; background-color:#8f0e7e; color: #FFF"),
        hr(style = "border-top: 1px solid #8f0e7e;"),
        actionButton("clear_selections","Clear Selections",
                     style = "width:100%; background-color:#c21104; color: #FFF"),
        hr(style = "border-top: 1px solid #8f0e7e;"),
        actionButton("generate_results", label = "Generate results", icon = icon("plus-square"),
                     style = "width:100%; background-color:#003366; color: #FFF"),
        h5("Ready to Go!")
      ),
      mainPanel(
        # Biogeoclimatic Zones + Subzones Variants Map
        leafletOutput("bec_map")
      )
    )
  ),
  # Feasibility report ----
  tabPanel(
    title = "FEASIBILITY REPORT", value = "feasibility", icon = icon("leaf"),
    sidebarLayout(
      # Inputs
      sidebarPanel(
        h6("Filters"),
        selectInput("siteref_feas", label = "Sites:", choices = character()),
        selectInput("site_series_feas", label = "Site Series", choices = character()),
        selectInput("filter_feas", label = "Feasibility", choices = c("All" = "a", "Feasible Only" = "f")),
        h6("Legend"),
        HTML(
          paste0(
            '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
            c("limegreen", "deepskyblue", "gold", "grey"),
            '" /><span style="vertical-align:middle">&nbsp;',
            c("Primary", "Secondary", "Tertiary", "Not Suitable"),
            '</span>',
            collapse = "<br />"
          )
        )
      ),
      mainPanel(
        # CCISS Report
        column(12, align="center", tableOutput("results_feas"))
      )
    )
  ),
  # BEC Futures ----
  navbarMenu(
    title = "BEC FUTURES",
    tabPanel(
      title = "Graphical",
      sidebarLayout(
        # Inputs
        sidebarPanel(
          h6("Filter"),
          selectInput("siteref_bgc_fut", label = "Sites:", choices = character()),
          selectInput("ss_bgc_fut",label = "Site Series:", choices = character()),
          sliderInput("min_ssoverlap",label = "Min Site Series Overlap", min = 0.05,max = 0.25,value = 0.1)
        ),
        mainPanel(
          # BEC Futures: ratio of predicted BGC with aligned site series from all selected climate model/scenarios
          h3("Ratio of Model Predictions of future BGCs and aligned site series", textOutput("current_bgc_fut", inline = TRUE)),
          plotly::plotlyOutput("bgc_fut_plot", width = "100%", height = "auto")
        )
      )
    ),
    tabPanel(
      title = "Spatial",
      sidebarLayout(
        # Inputs
        sidebarPanel(
          h6("Filter"),
          selectInput("siteref_bgc_fut_spatial", label = "Sites:", choices = character()),
          selectInput(
            "bgc_spatial_period", label = "Period:",
            choices = c("Choose Period" = "",
                        "Current" = 1991,
                        "2021-2040" = 2021,
                        "2041-2060" = 2041,
                        "2061-2080" = 2061,
                        "2081-2100" = 2081)
            )
        ),
        mainPanel(
          # BEC Futures: Map of all predicted BGCs
          leafletOutput("wna_map", height = "auto")
        )
      )
    )
  ),
  # Silvics & Ecology ----
  tabPanel(
    title = "SILVICS & ECOLOGY",
    sidebarLayout(
      # Inputs
      sidebarPanel(
        h6("Filters"),
        selectInput("siteref_silv", label = "Sites:", choices = character()),
        selectInput("site_series_silv", label = "Site Series", choices = character()),
        selectInput(
          "filter_silv", label = "Tree Species",
          choices = c("Feasible Species" = "f", "All Species" = "a")
        )
      ),
      mainPanel(
        # Silvics & Ecology
        tabsetPanel(
          tabPanel(
            title = "Chief Forester Reference Guide",
            uiOutput("silviculture_block")
          ),
          tabPanel(
            title = "Tolerance",
            tableOutput("silvics_tol_dt")
          ),
          tabPanel(
            title = "Resistance",
            tableOutput("silvics_resist_dt")
          ),
          tabPanel(
            title = "Regeneration stage",
            tableOutput("silvics_regen_dt")
          ),
          tabPanel(
            title = "Maturing stage",
            tableOutput("silvics_mature_dt")
          )
        )
      )
    )
  ),
  # Species portfolio draft ----
  tabPanel(
    title = HTML("SPECIES PORTFOLIO <i>Draft</i>"),
    sidebarLayout(
      # Inputs
      sidebarPanel(
        h6("Data Options"),
        selectInput("port_bgc",label = "Select BGC:", choices = character()),
        radioButtons("port_ss",label = "Select Site Postion:",choices = c("B2","Zonal","D6"), selected = "Zonal"),
        {
          treeOpts <- c("Py","Fd","At","Pl","Sx","Bl","Cw","Hw","Pw","Ss","Lw","Ba","Hm","Dr","Mb")
          selectInput("tree_species",label = "Included Species:", choices = treeOpts, selected = treeOpts, multiple = T)
        },
        radioButtons("port_length", label = "Optimisation Period (Rotation Length):",
                     choiceNames = c("Current Period","20 Year","40 Year","60 Year","80 Year"),
                     choiceValues = c(1991,2021,2041,2061,2081),selected = 2081),
        # radioButtons(
        #     "fut_scn",
        #     "RCP Scenario:",
        #     selected = "ssp370",
        #     c("2.6 W/m2" = "ssp126", "4.5 W/m2" = "ssp245", "7.0 W/m2" = "ssp370", "8.5 W/m2" = "ssp585"),
        #     
        #   )
        h6("Portfolio Parameters"),
        rHandsontableOutput("setbounds"),
        sliderInput("return_level","Specified Return:", min = 0.5, max = 1,value = 0.9),
        sliderInput("min_accept","Minimum allowed weight:",min = 0.01,max = 0.2,value = 0.05),
        actionButton("generate_portfolio", label = "Run Portfolio", icon = icon("plus-square"),
                     style = "width:100%; background-color:#003366; color: #FFF")
      ),
      mainPanel(
        fluidRow(
          tabPanel(
            title = "Efficient Frontier",
            plotOutput("efficient_frontier", width = "100%", height = "auto")
          ),
          tabPanel(
            title = "Growth Simulations",
            plotOutput("growth_sim",  width = "100%", height = "auto")
          )
        ),
        fluidRow(
          tabPanel(
            title = "Optimised Weights",
            tableOutput("port_table")
          ),
          tabPanel(
            title = "Site Index and Feasibility",
            DTOutput("port_sssum")
          )
        )
      )
    )
  ),
  # Export ----
  tabPanel(
    title = "EXPORT",
    sidebarLayout(
      # Inputs
      sidebarPanel(
        h6("Filter"),
        selectInput("report_filter_feas", label = "Tree Species", choices = c("All" = "a", "Feasible Only" = "f")),
        actionButton("report_filter_all", label = "Check All", icon = icon("check")),
        actionButton("report_filter_none", label = "Uncheck All", icon = icon("ban")),
        div(
          checkboxGroupInput("report_filter", label = "Site Series", choices = character(), width = 400),
          style = "line-height: 1.5; color: #222; background-color: #fff; margin: 10px 0px 10px 0px; padding: 0px 10px 0px 10px"
        )
      ),
      # Export a Digital Report or Dataset for Analyzed Sites
      mainPanel(
        title = "Export a report on selected points",
        {
          dl_style <- "max-width: 300px; width:100%; height: 40px !important;"
          splitLayout(
            div(
              textInput("report_name", "Name for Report", value = "report"),
              radioButtons("report_format", "Report Format", c("html","pdf"), inline = TRUE),
              span(downloadButton("report_download", "Produce Report", style = dl_style), id = "download_report_span")
            ),
            div(
              radioButtons("data_format", "Data Format", c("csv", "rds"), inline = TRUE),
              span(downloadButton("data_download", "Download Data", style = dl_style), id = "download_data_span")
            ) 
          )
        }
      )
    )
  ),
  # Tech specs ----
  navbarMenu(
    title = "TECH SPECS",
    tabPanel(
      title = "How the CCISS tool works",
      tabPanel(
        title = "How the CCISS tool works",
        tags$p("The Climate Change Informed Species Selection Tool provides information on future tree species suitability in British Columbia. It combines future climate information with species viability models to illustrate how likely each species is to thrive in the range of potential futures."),
        tags$p("The CCISS tool reassesses the suitability ranks of species at a site series level under multiple plausible modelled future climates. Understanding climate- and site-level species suitability is one of the foundational pieces of information that a forester requires for the creation of successful silvicultural prescriptions over a rotation. The CCISS tool looks at near- and mid-term projected changes to BGC climates and the implications to species suitability. The tool then aligns the projected future suitability rank of species at a POI with the suitability in the default stocking standards outlined in the Chief Forester’s Reference Guide to highlight where there are predicted climate change induced shifts in species suitability. This information can be used to inform planting/ silvicultural prescription outlined in climate change informed stocking standard. The CCISS tool is spatial explicit to account for the gradient of climate change that will different regions and elevations of a BGC."),
        tags$p(tags$a(href = "https://www.for.gov.bc.ca/ftp/HRE/external/!publish/CCISS/CCISS_in_Stocking%20Standards.pdf", "Would you like to know more?"))  
      )
    ),
    tabPanel(
      title = "Model information",
      tabPanel(
        title = "Current versions of Information Tables, Maps, and Models used in this App",
        div(
          tableOutput("modelsinfo"),
          plotly::plotlyOutput("timings", width = "100%")
        )
      )
    ),
    tabPanel(
      title = "Shiny App Information",
      tabPanel(
        title = "Shiny App Information",
        tableOutput("shinyinfo")
      )
    )
  ),
  # Instructions ----
  tabPanel(
    title = "INSTRUCTIONS",
    tabPanel(
      title = "Instructions",
      h5("What is CCISS?"),
      tags$p("Here's some pithy text about CCISS."),
      h5("How do I use the tool?"),
      tags$p("More pithy and instructive text.")
    ),
    tags$script(
      # ?Quotes - raw character constant
      r"(
      
      $("body").on("shown.bs.tab", "a[data-toggle='tab']", function(e) {
      Shiny.setInputValue("active_tab", $(e.target).parent().index());
      })
      
      )"     
    ),
    tags$script(
      r"(
      
      Shiny.addCustomMessageHandler("jsCode",
      function(message) {
      console.log(message)
      eval(message.code);
      }
      );
      
      )"
    )
  )
)