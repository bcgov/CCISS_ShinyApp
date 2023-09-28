# CCISS UI
tooltipsIcon <- icon("question-circle")
# Use regular style instead of solid
tooltipsIcon$attribs$class <- gsub("fa ", "far ", tooltipsIcon$attribs$class, fixed = TRUE)
# Wrap in a span to be able to use prompter
tooltipsIcon <- span(tooltipsIcon)

navhelplink <- function(title, inputId) {
  HTML(
    paste0(
      title,
      '</a><a id="',
      inputId,
      '" class="action-button shiny-bound-input" href="#" style="padding-left:0 !important; color: white !important"><sup><i class="fa fa-question-circle" role="presentation" aria-label="question-circle icon"></i></sup>'
    )
  )
}

sidebarhelplink <- function(inputId) {
  tags$p(style = "text-align: center;", shiny::actionLink(
    inputId = inputId,
    label = "Section Instructions", 
    icon = icon("question-circle")
  ))
}


  navbarPage(
    title = HTML('&nbsp;&nbsp;<img src="logo.svg" class="navbar-logo">',navhelplink("The CCISS Tool", "cciss_about_nav")), ##navhelplink("The CCISS Tool", "cciss_about_nav")
    theme = {
      theme <- bslib::bs_theme(version = 5,
                               bootswatch = "sandstone",
                               primary = "#003366")
      # theme$layers$bootswatch$defaults[[3]][[2]] <-
      #   "$navbar-default-bg: primary !default;"
      theme
    },
    collapsible = TRUE,
    windowTitle = "Climate Change Informed Species Selection Tool",
    id = "cciss_navbar",
    # Select sites ----
    tabPanel(
      title = navhelplink("SELECT SITES", "cciss_instructions_select_sites_nav"),
      value = "sites",
      class = "tabcontainer",
      tags$head(includeCSS("./www/style.css")),
      prompter::use_prompt(),
      tags$head(includeScript("./www/cciss.js")),
#       tags$script(
#         "function deleteRow(el){
# 				$('#points_table')
# 					.data('datatable')
# 					.row($(el).parents('tr'))
# 					.remove()
# 					.draw();
# 			};"
#       ),
      sidebarLayout(
        # Inputs
        sidebarPanel(
          width = 4,
          sidebarhelplink("cciss_instructions_select_sites"),
          style = "padding: 5px 5px 5px 5px; margin:0%; overflow-y:scroll; max-height: 90vh; position:relative; align: centre",

          wellPanel(
            splitLayout(
              actionButton("clear_selections", "Clear Selections",
                           style = "width:100%; height:70px; background-color:#c21104; color: #FFF"),
              actionButton(
                "generate_results",
                label = "Generate results",
                icon = icon("plus-square"),
                style = "width:100%; height:70px; background-color:#003366; color: #FFF"
              )
            ),
            splitLayout(
              # radioButtons(
              #   "aggregation",
              #   "Report Type",
              #   c("Indiv Points" = "FALSE", "BGC avg" = "TRUE"),
              #   inline = TRUE,
              #   selected = "TRUE"
              # ),
              tagList(
                br(),
                actionButton("sesh_params", "Model Parameters", icon = icon("sliders-h"), style = "width:100%; align:center;")
              ),
              tagList(
                br(),
               # p("Report by:"),
                switchInput("aggregation", value = TRUE, onLabel = "Report averaged by BGC    ", offLabel = "Report by individual sites", width = '100%')
              )
              
            )

          ),

          hr(style = "border-top: 1px solid #8f0e7e;"),
          h4("Add Sites Using One of the 3 Methods Below"),
          
          accordion(
            multiple = FALSE,
            accordion_panel(
              title = h5(
                "Method 1. Click on map to add single points",
                prompter::add_prompt(
                  tooltipsIcon,
                  message = "Click on the map to add one or more points or use 'Enter New' to manually add lat/long coordinates",
                  position = "top",
                  size = "large",
                  shadow = FALSE
                )
              ),
              DT::DTOutput("points_table", width = "100%"),
              actionButton("add_dialog", "Enter New", icon("plus"), width =
                             120),
              actionButton("delete_button", "Selected", icon("trash-alt"), width =
                             120),
              value = "acc1"
              # actionButton("clear_button", "Clear All", icon("broom"), width =
              #                120),
             
            ), 
            accordion_panel(
              title = h5(
                "Method 2. Click on BGC and District",
                prompter::add_prompt(
                  tooltipsIcon,
                  message = "Click on map to use preselected points across an entire BGC subzone/variant or only BGCs within a Forest District",
                  position = "top",
                  size = "large",
                  shadow = FALSE
                )
              ),
              value = "acc2",
                radioButtons(
                  "preselected",
                  label = NULL,
                  choiceNames =  c("All of BGC", "BGC in District"),
                  choiceValues = c("BGC", "BGC_Dist"),
                  inline = T
                ),
                
                textOutput("bgc_click_show"),
                textOutput("dist_click_show"),
                hr(style = "border-top: 1px solid #8f0e7e;")
            ),
            accordion_panel(
              title = h5(
                "Method 3. Upload a CSV file",
                prompter::add_prompt(
                  tooltipsIcon,
                  message = "Upload a csv file with columns sitename, latitude, longitude, and (optionally) siteseries with your points of interest.",
                  position = "top",
                  size = "large",
                  shadow = FALSE
                )
              ),
              value = "acc3",
                actionButton("upload_button", "Upload CSV", icon("upload"),
                             style = "width:100%; background-color:#8f0e7e; color: #FFF"),
                hr(style = "border-top: 1px solid #8f0e7e;")
            ),
          id = "acc"),
          br(),
          hr(style = "border-top: 1px solid #8f0e7e;"),
          #wellPanel(
            splitLayout(
              selectInput("findbec","Find-a-BEC", 
                          choices = c("(N)",bgc_choices), 
                          multiple = F),
              tagList(br(),
                      actionButton("findbecclear","Clear")
              ),
              tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))
            ),
            br(),
            br()
          #)
          
          
          
        ),
       
        mainPanel(width = 8,
                  # Biogeoclimatic Zones + Subzones Variants Map
                  leafletOutput("bec_map"))
        
        )
      ),
    # Feasibility report ----
    tabPanel(
      title = navhelplink("FEASIBILITY REPORT", "cciss_instructions_feasibility_report_nav"),
      value = "feasibility",
      tags$style(type='text/css', ".selectize-input { font-size: 54px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
      sidebarLayout(
        # Inputs
        sidebarPanel(
          width = 2,
          sidebarhelplink("cciss_instructions_feasibility_report"),
          h5("Report Type"),
          switchInput("feas_type", value = TRUE, onLabel = "Detailed", offLabel = "Summary", width = '200%'),
          #materialSwitch("feas_type","Full Report", right = TRUE, status = "primary", value = TRUE),
          #h6("Filters"),
          selectInput("siteref_feas", label = "Choose Site/BGC", choices = character()),
          selectInput("site_series_feas", label = "Choose Site Series", choices = character()),
          radioButtons(
            "filter_feas",
            label = "Feasibility",
            choices = c("All" = "a", "Feasible Only" = "f"),
            selected = "a",
            inline = T
          ),
          h5("Feasibility Legend"),
          bslib::tooltip(
            span(HTML(
              paste0(
                '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
                c("limegreen", "deepskyblue", "gold", "grey"),
                '" /><span style="vertical-align:middle">&nbsp;',
                c("E1: High", "E2: Moderate", "E3: Low", "X: Not Suitable"),
                '</span>',
                collapse = "<br />"
              )
            )
            ),
            tooltip_text$feas_legend
          )
          ,
          plotOutput("edaplot")
        ),
        mainPanel(width = 10,
                  # CCISS Report
                  uiOutput("results_feas_all")
                  )
      )
    ),
    # BEC Futures ----
    navbarMenu(
      title = navhelplink("BEC FUTURES", "cciss_instructions_bec_futures_nav"),
      tabPanel(title = "Chart",
               sidebarLayout(
                 # Inputs
                 sidebarPanel(
                   width = 2,
                   sidebarhelplink("cciss_instructions_bec_futures"),
                   h6("Filter"),
                   selectInput("siteref_bgc_fut", label = "Sites:", choices = character()),
                   selectInput("ss_bgc_fut", label = "Site Series:", choices = character()),
                   sliderInput(
                     "min_ssoverlap",
                     label = "Min Site Series Overlap",
                     min = 0.05,
                     max = 0.25,
                     value = 0.1
                   )
                 ),
                 mainPanel(
                   width = 10,
                   # BEC Futures: ratio of predicted BGC with aligned site series from all selected climate model/scenarios
                   h4(
                     "Ratio of Model Predictions of future BGCs and aligned site series",
                     textOutput("current_bgc_fut", inline = TRUE)
                   ),
                   plotly::plotlyOutput("bgc_fut_plot")
                 )
               )),
      tabPanel(title = "Map",
               sidebarLayout(
                 # Inputs
                 sidebarPanel(
                   width = 2,
                   sidebarhelplink("cciss_instructions_bec_futures_spatial"),
                   h6("Filter"),
                   selectInput(
                     "siteref_bgc_fut_spatial",
                     label = "Sites:",
                     choices = character()
                   ),
                   selectInput(
                     "bgc_spatial_period",
                     label = "Period:",
                     choices = c(
                       "Choose Period" = "",
                       "Current" = 1991,
                       "2021-2040" = 2021,
                       "2041-2060" = 2041,
                       "2061-2080" = 2061,
                       "2081-2100" = 2081
                     )
                   )
                 ),
                 mainPanel(width = 10,
                           # BEC Futures: Map of all predicted BGCs
                           leafletOutput("wna_map"))
               ))
    ),
    # Silvics & Ecology ----
    tabPanel(title = navhelplink("SILVICS & ECOLOGY", "cciss_instructions_silvics_ecology_nav"),
             sidebarLayout(
               # Inputs
               sidebarPanel(
                 width = 2,
                 sidebarhelplink("cciss_instructions_silvics_ecology"),
                 h6("Filters"),
                 selectInput("siteref_silv", label = "Sites:", choices = character()),
                 selectInput("site_series_silv", label = "Site Series", choices = character()),
                 selectInput(
                   "filter_silv",
                   label = "Tree Species",
                   choices = c("Feasible Species" = "f", "All Species" = "a")
                 ),
                 h6("Legend"),
                 HTML(
                   paste0(
                     '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
                     c("green", "red", "purple"),
                     '" /><span style="vertical-align:middle">&nbsp;',
                     c("Improving", "Decreasing", "Adding"),
                     '</span>',
                     collapse = "<br />"
                   )
                 )
               ),
               mainPanel(
                 width = 10,
                 # Silvics & Ecology
                 tabsetPanel(
                   type = "pills",
                   # tabPanel(title = "Chief Forester Reference Guide",
                   #          #uiOutput("silviculture_block")),
                   tabPanel(title = "Tolerance",
                            tableOutput("silvics_tol_dt")),
                   tabPanel(title = "Resistance",
                            tableOutput("silvics_resist_dt")),
                   tabPanel(title = "Regeneration stage",
                            tableOutput("silvics_regen_dt")),
                   tabPanel(title = "Maturing stage",
                            tableOutput("silvics_mature_dt"))
                 )
               )
             )),
    # Species portfolio draft ----
    # tabPanel(
    #   title = navhelplink("SPECIES PORTFOLIO<i><sup>Draft</sup></i>", "cciss_instructions_species_portfolio_nav"),
    #   sidebarLayout(
    #     # Inputs
    #     sidebarPanel(
    #       width = 3,
    #       sidebarhelplink("cciss_instructions_species_portfolio"),
    #       h6("Data Options"),
    #       selectInput("port_bgc", label = "Select BGC:", choices = character()),
    #       radioButtons(
    #         "port_ss",
    #         label = "Select Site Postion:",
    #         choices = c("B2", "Zonal", "D6"),
    #         selected = "Zonal"
    #       ),
    #       {
    #         treeOpts <-
    #           c("Py",
    #             "Fd",
    #             "At",
    #             "Pl",
    #             "Sx",
    #             "Bl",
    #             "Cw",
    #             "Hw",
    #             "Pw",
    #             "Ss",
    #             "Lw",
    #             "Ba",
    #             "Hm",
    #             "Dr",
    #             "Mb")
    #         selectInput(
    #           "tree_species",
    #           label = "Included Species:",
    #           choices = treeOpts,
    #           selected = treeOpts,
    #           multiple = T
    #         )
    #       },
    #       radioButtons(
    #         "port_length",
    #         label = "Optimisation Period (Rotation Length):",
    #         choiceNames = c("Current Period", "20 Year", "40 Year", "60 Year", "80 Year"),
    #         choiceValues = c(1991, 2021, 2041, 2061, 2081),
    #         selected = 2081
    #       ),
    #       # radioButtons(
    #       #     "fut_scn",
    #       #     "RCP Scenario:",
    #       #     selected = "ssp370",
    #       #     c("2.6 W/m2" = "ssp126", "4.5 W/m2" = "ssp245", "7.0 W/m2" = "ssp370", "8.5 W/m2" = "ssp585"),
    #       #
    #       #   )
    #       h6("Portfolio Parameters"),
    #       rHandsontableOutput("setbounds"),
    #       sliderInput(
    #         "return_level",
    #         "Specified Return:",
    #         min = 0.5,
    #         max = 1,
    #         value = 0.9
    #       ),
    #       sliderInput(
    #         "min_accept",
    #         "Minimum allowed weight:",
    #         min = 0.01,
    #         max = 0.2,
    #         value = 0.05
    #       ),
    #       actionButton(
    #         "generate_portfolio",
    #         label = "Run Portfolio",
    #         icon = icon("plus-square"),
    #         style = "width:100%; background-color:#003366; color: #FFF"
    #       )
    #     ),
    #     {
    #       div_style <-
    #         "border:1px solid var(--lt-color-gray-400); padding: 0.25rem; margin-bottom: 0.25rem; min-height: 400px;"
    #       p_style <-
    #         "border-bottom: 1px solid var(--lt-color-gray-400);"
    #       mainPanel(width = 9,
    #                 fluidRow(column(
    #                   width = 6,
    #                   div(
    #                     style = div_style,
    #                     p("Efficient Frontier", style = p_style),
    #                     plotOutput("efficient_frontier")
    #                   )
    #                 ),
    #                 column(
    #                   width = 6,
    #                   div(
    #                     style = div_style,
    #                     p("Growth Simulations", style = p_style),
    #                     plotOutput("growth_sim")
    #                   )
    #                 )),
    #                 fluidRow(column(
    #                   width = 6,
    #                   div(
    #                     style = div_style,
    #                     p("Optimised Weights", style = p_style),
    #                     tableOutput("port_table")
    #                   )
    #                 ),
    #                 column(
    #                   width = 6,
    #                   div(
    #                     style = div_style,
    #                     p("Site Index and Feasibility", style = p_style),
    #                     DTOutput("port_sssum")
    #                   )
    #                 )))
    #     }
    #   )
    # ),
    # Export ----
    tabPanel(
      title = navhelplink("Export", "cciss_instructions_export_nav"),
      sidebarLayout(
        # Inputs
        sidebarPanel(
          width = 2,
          sidebarhelplink("cciss_instructions_export"),
          h6("Filter"),
          selectInput(
            "report_filter_feas",
            label = "Tree Species",
            choices = c("All" = "a", "Feasible Only" = "f")
          ),
          actionButton(
            "report_filter_all",
            label = "Check All",
            icon = icon("check")
          ),
          actionButton(
            "report_filter_none",
            label = "Uncheck All",
            icon = icon("ban")
          ),
          div(
            checkboxGroupInput(
              "report_filter",
              label = "Site Series",
              choices = character(),
              width = 400
            ),
            style = "line-height: 1.5; color: #222; background-color: #fff; margin: 10px 0px 10px 0px; padding: 0px 10px 0px 10px"
          )
        ),
        # Export a Digital Report or Dataset for Analyzed Sites
        mainPanel(width = 10,
                  title = "Export a report on selected points",
                  {
                    dl_style <- "max-width: 300px; width:100%; height: 40px !important;"
                    div_style <- "padding: 1rem;"
                    p_style <-
                      "border-bottom: 1px solid var(--lt-color-gray-400);"
                    fluidRow(column(
                      width = 3,
                      div(
                        style = div_style,
                        p("Export Report", style = p_style),
                        textInput("report_name", "Name for Report", value = "report"),
                        radioButtons("report_format", "Report Format", c("html", "pdf"), inline = TRUE),
                        span(
                          downloadButton("report_download", "Produce Report", style = dl_style),
                          id = "download_report_span"
                        )
                      )
                    ),
                    column(width = 3,
                           div(
                             style = div_style,
                             p("Export Data", style = p_style),
                             radioButtons("data_format", "Data Format", c("csv", "rds"), inline = TRUE),
                             span(
                               downloadButton("data_download", "Download Data", style = dl_style),
                               id = "download_data_span"
                             )
                           )))
                  })
      )
    ),
    # Tech specs ----
    navbarMenu(
      title = "APP INFO",
      menuName = "cciss_help",
      tabPanel(
        title = "About",
        value = "cciss_about",
        fluidRow(
          column(
            width = 6,
            offset = 1,
            tabPanel(
              title = "",
              includeHTML("./instructions/About_CCISS.html") 
            )
          )
        )
      ),
      tabPanel(
        title = "Instructions",
        value = "cciss_instructions",
        fluidRow(
          column(
            width = 8,
            offset = 1,
            tags$h4("Instructions"),
            tabsetPanel(
              id = "cciss_instructions_set",
              type = "pills",
              tabPanel(
                title = "Select Sites",
                value = "cciss_instructions_select_sites",
                includeHTML("./instructions/SelectSites.html") 
              ),
              tabPanel(
                title = "Feasibility Report",
                value = "cciss_instructions_feasibility_report",
                includeHTML("./instructions/FeasibilityReport.html") 
              ),
              tabPanel(
                title = "BEC Futures",
                value = "cciss_instructions_bec_futures",
                includeHTML("./instructions/BECFutures.html") 
              ),
              tabPanel(
                title = "Silvics & Ecology",
                value = "cciss_instructions_silvics_ecology",
                includeHTML("./instructions/SilvicsEcology.html") 
              ),
              # tabPanel(
              #   title = "Species Portfolio",
              #   value = "cciss_instructions_species_portfolio",
              #   includeHTML("./instructions/SpeciesPortfolio.html") 
              # ),
              tabPanel(
                title = "Export",
                value = "cciss_instructions_export",
                includeHTML("./instructions/Export.html") 
              )
            )
          )
        )       
      ),
      tabPanel(
        title = "Model information",
        value = "model_info",
        fluidRow(
          column(
            width = 6,
            offset = 1,
            tabPanel(
              title = "",
              tags$h4("Current versions of Information Tables, Maps, and Models used in this App"),
              div(
                tableOutput("modelsinfo"),
                plotly::plotlyOutput("timings", width = "100%")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Shiny App Information",
        value = "app_info",
        fluidRow(
          column(
            width = 6,
            offset = 1,
            tabPanel(
              title = "",
              tags$h4("Shiny App Information"),
              tableOutput("shinyinfo")
            )
          )
        )
      )
    )
  )
#)
