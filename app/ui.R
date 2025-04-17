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
    label = "What does this page do", 
    icon = icon("question-circle")
  ))
}

tags$head(
  tags$link(rel="apple-touch-icon", href="images/bcid-apple-touch-icon.png", sizes="180x180"),
  tags$link(rel="icon", href="images/bcid-favicon-32x32.png", sizes="32x32", type="image/png"),
  tags$link(rel="icon", href="images/bcid-favicon-16x16.png", sizes="16x16", type="image/png"),
  tags$link(rel="mask-icon", href="images/bcid-apple-icon.svg", color="#036"),
  tags$link(rel="icon", href="images/bcid-favicon-32x32.png")
)

navbarPage(
  
  #title = HTML('&nbsp;&nbsp;<img src="logo.svg" class="navbar-logo">',navhelplink("The CCISS Tool", "cciss_about_nav")), ##navhelplink("The CCISS Tool", "cciss_about_nav")
  theme = bslib::bs_theme(
    preset = "bcgov",
    "navbar-brand-padding-y" = "0rem",
    "navbar-brand-margin-end" = "4rem"
  ),
  title = shiny::tagList(
    shiny::tags$image(
      src = "images/bcid-logo-rev-en.svg",
      style = "display: inline-block",
      height = "35px",
      alt = "British Columbia"
    ),
    "The CCISS Tool"
  ),
  collapsible = TRUE,
  windowTitle = "CCISS",
  id = "cciss_navbar",
  # Select sites ----
  tabPanel(
    title = navhelplink("SELECT SITES", "cciss_instructions_select_sites"),
    value = "sites",
    class = "tabcontainer",
    tags$head(includeCSS("./www/style.css")),
    prompter::use_prompt(),
    tags$head(includeScript("./www/cciss.js")),
    tags$script("
$(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover(); 
});"),
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
        p("Welcome to CCISS v0.999 (beta)! Note that this tool uses a yet-to-be released version of BEC (v13), which includes changes to the coastal BEC classification that will be published in Fall 2025. If you would like to use the previous version based on BEC 12, you can find it ", a("here!",href = "https://thebeczone.ca/shiny/cciss12")),
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
            tagList(
              br(),
              actionButton("sesh_params", "Model Parameters", icon = icon("sliders-h"), style = "width:100%; align:center;")
            ),
            tagList(
              br(),
              # p("Report by:"),
              switchInput("aggregation", value = FALSE, onLabel = "Report averaged by BGC    ", offLabel = "Report by individual sites", width = '100%')
            )
            
          )
          
        ),
        
        hr(style = "border-top: 1px solid #8f0e7e;"),
        h4("Add Sites Using One of the 3 Methods Below"),
        
        accordion(
          multiple = FALSE,
          accordion_panel(
            title = h5(
              "Method 1. Click on map to add points",
              prompter::add_prompt(
                tooltipsIcon,
                message = tooltip_text$select_points,
                position = "top-left",
                size = "large",
                shadow = FALSE
              )
            ),
            DT::DTOutput("points_table", width = "100%"),
            actionButton("add_dialog", "Enter New", icon("plus"), width =
                           140),
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
                message = tooltip_text$bgc_click,
                position = "top-left",
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
                message = tooltip_text$upload_csv,
                position = "top-left",
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
                leafletOutput("bec_map", height = "70vh"))
      
    )
  ),
  # Feasibility report ----
  tabPanel(
    tags$script("
$(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover(); 
});"),
    title = navhelplink("SUITABILITY REPORT", "cciss_instructions_feasibility_report_nav"),
    value = "feasibility",
    tags$style(type='text/css', ".selectize-input { font-size: 54px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    sidebarLayout(
      
      # Inputs
      sidebarPanel(
        width = 2,
        sidebarhelplink("cciss_instructions_feasibility_report"),
        h5("Report Type"),
        switchInput("feas_type", value = TRUE, onLabel = "Detailed", offLabel = "Summary", width = '200%'),
        switchInput("ohr_feas", value = TRUE, onLabel = "Use OHR", 
                    offLabel = "Remove OHR", width = '200%'),
        #materialSwitch("feas_type","Full Report", right = TRUE, status = "primary", value = TRUE),
        h5("Filters"),
        selectInput("siteref_feas", label = "Choose Site/BGC", choices = character()),
        selectInput("site_series_feas", label = "Choose Site Series", choices = character()),
        radioButtons(
          "filter_feas",
          label = "Suitability",
          choices = c("All" = "a", "Suitable Only" = "f"),
          selected = "a",
          inline = T
        ),
        h5("Suitability Legend"),
        bslib::tooltip(
          span(HTML(
            paste0(
              '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
              c("limegreen", "deepskyblue", "gold", "grey","black"),
              '" /><span style="vertical-align:middle">&nbsp;',
              c("E1: High", "E2: Moderate", "E3: Low", "X: Not Suitable","Novel Climate"),
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
                 ),
                 radioButtons("future_showss","Show on Plot: ", 
                              choices = c("BGC","Site Series Overlap"),
                              selected = "BGC")
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
             value = "wna_map",
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
                     "1991-2020 (obs)" = 1991,
                     "2001-2020 (mod)" = 2001,
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
    title = navhelplink("EXPORT", "cciss_instructions_export_nav"),
    sidebarLayout(
      # Inputs
      sidebarPanel(
        width = 2,
        sidebarhelplink("cciss_instructions_export"),
        h6("Filter"),
        selectInput(
          "report_filter_feas",
          label = "Tree Species",
          choices = c("All" = "a", "Suitable Only" = "f")
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
  ### CCISS Spatial
  tabPanel(
    title = navhelplink("CCISS SPATIAL", "cciss_instructions_export_nav"),
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        radioButtons("region_type","Subregion Type", choices = c("None", "District","FLP Area")),
        radioButtons("type","Display BGC or Suitability", choices = c("BGC","Suitability"), selected = "BGC"),
        radioButtons("period_type","Choose a Time Period", choices = list(
          "Reference (1961-1990)" = "Historic", 
          "Observed (2001-2020)" = "obs",
          "Future (GCMs)" = "Future")),
        conditionalPanel(condition = "input.period_type == 'Historic'",
                         radioButtons("hist_type", "Mapped or Predicted?", choices = c("Modelled","Mapped"))
                         ),
        conditionalPanel(
          condition = "input.type == 'BGC' & input.period_type == 'Future'",
          h1("GCM Options"),
          selectInput("gcm_select","Select GCM", choices = gcms, selected = gcms[1]),
          selectInput("period_select","Select Period", choices = periods, selected = periods[1])        
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC'",
          h1("Suitability Options"),
          selectInput("edatope_feas","Select Edatope (SNR/SMR)", choices = c("B2","C4","D6"), selected = "C4", multiple = FALSE),
          selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC' & input.period_type !== 'Historic'",
          selectInput("map_stat","Select Map Type", choices = list("Projected Suitability" = "NewFeas",
                                                                   "Suitability Change" = "MeanChange"), multiple = FALSE)     
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC' & input.period_type == 'Future'",
          selectInput("period_feas","Select Period", choices = c(periods[-5])),     
        ),
        conditionalPanel(
          condition = "input.period_type != 'Historic'",
          checkboxInput("novelty","Display Novelty?", value = FALSE),
        ),
        actionButton("clear_map","Hide/Show Layer"),
        downloadButton("download_full","Download Province"),
        
        checkboxInput("findabec","Find-A-BEC"),
        conditionalPanel(condition = "input.findabec == true",
                         pickerInput("selectBGC","Select Zone", 
                                     choices = c("(N)",zones), 
                                     multiple = F,selected = "(N)"),
                         pickerInput("selectSubzone","Select Subzone(s)", choices = "",options = pickerOptions(actionsBox = T), multiple = T),
                         checkboxInput("gray_out", "Gray non-selected BGCs?", value = FALSE),
                         actionButton("clearFAB","Clear Map"),
                         span(textOutput("selectedBEC", inline = T),style= "font-size:24px")
        ),
        tags$head(tags$style(".modal-body{ min-height:70vh}")),
        width = 2
      )
      ,
    
      # Export a Digital Report or Dataset for Analyzed Sites
      mainPanel(width = 10,
                title = "CCISS Spatial",
                useShinyjs(),
                  tags$head(
                    tags$style(HTML("
                              #map-container {
                                width: 100%;
                                height: 100vh;
                                transition: width 0.5s ease-in-out;
                              }
                              .half-map {
                                width: 60% !important;
                                float: left;
                              }
                              #plot-container {
                                width: 35%;
                                float: right;
                              }
                            "))
                  ),
                  # Map container
                  div(id = "map-container",
                      leafletOutput("map", width = "100%", height = "100vh")
                  ),
                  
                  # Plot container (initially hidden)
                  conditionalPanel(condition = "input.region_type !== 'None'",
                      div(id = "plot-container",
                          wellPanel(
                            h2("Summary by Region"),
                            selectInput("xvariable","X-Axis Variable", choices = c("Time","MAT","MAP","CMD","DD5")),
                            conditionalPanel(
                              condition = "input.type == 'BGC'",
                              checkboxInput("zone_sz","Summarise by Zone?",value = TRUE),
                            ),
                            checkboxInput("plot_obs","Show 2001-2020 Observed?", value = TRUE),
                            actionButton("reset_plot","Reset Plot"),
                            actionButton("reset_district","Clear Selected District"),
                            actionButton("action_download","Download Data"),
                            girafeOutput("summary_plot")
                          )
                      )
                    )
                  )
    )  
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
                 choices = c("Suitable Species" = "f", "All Species" = "a")
               )
             ),
             mainPanel(
               width = 10,
               # Silvics & Ecology
               tabsetPanel(
                 type = "pills",
                 
                 tabPanel(title = "Tolerance",
                          tableOutput("silvics_tol_dt")),
                 tabPanel(title = "Resistance",
                          tableOutput("silvics_resist_dt")),
                 tabPanel(title = "Regeneration stage",
                          tableOutput("silvics_regen_dt")),
                 tabPanel(title = "Maturing stage",
                          tableOutput("silvics_mature_dt")),
                 tabPanel(title = "About",
                          includeHTML("./instructions/Silvics_About.html"))
               )
             )
           )),
  # Tech specs ----
  navbarMenu(
    title = "DOCUMENTATION",
    menuName = "cciss_help",
    tabPanel(
      title = "Overview",
      value = "cciss_about",
      fluidRow(
        column(
          width = 6,
          offset = 1,
          tabPanel(
            title = "",
            includeHTML("./instructions/1a_About_CCISS.html") 
          )
        )
      )
    ),
    tabPanel(
      title = "Instructions (How to CCISS)",
      value = "cciss_instructions",
      fluidRow(
        column(
          width = 8,
          offset = 1,
          tags$h4("Instructions (How to CCISS)"),
          tabsetPanel(
            id = "cciss_instructions_set",
            type = "pills",
            tabPanel(
              title = "Select Sites",
              value = "cciss_instructions_select_sites",
              includeHTML("./instructions/2a_SelectSites.html") 
            ),
            tabPanel(
              title = "Suitability Report",
              value = "cciss_instructions_feasibility_report",
              includeHTML("./instructions/2b_SuitabilityReport.html") 
            ),
            tabPanel(
              title = "BEC Futures",
              value = "cciss_instructions_bec_futures",
              includeHTML("./instructions/2c_BECFutures.html") 
            ),
            tabPanel(
              title = "Silvics & Ecology",
              value = "cciss_instructions_silvics_ecology",
              includeHTML("./instructions/2d_SilvicsEcology.html") 
            ),
            # tabPanel(
            #   title = "Species Portfolio",
            #   value = "cciss_instructions_species_portfolio",
            #   includeHTML("./instructions/SpeciesPortfolio.html") 
            # ),
            tabPanel(
              title = "Export",
              value = "cciss_instructions_export",
              includeHTML("./instructions/2e_Export.html") 
            ),
            tabPanel(
              title = "CCISS Spatial",
              value = "cciss_instructions_spatial",
              includeHTML("./instructions/2f_Spatial.html") 
            )
          )
        )
      )       
    ),
    tabPanel(
      title = "Methods (how the tool works)",
      value = "cciss_methods",
      fluidRow(
        column(
          width = 8,
          offset = 1,
          tags$h4("Methods"),
          tabsetPanel(
            id = "cciss_methods_set",
            type = "pills",
            tabPanel(
              title = "Overview",
              value = "cciss_3a",
              includeHTML("./instructions/3a_MethodsOverview.html") 
            ),
            tabPanel(
              title = "BEC",
              value = "cciss_3b",
              includeHTML("./instructions/3b_BEC.html") 
            ),
            tabPanel(
              title = "Suitability Ratings",
              value = "cciss_3c",
              includeHTML("./instructions/3c_SuitabilityRatings.html") 
            ),
            tabPanel(
              title = "Climate Change Projections",
              value = "cciss_3d",
              includeHTML("./instructions/3d_ClimateProjections.html") 
            ),
            tabPanel(
              title = "BGC Model",
              value = "cciss_3e",
              includeHTML("./instructions/3e_BGCmodel.html") 
            ),
            tabPanel(
              title = "Novel Climates",
              value = "cciss_3f",
              includeHTML("./instructions/3f_NovelClimates.html") 
            ),
            tabPanel(
              title = "Edatopic Overlap",
              value = "cciss_3g",
              includeHTML("./instructions/3g_EdatopicOverlap.html") 
            ),
            tabPanel(
              title = "Rule Sets",
              value = "cciss_3h",
              includeHTML("./instructions/3h_Rulesets.html") 
            ),
            tabPanel(
              title = "Outside Home Range",
              value = "cciss_3i",
              includeHTML("./instructions/3i_OHR.html") 
            ),
            # tabPanel(
            #   title = "BEC 13",
            #   value = "cciss_3j",
            #   includeHTML("./instructions/3j_BEC13.html") 
            # ),
            tabPanel(
              title = "Expert Review",
              value = "cciss_3k",
              includeHTML("./instructions/3k_ExpertReview.html") 
            )

          )
        )
      )       
    ),
    tabPanel(
      title = "Known Issues",
      value = "cciss_issues",
      fluidRow(
        column(
          width = 8,
          offset = 1,
          tags$h4("Known Issues"),
          tabsetPanel(
            id = "cciss_issues",
            type = "pills",
            tabPanel(
              title = "Overview",
              value = "cciss_4a",
              includeHTML("./instructions/4a_KnownIssues.html") 
            ),
            tabPanel(
              title = "Sources of Error",
              value = "cciss_4b",
              includeHTML("./instructions/4b_SourcesOfError.html") 
            ),
            tabPanel(
              title = "BGC mapping as a baseline",
              value = "cciss_4c",
              includeHTML("./instructions/4c_BaselineBGCs.html") 
            ),
            tabPanel(
              title = "Space-for-time Substitution",
              value = "cciss_4d",
              includeHTML("./instructions/4d_SpaceForTime.html") 
            )
          )
        )
      )       
    ),
    tabPanel(
      title = "Using CCISS for Decisions",
      value = "cciss_decisions",
      fluidRow(
        column(
          width = 6,
          offset = 1,
          tabPanel(
            title = "",
            includeHTML("./instructions/5a_DecisionGuidance.html") 
          )
        )
      )
    ),
    tabPanel(
      title = "Definitions",
      value = "cciss_definitions",
      fluidRow(
        column(
          width = 8,
          offset = 1,
          tags$h4("Definitions"),
          tabsetPanel(
            id = "cciss_definitions",
            type = "pills",
            tabPanel(
              title = "Glossary of Terms",
              value = "cciss_6a",
              includeHTML("./instructions/6a_GlossaryofTerms.html") 
            ),
            tabPanel(
              title = "Species Codes",
              value = "cciss_6b",
              includeHTML("./instructions/6b_SpeciesCodes.html") 
            ),
            tabPanel(
              title = "BEC Codes",
              value = "cciss_6c",
              includeHTML("./instructions/6c_BECCodes.html") 
            ),
            tabPanel(
              title = "Suitability Definitions",
              value = "cciss_6d",
              includeHTML("./instructions/6d_SuitabilityDefinitions.html") 
            ),
            tabPanel(
              title = "BEC 13 Crosswalk",
              value = "cciss_6e",
              includeHTML("./instructions/6e_BEC13Crosswalk.html") 
            )
          )
        )
      )       
    ),
    tabPanel(
      title = "Providing Feedback",
      value = "cciss_feedback",
      fluidRow(
        column(
          width = 6,
          offset = 1,
          tabPanel(
            title = "",
            includeHTML("./instructions/6a_ProvidingFeedback.html") 
          )
        )
      )
    ),
    tabPanel(
      title = "FAQs",
      value = "cciss_faqs",
      fluidRow(
        column(
          width = 6,
          offset = 1,
          tabPanel(
            title = "",
            includeHTML("./instructions/7a_FAQs.html") 
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
  ),
  tags$footer(
    class = "footer mt-5",
    tags$nav(
      class = "navbar navbar-expand-lg bottom-static navbar-dark bg-primary-nav",
      tags$div(
        class = "container",
        tags$ul(
          class = "navbar-nav",
          tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content/environment/natural-resource-stewardship/natural-resources-climate-change/future-forest-ecosystems-centre", "FFEC Home", target = "_blank")),
          tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=79F93E018712422FBC8E674A67A70535", "Disclaimer", target = "_blank")),
          tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=9E890E16955E4FF4BF3B0E07B4722932", "Privacy", target = "_blank")),
          tags$li(class = "nav-item", tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA", "Copyright", target = "_blank")),
          tags$li(class = "nav-item", tags$a(class = "nav-link", href = "mailto: ffec@gov.bc.ca", "Contact Us", target = "_blank"))
        )
      )
    )
  )
)
#)
