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
      '" class="action-button shiny-bound-input" href="#" style="padding-left:0 !important"><sup><i class="fa fa-question-circle" role="presentation" aria-label="question-circle icon"></i></sup>'
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
    title = "SELECT SITES",
    value = "cciss_home",
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
        p("Welcome to CCISS v13.1!"),
        style = "padding: 5px 5px 5px 5px; margin:0%; overflow-y:scroll; max-height: 90vh; position:relative; align: centre",
        
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
        #wellPanel(
        splitLayout(
          selectInput("findbec","Find-a-BEC", 
                      choices = c("(N)",WNA_BGCs$BGC), 
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
    title = "SUITABILITY REPORT",
    value = "suitability_report",
    tags$style(type='text/css', ".selectize-input { font-size: 54px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
    sidebarLayout(
      
      # Inputs
      sidebarPanel(
        width = 2,
        a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#suitability-report", target = "_blank"), 
        #sidebarhelplink("cciss_instructions_feasibility_report"),
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
        h4("Legend"),
        conditionalPanel(
          condition = "input.feas_type",
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
        ),
        conditionalPanel(
          condition = "!input.feas_type",
          HTML(paste0(
            # First three boxes (filled)
            '<svg viewBox="0 0 1 1" height="18px" width="18px" style="vertical-align:middle">',
            '<rect height="1" width="1" style="fill:', 
            c("blue", "red", "purple"), 
            '"/>',
            '</svg>',
            '<span style="vertical-align:middle; position:relative; top:-2px">&nbsp;',
            c("Increasing", "Decreasing", "Becoming Suitable"),
            '</span>',
            collapse = "<br />"
          ),
          '<br />',
          '<svg viewBox="0 0 1 1" height="18px" width="18px" style="vertical-align:middle; overflow:visible">',
          '<rect height="1" width="1" style="fill:none;stroke:red;stroke-width:0.1"/>',
          '<line x1="-0.2" y1="0.5" x2="1.2" y2="0.5" style="stroke:red;stroke-width:0.15"/>',
          '</svg>',
          '<span style="vertical-align:middle; position:relative; top:-2px">&nbsp;Becoming Unsuitable</span>')
        ),
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
    title = "BEC FUTURES",
    tabPanel(title = "Chart",
             value = "bec_futures",
             sidebarLayout(
               # Inputs
               sidebarPanel(
                 width = 2,
                 a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#bec-futures", target = "_blank"),
                 #sidebarhelplink("cciss_instructions_bec_futures"),
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
                 a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#map", target = "_blank"),
                 #sidebarhelplink("cciss_instructions_bec_futures_spatial"),
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
  # Silvics & Ecology ----
  tabPanel(title = "SILVICS",
           value = "silvics",
           sidebarLayout(
             # Inputs
             sidebarPanel(
               width = 2,
               a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#silvics", target = "_blank"),
               
               #sidebarhelplink("cciss_instructions_silvics_ecology"),
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
                 tabPanel(title = "About",
                          includeHTML("./instructions/Silvics_About.html")),
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
  
  # Export ----
  tabPanel(
    title = "EXPORT",
    value = "export",
    sidebarLayout(
      # Inputs
      sidebarPanel(
        width = 2,
        a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#export", target = "_blank"),
        #sidebarhelplink("cciss_instructions_export"),
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
    title = "CCISS SPATIAL", 
    value = "cciss_spatial",
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        #sidebarhelplink("cciss_instructions_cciss_spatial"),
        a("What does this page do?", href = "https://bcgov-ffec.ca/cciss-docs/Instructions.html#cciss-spatial", target = "_blank"),
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
          condition = "(input.type == 'BGC' & input.period_type != 'Future') | (input.type == 'BGC' & input.gcm_select == 'Ensemble')",
          checkboxInput("byzone","Show map by zone?", value = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC'",
          h1("Suitability Options"),
          selectInput("edatope_feas","Select Edatope (SNR/SMR)", choices = c("B2","C4","D6"), selected = "C4", multiple = FALSE),
          selectInput("species_feas", "Select Species", choices = c("Pl","Sx","Fd","Cw","Hw","Py", "Bl","At", "Ac", "Ep", "Yc", "Pw", "Ss", "Bg", "Lw", "Sb"), multiple = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC' & input.period_type !== 'Historic'",
          selectInput("map_stat","Select Map Type", choices = list("Projected Suitability" = "NewFeas",
                                                                   "Suitability Change" = "MeanChange"), multiple = FALSE)     
        ),
        
        conditionalPanel(
          condition = "input.type !== 'BGC' & input.period_type == 'Future'",
          selectInput("period_feas","Select Period", choices = c(periods)),     
        ),
        conditionalPanel(
          condition = "input.period_type !== 'Historic' & input.gcm_select !== 'Zone_Ensemble'",
          checkboxInput("novelty","Display Novelty?", value = FALSE),
        ),
        actionButton("clear_map","Hide/Show Layer"),
        actionButton("download_spatial","Download Province"),
        
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
                            conditionalPanel(
                              condition = "input.cs_plot_type == 'Area'",
                              selectInput("xvariable","X-Axis Variable", choices = c("Time","MAT","MAP","CMD","DD5")),
                              # conditionalPanel(
                              #   condition = "input.type == 'BGC'",
                              #   checkboxInput("zone_sz","Summarise by Zone?",value = TRUE),
                              # ),
                              checkboxInput("plot_obs","Show 2001-2020 Observed?", value = TRUE)
                            ),
                            
                            radioButtons("cs_plot_type", "Choose a plot type:", choices = c("Area"), selected = "Area"),
                            conditionalPanel(condition = "input.cs_plot_type == 'Area' & input.type != 'BGC'",
                                             checkboxInput("frac_suit", "Use Fractional Suitabilities?", value = TRUE)
                                             ),
                            conditionalPanel(condition = "input.type != 'BGC' & input.cs_plot_type == 'Area' & input.frac_suit == false",
                                             radioButtons("binary_type", "Select Included Suitabilities:", 
                                                          choices = c("E1" = "spp_area_bin1","E1&E2" = "spp_area_bin2","E1&E2&E3" = "spp_area_bin3"), selected = "spp_area_bin1")
                            ),
                            conditionalPanel(condition = "input.cs_plot_type == 'Persistance/Expansion' & input.type == 'Suitability'",
                                             checkboxInput("per_exp_focal","Highlight focal species?", value = FALSE)
                                             ),
                            div(
                              style = "width: 100%;",
                              conditionalPanel(
                                "input.cs_plot_type == 'Area'",
                                girafeOutput("summary_plot")
                              ),
                              conditionalPanel(
                                "input.cs_plot_type != 'Area'",
                                plotOutput("summary_plot_base", click = "per_exp_click")
                              )
                            ),
                            actionButton("reset_plot","Reset Plot"),
                            actionButton("reset_district","Clear Selected Subregion"),
                            downloadButton("sum_plt_download","Download Plot")
                          )
                      )
                    )
                  )
    )  
  ),
  # Tech specs ----
  navbarMenu(
    title = "DOCUMENTATION",
    menuName = "cciss_help",
    tabPanel(
      title = "CCISS Documentation",
      value = "cciss_docs",
      tags$iframe(src = "https://bcgov-ffec.ca/cciss-docs/index.html",
                  width = "100%", frameborder = "0", height = "900px")
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