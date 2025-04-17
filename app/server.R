# Server start setup ----

# Database connections
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)


dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##bybec db connection
sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)


# poolclim <- dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "bgc_climate_data",
#   host = Sys.getenv("BCGOV_HOST"),
#   port = 5432, 
#   user = Sys.getenv("BCGOV_USR"),
#   password = Sys.getenv("BCGOV_PWD")
# )
onStop(function() {
  poolClose(pool)
  poolClose(sppDb)
  poolClose(dbCon)
  #poolClose(poolclim)
})

DEV = TRUE

#####load feas table from database
#S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
S1 <- ccissr::S1
S1 <- S1[,.(bgc,ss_nospace, spp, newfeas, outrange)]
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible","OHR"))
#####
bcgov_tileserver <- Sys.getenv("BCGOV_TILESERVER")
bcgov_tilelayer <- Sys.getenv("BCGOV_TILELAYER")
district_tileserver <- "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"
if (is.null(bcgov_tileserver) || is.null(bcgov_tilelayer)) stop("tileserver env not set")
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")
source("./server/LeafletSource.R")


##setup edatopic grid
grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)
edaGrid <- setDT(expand.grid(seq(1,5,1),seq(1,8,1)))
setnames(edaGrid,c("X","Y"))
setorder(edaGrid,X,Y)
edaName <- c(paste0("A",7:0),paste0("B",7:0),paste0("C",7:0),paste0("D",7:0),paste0("E",7:0))
edaGrid[,edatopic := edaName]
edaGrid[,Col := "grey"]

### create parameter input charts
gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,0,0,1,1,1,1,0,0,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]

shinyServer(function(input, output, session) {
  # bslib::bs_themer()
  # Reusing Shiny session userData environment
  uData <- session$userData
  portfolio_results <- reactiveValues(data = NULL)
  session_params <- reactiveValues(estabWt = c(0.3,0.35,0.35),futWt = c(0.25,0.25,0.25,0.25),
                                   modelWt = all_weight, rcpWt = rcp_weight, gcmWt = gcm_weight,
                                   show_novelty = TRUE, nov_c = 5, show_ohr = TRUE)
  update_flag <- reactiveVal(0)
  selected_site <- reactiveValues(siteref = NULL, ss = NULL)
  login_text <- reactiveValues(message = '')
  
  source("./server/generate.R", local = TRUE)
  source("./server/points.R", local = TRUE)
  source("./server/map.R", local = TRUE)
  source("./server/feasibility.R", local = TRUE)
  source("./server/silviculture.R", local = TRUE)
  source("./server/futures.R", local = TRUE)
  source("./server/download.R", local = TRUE)
  source("./server/CCISS_Spatial.R", local = TRUE)
  #source("./server/generate_portfolio.R", local = TRUE)
  source("./server/instructions.R", local = TRUE)
  
  output$modelsinfo <- function() {
    knitr::kable(models_info, format = "html", table.attr = 'class="table table-hover"') 
  }
  
  ##login
  if(!DEV){
    showModal(
      modalDialog(
        size = "xl",
        footer = NULL,
        textOutput("login_error"),
        splitLayout(
          wellPanel(
            h4("Sign Up"),
            textInput("su_email","Email:"),
            textInput("su_uname", "Create Username"),
            selectInput("su_role","What are you?", choices = c("", "Forester","Government","Academic","Student","Unicorn","Good CCISSer"), 
                        selected = NULL),
            actionButton("su_go","Sign Up!")
          ),
          wellPanel(
            h4("Log In"),
            textInput("login_uname","Enter Username"),
            actionButton("login_go","Log In!")
          )
        )
      )
    )
    
    output$login_error <- renderText({login_text$message})
    
    observeEvent(input$su_go, {
      if(input$su_email == "" | input$su_uname == "" | input$su_role == ""){
        login_text$message <- "Please enter your email and password."
      }else{
        tryCatch({
          dbExecute(pool, paste0("INSERT INTO cciss_users (username, email, role, nsession) VALUES ('",
                                 input$su_uname,"', '",input$su_email,"','",input$su_role,"',0)"))
          removeModal()
        },
        error = function(e) {
          login_text$message <- "Username Exists. Please login or choose a new one."
        })
      }
    })
    
    observeEvent(input$login_go, {
      if(dbGetQuery(pool, paste0("SELECT exists (SELECT 1 FROM cciss_users WHERE username = '",input$login_uname,"' LIMIT 1);"))[,1]){
        dbExecute(pool,paste0("UPDATE cciss_users SET nsession = nsession + 1 WHERE username = '",input$login_uname,"';"))
        removeModal()
      }else{
        login_text$message <- "Username does not exist."
      }
    })
  }
  
  ##hover text for feasibility report
  hoverText <- c("Species","Time Period","Percentage of models preciting each feasibility",
                 "Chief Forester's reference guide","Environmental Feasibility",
                 "Modelled Establishment feasibility (based on historic,current, and near-future)",
                 "Modelled future feasibility (based on all four future periods)","Model Trends")
  ##show instructions
  # output$modal_ui <- renderUI({
  #   HTML(readLines(rmarkdown::render("./server/CCISS_Instructions.Rmd",encoding = "UTF-8",envir = new.env()),encoding = "UTF-8"))
  # })
  
  uData$bec_click_flag <- FALSE
  ##default session parameters
  output$rcp_select <- renderAmChart4({
    amBarChart(
      data = session_params$rcpWt,
      category = "rcp", values = "weight",
      draggable = TRUE,
      tooltip =
        "[bold]{valueY}[/]",
      xAxis = list(title = amText(text = "Scenario")),
      yAxis = list(
        title = amText(text = "Weight", color = "maroon"),
        labels = amAxisLabels(fontSize = 10),
        gridLines = amLine(color = "orange", width = 1, opacity = 0.4)
      ),
      yLimits = c(0, 1),
      valueFormatter = "#.##",
      theme = "dataviz")
  })
  output$gcm_select <- renderAmChart4({
    amBarChart(
      data = session_params$gcmWt,
      category = "gcm", values = "weight",
      draggable = TRUE,
      columnWidth = 80,
      tooltip =
        "[bold]{valueY}[/]",
      xAxis = list(title = amText(text = "GCM"),
                   labels = amAxisLabels(fontSize = 10,rotation = 90)),
      yAxis = list(
        title = amText(text = "Weight", color = "maroon"),
        labels = amAxisLabels(fontSize = 10),
        gridLines = amLine(color = "orange", width = 1, opacity = 0.4)
      ),
      yLimits = c(0, 1),
      valueFormatter = "#.##",
      theme = "dataviz")
  })
  
  observeEvent(input$sesh_params,{
    showModal(modalDialog(
      h2("Adjust Session Parameters"),
      h6("Climatic Novelty"),
      switchInput("show_novelty", value = session_params$show_novelty, onLabel = "Remove highly novel predictions", 
                  offLabel = "No Novelty Analysis", width = '100%'),
      sliderInput("novelty_cutoff", "Novelty Cutoff Sigma", min = 2, max = 8, value = session_params$nov_c, step = 0.1),
      
      h6("Outside of Home Range (OHR) Species"),
      switchInput("show_ohr", value = session_params$show_ohr, onLabel = "Use OHR Suitabilities", 
                  offLabel = "Remove OHR Suitabilities", width = '100%'),
      
      h6("Establishment Suitability Weights"),
      splitLayout(
        numericInput("hwt961","1961-1990", value = session_params$estabWt[1], min = 0, max = 1,step = 0.05),
        numericInput("hwt991","1991-2020", value = session_params$estabWt[2], min = 0, max = 1,step = 0.05),
        numericInput("hwt21","2021-2040", value = session_params$estabWt[3], min = 0, max = 1,step = 0.05)
      ),
      h6("Maturity Suitability Weights"),
      splitLayout(
        numericInput("wt21","2021-2040", value = session_params$futWt[1], min = 0, max = 1,step = 0.05),
        numericInput("wt41","2041-2060", value = session_params$futWt[2], min = 0, max = 1,step = 0.05),
        numericInput("wt61","2061-2080", value = session_params$futWt[3], min = 0, max = 1,step = 0.05),
        numericInput("wt81","2081-2100", value = session_params$futWt[4], min = 0, max = 1,step = 0.05)
      ),
      h6("GCM Weights"),
      p("Click and drag bars to adjust weights"),
      amChart4Output("gcm_select",width = "790px"),
      h6("Scenario Weights"),
      amChart4Output("rcp_select"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("adj_params","OK!",
                     style = "background-color:#1d8f0e; color: #FFF")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$adj_params,{
    estabWt <- as.numeric(c(input$hwt961,input$hwt991,input$hwt21))
    estabWt <- estabWt/sum(estabWt)
    futWt <- as.numeric(c(input$wt21,input$wt41,input$wt61,input$wt81))
    futWt <- futWt/sum(futWt)
    
    gcm_weight <- as.data.table(input$gcm_select)
    setnames(gcm_weight,c("gcm","weight"))
    rcp_weight <- as.data.table(input$rcp_select)
    setnames(rcp_weight,c("rcp","weight"))
    all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
    all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
    all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
    all_weight[,weight := wgcm*wrcp]
    session_params$estabWt <- estabWt
    session_params$futWt <- futWt; session_params$modelWt <- all_weight;
    session_params$rcpWt <- rcp_weight; session_params$gcmWt <- gcm_weight
    
    session_params$show_novelty <- input$show_novelty
    session_params$nov_c <- input$novelty_cutoff
    session_params$show_ohr <- input$show_ohr
    updateSwitchInput(session, inputId = "ohr_feas", value = input$show_ohr)
    session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', false)"))
    removeModal()
  })
  
  output$shinyinfo <- function() {
    app_info <- data.table(
      Information = c("Shiny host","Tiles source","Tileserver","Github","Copyright","License"),
      Value = c(system("hostname", intern = TRUE), Sys.getenv("BCGOV_TILESERVER"),
                strsplit(Sys.getenv("BCGOV_TILESERVER"), split = "data")[[1]][1],
                as.character(tags$a(href = "https://github.com/bcgov/CCISS_ShinyApp", "bcgov/CCISS_ShinyApp")),
                paste(format(Sys.Date(), "%Y"), "Province of British Columbia"),
                as.character(tags$a(href = "http://www.apache.org/licenses/LICENSE-2.0", "Apache 2.0 LICENSE")))
    )
    knitr::kable(app_info, format = "html", table.attr = 'class="table table-hover"', escape = FALSE)
  }
})
