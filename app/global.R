# # Shared shiny setup ----
# if (!requireNamespace("Require")) {
#   install.packages("Require")
# }
# 
# suppressPackageStartupMessages({
#   Require::Require(c(
#     "colourvalues",
#     "leaflet.extras",
#     "pagedown",
#     "prettydoc",
#     "prompter",
#     "RPostgres"
#   ), require = FALSE)  ## don't load
# })
# 
# suppressPackageStartupMessages({
#   Require::Require(c(
#     "bslib",
#     "bcgov/ccissr@main",
#     "data.table",
#     "DT",
#     "ggplot2",
#     "ggthemes",
#     "kableExtra",
#     "leaflet",
#     "plotly",
#     "pool",
#     "rAmCharts4",
#     "rhandsontable",
#     "shiny",
#     "shinyWidgets"
#   ))
# })

req_libs <- list(
  "bslib",
  "ccissr",
  "data.table",
  "DT",
  "ggplot2",
  "ggthemes",
  "kableExtra",
  "leaflet",
  "plotly",
  "pool",
  "rAmCharts4",
  "rhandsontable",
  "shiny",
  "shinyWidgets",
  "RPostgres",
  "shinyjs",
  "glue"
)

lapply(req_libs, library, character.only = TRUE)

source("./server/tooltip_verbage.R")

### CCISS Spatial
source("cciss_spatial/JS_Source.R")
source("cciss_spatial/plot_functions.R", local = TRUE)
dist_bnds <- fread("cciss_spatial/district_bounds.csv")
flp_bnds <- fread("cciss_spatial/flp_bounds.csv")
dist_bnds <- rbind(dist_bnds,flp_bnds)
t_rast <- rast("cciss_spatial/Raster_Templated.tif")

gcms <- c("Ensemble", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR")
periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080","2081_2100")
base_tileserver <- "https://tileserver.thebeczone.ca/data/bgc_GCM_PERIOD_SUBZONE/{z}/{x}/{y}.webp"
novelty_tileserver <- "https://tileserver.thebeczone.ca/data/novelty_GCM_PERIOD/{z}/{x}/{y}.webp"
species_tileserver <- "https://tileserver.thebeczone.ca/data/STAT_PERIOD_EDATOPE_SPECIES/{z}/{x}/{y}.webp"

colour_ref <- trimws(WNA_BGCs$SubzoneColour)
names(colour_ref) <- WNA_BGCs$BGC

subzones <- sort(WNA_BGCs$BGC)
zones <- sort(unique(WNA_BGCs$Zone))

gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")

gcm_run <- data.table(gcm = c("obs", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0"),
                      run = c(NA,"r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1"),
                      keep = TRUE)
mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

models_info <- fread("CCISS_Version_Info.csv")

## species outlooks summary
species_outlooks_intro_ui <- function() {
  tagList(
    tags$div(
      class = "species-outlooks-intro",
      
      tags$p(
        class = "species-outlooks-lead",
        paste(
          "Welcome to Species Outlooks, a collection of species-specific",
          "dashboard summaries of the inputs and outputs of the Climate Change Informed",
          "Species Selection (CCISS) tool. These dashboards are intended to support",
          "forest practitioners, planners, and land managers in making informed,",
          "forward-looking decisions about where tree species may be suitable to",
          "plant over the coming decades. As climate conditions shift, these",
          "decisions will become increasingly important for maintaining productive,",
          "resilient, and adaptive forest landscapes."
        )
      ),
      
      tags$p(
        "Each Species Outlook includes the following sections:"
      ),
      
      tags$div(
        class = "outlook-section",
        
        tags$h3(
          icon("clock-rotate-left"),
          "Reference (historical baseline) period"
        ),
        
        tags$p(
          paste(
            "Maps and a descriptive summary of the species’ range in British",
            "Columbia during the 1961–1990 baseline climate period.",
            "Environmental suitability is summarized across Biogeoclimatic (BGC)",
            "units and across three edatopic (soil nutrient × moisture) regimes,",
            "relative to the local biogeoclimatic subzone:"
          )
        ),
        
        tags$ul(
          tags$li("Poor/subxeric (B2)"),
          tags$li("Medium/mesic (C4)"),
          tags$li("Rich/hygric (D6)")
        )
      ),
      
      tags$div(
        class = "outlook-section",
        
        tags$h3(
          icon("chart-line"),
          "CCISS projections"
        ),
        
        tags$p(
          paste(
            "Visualizations and interpretations of CCISS projections through",
            "2100, exploring potential changes in the species’ suitable range",
            "across British Columbia. These include:"
          )
        ),
        
        tags$ul(
          tags$li(
            paste(
              "Projected persistence and expansion of suitable range relative",
              "to the historically suitable range"
            )
          ),
          tags$li(
            paste(
              "Shifts in environmental suitability across the province and",
              "within individual BGC zones"
            )
          ),
          tags$li(
            paste(
              "Shifts in environmentally suitable area across the province",
              "and within individual BGC zones"
            )
          ),
          tags$li(
            "Projected changes in particular areas of interest in BC ",
            tags$span(class = "development-label", "In development")
          ),
          tags$li(
            "Projected changes along elevational gradients ",
            tags$span(class = "development-label", "In development")
          )
        )
      ),
      
      tags$div(
        class = "outlook-section",
        
        tags$h3(
          icon("shield-halved"),
          "Forest health"
        ),
        
        tags$p(
          paste(
            "A summary of important forest health considerations and concerns",
            "for the species."
          )
        )
      ),
      
      tags$div(
        class = "outlook-section",
        
        tags$h3(
          icon("book"),
          "References and additional resources"
        ),
        
        tags$ul(
          tags$li("A bibliography of works cited"),
          tags$li(
            paste(
              "Other tools, websites, and guides providing additional information",
              "about the species, particularly background ecology and silvics",
              "information not summarized in the outlook"
            )
          )
        )
      ),
      
      tags$div(
        class = "outlook-note",
        
        tags$h3(
          icon("circle-info"),
          "Interpreting and applying the Species Outlooks"
        ),
        
        tags$p(
          paste(
            "These outlooks provide species-specific overviews and interpretations",
            "of CCISS data. They do not represent management recommendations."
          ),
          " Please see ",
          tags$a(
            "CCISS Documentation: Decision Guidance",
            href = paste0(
              "https://bcgov-ffec.ca/cciss-docs/",
              "Decisions.html"
            ),
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          paste(
            " for information about the appropriate and intended uses of",
            "these data."
          )
        )
      )
    )
  )
}

## species outlook cards
species_outlooks <- data.frame(
  code = c("Cw", "Fd"),
  common_name = c(
    "Western redcedar",
    "Douglas-fir"
  ),
  scientific_name = c(
    "Thuja plicata",
    "Pseudotsuga menziesii"
  ),
  description = c(
    paste(
      "View the province-wide CCISS outlook for western redcedar,",
      "including historical suitability, future projections,",
      "and forest health considerations."
    ),
    paste(
      "View the province-wide CCISS outlook for Douglas-fir,",
      "including historical suitability, future projections,",
      "and forest health considerations."
    )
  ),
  image = c(
    "images/Cw_shadow.gif",
    "images/Fd_shadow.gif"
  ),
  url = c(
    "/spp-outlooks/Cw/",
    "/spp-outlooks/Fd/"
  ),
  available = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

species_outlook_card <- function(code,
                                 common_name,
                                 scientific_name,
                                 description,
                                 image,
                                 url,
                                 available = TRUE) {
  
  card_contents <- tagList(
    tags$div(
      class = "species-card-image",
      
      tags$img(
        src = image,
        alt = paste("Silhouette of", common_name),
        loading = "lazy"
      )
    ),
    
    tags$div(
      class = "species-card-content",
      
      tags$div(
        class = "species-card-heading",
        
        tags$span(
          class = "species-code",
          code
        ),
        
        tags$div(
          tags$h3(common_name),
          tags$p(
            class = "scientific-name",
            tags$em(scientific_name)
          )
        )
      ),
      
      tags$p(
        class = "species-card-description",
        description
      ),
      
      tags$div(
        class = "species-card-action",
        
        if (available) {
          tagList(
            tags$span("View Species Outlook"),
            icon("arrow-up-right-from-square")
          )
        } else {
          tags$span(
            class = "coming-soon-label",
            "Coming soon"
          )
        }
      )
    )
  )
  
  if (available) {
    tags$a(
      class = "species-outlook-card",
      href = url,
      target = "_blank",
      rel = "noopener noreferrer",
      `aria-label` = paste(
        "Open the",
        common_name,
        "Species Outlook in a new tab"
      ),
      card_contents
    )
  } else {
    tags$div(
      class = "species-outlook-card unavailable",
      card_contents
    )
  }
}

species_outlooks_selector_ui <- function() {
  tags$section(
    class = "species-selector",
    `aria-labelledby` = "species-selector-heading",
    
    tags$div(
      class = "species-selector-header",
      
      tags$h2(
        id = "species-selector-heading",
        "Explore Species Outlooks"
      ),
      
      tags$p(
        paste(
          "Select a species to open its complete outlook.",
          "The outlook will open in a new browser tab."
        )
      )
    ),
    
    tags$div(
      class = "species-card-grid",
      
      lapply(
        seq_len(nrow(species_outlooks)),
        function(i) {
          do.call(
            species_outlook_card,
            as.list(species_outlooks[i, ])
          )
        }
      )
    ),
    
    tags$p(
      class = "species-image-credit",
      "Tree silhouettes: Natural Resources Canada, Canadian Forest Service."
    )
  )
}

bcgov_theme <- function(action = c("install","remove")) {
  action <- match.arg(action)
  
  # Injecting bcgov theme directly into bslib library
  target <- find.package("bslib")
  if (file.access(target,2) < 0) {
    stop("This must be run with write access to the bslib package")
  }
  
  src <- "./"
  f <- dir(, recursive = TRUE) |> grep("^fonts|^lib", x = _, value = TRUE)
  
  if (action == "install") {
    lapply(file.path(target, unique(dirname(f))), dir.create, showWarnings = FALSE, recursive = TRUE)
    file.copy(file.path(src, f), file.path(target, f))
  }
  
  if (action == "remove") {
    unlink(file.path(target, f))
    unlink(file.path(target, "lib/bsw5/dist/bcgov"), recursive = TRUE)
  }
  
  return(invisible())
  
}

if (!"bcgov" %in% bslib::bootswatch_themes()) {
  bcgov_theme("install")
}