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
  "shinyjs"
)

lapply(req_libs, library, character.only = TRUE)
source("./server/tooltip_verbage.R")
bgc_choices <- SS[grep("BEC",Source),BGC]

### CCISS Spatial
source("cciss_spatial/JS_Source.R")
source("cciss_spatial/plot_functions.R", local = TRUE)
dist_bnds <- fread("cciss_spatial/district_bounds.csv")
flp_bnds <- fread("cciss_spatial/flp_bounds.csv")
dist_bnds <- rbind(dist_bnds,flp_bnds)
t_rast <- rast("cciss_spatial/Raster_Templated.tif")

gcms <- c("SZ_Ensemble", "Zone_Ensemble", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR")
periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080","2081_2100")
base_tileserver <- "https://tileserver.thebeczone.ca/data/bgc_GCM_PERIOD/{z}/{x}/{y}.webp"
novelty_tileserver <- "https://tileserver.thebeczone.ca/data/Novelty_GCM_PERIOD/{z}/{x}/{y}.webp"
species_tileserver <- "https://tileserver.thebeczone.ca/data/STAT_PERIOD_EDATOPE_SPECIES/{z}/{x}/{y}.webp"

colour_ref <- subzones_colours_ref$colour
names(colour_ref) <- subzones_colours_ref$classification

subzones <- sort(subzones_colours_ref$classification)
zones <- sort(zone_colours$classification)

gcms_use <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0")
runs_use <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1")

gcm_run <- data.table(gcm = c("obs", "ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0"),
                      run = c(NA,"r1i1p1f1","r4i1p1f1","r2i1p3f1","r2i1p1f1","r1i1p1f1","r1i1p1f1"),
                      keep = TRUE)
mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

models_info <- fread("CCISS_Version_Info.csv")

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
