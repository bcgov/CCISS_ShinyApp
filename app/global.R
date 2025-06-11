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

bgc_choices <- c("BAFAun", "BAFAunp", "BGxh1", "BGxh2", "BGxh3", "BGxw1", "BGxw2", 
  "BWBSdk", "BWBSmk", "BWBSmw", "BWBSvk", "BWBSwk1", "BWBSwk2", 
  "BWBSwk3", "CDFmm", "CMAun", "CMAunp", "CMAwh", "CWHdm1", "CWHdm2", 
  "CWHdm3", "CWHds1", "CWHds2", "CWHmm1", "CWHmm2", "CWHms3", "CWHms4", 
  "CWHms5", "CWHvh1", "CWHvh2", "CWHvh3", "CWHvm1", "CWHvm2", "CWHvm3", 
  "CWHvm4", "CWHwh1", "CWHwh2", "CWHwm", "CWHws1", "CWHws2", "CWHws3", 
  "CWHxs", "ESSFdc1", "ESSFdc2", "ESSFdc3", "ESSFdcp", "ESSFdcw", 
  "ESSFdh1", "ESSFdh2", "ESSFdk1", "ESSFdk2", "ESSFdkp", "ESSFdkw", 
  "ESSFdv1", "ESSFdv2", "ESSFdvp", "ESSFdvw", "ESSFmc", "ESSFmcp", 
  "ESSFmcw", "ESSFmh", "ESSFmk", "ESSFmkp", "ESSFmkw", "ESSFmm1", 
  "ESSFmm2", "ESSFmm3", "ESSFmmp", "ESSFmmw", "ESSFmv1", "ESSFmv2", 
  "ESSFmv3", "ESSFmv4", "ESSFmvp", "ESSFmw", "ESSFmw1", "ESSFmw2", 
  "ESSFmwp", "ESSFmww", "ESSFun", "ESSFunp", "ESSFvc", "ESSFvcp", 
  "ESSFvcw", "ESSFwc2", "ESSFwc3", "ESSFwc4", "ESSFwcp", "ESSFwcw", 
  "ESSFwh1", "ESSFwh2", "ESSFwh3", "ESSFwk1", "ESSFwk2", "ESSFwm1", 
  "ESSFwm2", "ESSFwm3", "ESSFwm4", "ESSFwmp", "ESSFwmw", "ESSFwv", 
  "ESSFwvp", "ESSFwvw", "ESSFxc1", "ESSFxc2", "ESSFxc3", "ESSFxcp", 
  "ESSFxcw", "ESSFxv1", "ESSFxv2", "ESSFxvp", "ESSFxvw", "ICHdk", 
  "ICHdm", "ICHdw1", "ICHdw3", "ICHdw4", "ICHmc1", "ICHmc1a", "ICHmc2", 
  "ICHmk1", "ICHmk2", "ICHmk3", "ICHmk4", "ICHmk5", "ICHmm", "ICHmw1", 
  "ICHmw2", "ICHmw3", "ICHmw4", "ICHmw5", "ICHun", "ICHvc", "ICHvk1", 
  "ICHvk2", "ICHwc", "ICHwk1", "ICHwk2", "ICHwk3", "ICHwk4", "ICHxm1", 
  "ICHxw", "ICHxwa", "IDFdc", "IDFdh", "IDFdk1", "IDFdk2", "IDFdk3", 
  "IDFdk4", "IDFdk5", "IDFdm1", "IDFdm2", "IDFdw", "IDFmw2", "IDFww", 
  "IDFxc", "IDFxh1", "IDFxh2", "IDFxk", "IDFxm", "IDFxw", "IDFxx1", 
  "IDFxx2", "IMAun", "IMAunp", "MHmm1", "MHmm2", "MHmmp", "MHms", 
  "MHmsp_WA", "MHun", "MHunp", "MHvh", "MHvhp", "MHwh", "MHwhp", 
  "MSdc1", "MSdc2", "MSdc3", "MSdk", "MSdm1", "MSdm2", "MSdm3", 
  "MSdv", "MSdw", "MSxk1", "MSxk2", "MSxk3", "MSxv", "PPxh1", "PPxh2", 
  "SBPSdc", "SBPSmc", "SBPSmk", "SBPSxc", "SBSdh1", "SBSdh2", "SBSdk", 
  "SBSdw1", "SBSdw2", "SBSdw3", "SBSmc1", "SBSmc2", "SBSmc3", "SBSmh", 
  "SBSmk1", "SBSmk2", "SBSmm", "SBSmw", "SBSun", "SBSvk", "SBSwk1", 
  "SBSwk2", "SBSwk3", "SBSwk3a", "SWBmk", "SWBmks", "SWBun", "SWBuns", 
  "SWBvk", "SWBvks")