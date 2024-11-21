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
  "RPostgres"
)

lapply(req_libs, library, character.only = TRUE)
source("./server/tooltip_verbage.R")
bgc_choices <- SS[grep("BEC",Source),BGC]
