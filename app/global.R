# Shared shiny setup ----
if (!requireNamespace("Require")) {
  install.packages("Require")
}

suppressPackageStartupMessages({
  Require::Require(c(
    "bslib",
    "bcgov/ccissr@main",
    "colourvalues",
    "DT",
    "ggplot2",
    "ggthemes",
    "kableExtra",
    "leaflet",
    "plotly",
    "pool",
    "prompter",
    "rAmCharts4",
    "rhandsontable",
    "RPostgres",
    "shiny",
    "shinyWidgets"
  ))
})

source("./server/tooltip_verbage.R")
bgc_choices <- SS[grep("BEC",Source),BGC]
