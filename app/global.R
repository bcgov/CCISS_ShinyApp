# Shared shiny setup ----

suppressPackageStartupMessages({
  library(shiny)
  library(ccissdev)
  library(leaflet)
  library(DT)
  library(RPostgres)
  library(plotly)
  library(pool)
  library(ggplot2)
  library(ggthemes)
  library(rAmCharts4)
  library(kableExtra)
  library(rhandsontable)
  library(colourvalues)
  library(prompter)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(bslib)
})

source("./server/tooltip_verbage.R")
bgc_choices <- SS[grep("BEC",Source),BGC]