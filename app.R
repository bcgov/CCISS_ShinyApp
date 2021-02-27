library(shiny)
library(bslib)
library(thematic)
# ----- non-shiny packages
require(data.table)
require(foreach)
require(tidyverse)
require(DBI)
require(sf)
library(here)
require(RPostgreSQL)
library(raster)
library(matrixStats)
library(Rcpp)
require(tictoc)
require(ggplot2)
require(ggthemes)
require(tmap)
library(leaflet)

Rcpp::sourceCpp("0CCISS_Cfn.cpp")
source("1CCISS_Data.R")
source("2CCISS_EdaOverlap.R")
source("3CCISS_Suit.R")
source("setup_function.R")
source("shiny_functions.R")

thematic::thematic_shiny(font = "auto")

# take a vector, make a histogram
ggvec2hist <- function(vec){
  ggplot(data.frame(x = vec), aes(x = x)) + geom_histogram(bins = 42)
}


# set up data
data_list <- setup_data_function()



cciss_app <- function(test_data){
  
  ui <-   navbarPage(title = "CCISS Shiny app draft",
    theme = bslib::bs_theme(
      bg = "white", fg = "#42a5f5", primary = "white",
      base_font = font_google("Bree Serif")
    ),
    tabPanel("BGC",
             bgc_vis_ui("bgc_vis")
    ),
    tabPanel("Site Map",
             site_map_ui("sites")
    ),
    tabPanel("FailRisk",
             risk_plot_ui("risk")
             )
  )
  
  server <-  function(input, output, session) {
    # browser()
    bgc_vis_server("bgc_vis", BGC_data = test_data$BGC)
    site_map_server("sites", site_data = test_data$CCISS_Sites)
    risk_plot_server("risk", summary_data = test_data$CCISS_Summary)
  }
  shinyApp(ui, server)
}

cciss_app(data_list)

