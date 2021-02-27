# stacked bar plot

stacked_bar <- function(bgc_dat, BGC_selected){
  
    subset(bgc_dat, bgc_dat$BGC == BGC_selected) %>% 
    ggplot(aes(x = FuturePeriod, fill = BGC.pred, y = BGC.prop)) + 
    geom_bar(stat = "identity") + 
    scale_fill_ordinal(option = "C")
}


bgc_vis_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(2,
      selectInput(ns("bgc"), "Select BGC",
                  choices = c("SBSdk", "IDFxm", "IDFdk3", "SBPSmk"))
    ),
    column(10,
      plotOutput(ns("plot"))
    )
  )
}

bgc_vis_server <- function(id, BGC_data){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$plot = renderPlot(stacked_bar(BGC_selected = input$bgc,
                                         bgc_dat = BGC_data))
  })
}



# test_bgc_display <- function(BGC_data){
#   ui <- bgc_vis_ui("bgc_vis")
#   
#   server <-  function(input, output, session) {
#     
#     bgc_vis_server("bgc_vis", BGC_data = BGC_data)
#   }
#   shinyApp(ui, server)
# }
# 
# test_bgc_display(BGC_data = BGC)




# sites ----------------------------------------------------


plot_sites <- function(sitedata){
  leaflet(sitedata) %>% 
    addTiles() %>% 
    leaflet::fitBounds(lng1 = -126, lat1 = 49,
                       lng2 = -119, lat2 = 55) %>% 
    addMarkers(~Long, ~Lat, label = ~ID1)
}


site_map_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(2,
      tags$div("Selections to filter data shown on map")
    ),
    column(10,
      leafletOutput(ns("plot"))
    )
  )
}

site_map_server <- function(id, site_data){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot = renderLeaflet(plot_sites(sitedata = site_data))
  })
}




# Fail Risk ---------------------------------------------------------------


# 
# data_list$CCISS_Summary 

plot_risk <- function(summarydata){
  summarydata %>% 
    dplyr::select(Spp, ID1, starts_with("FailRisk")) %>%
    pivot_longer(starts_with("FailRisk")) %>%
    mutate(value = factor(value, levels = c("Normal", "Increased", "High"))) %>%
    ggplot(aes(x = Spp, y = value, fill = value)) +
    geom_count(pch = 21) +
    facet_grid(ID1~name) +
    scale_fill_brewer(palette = "Reds")
}


risk_plot_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(2,
           tags$div("This figure could be interactive, displaying more detailed descriptions of which sub-sites (?) are included whenever a dot is clicked.")
    ),
    column(10,
           plotOutput(ns("plot")),
           div(
            "Fail risk for all species, at three different years and across all ID1 values. 
            Larger dots have more repeats of the value `SS_NoSpace` in that site.
               ")
    )
  )
}

risk_plot_server <- function(id, summary_data){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot = renderPlot(plot_risk(summarydata = summary_data))
  })
}


CCISS_Summary %>% 
  group_by(ID1, Spp, FailRisk2025) %>% tally

