library(shiny)
library(data.table)
library(leafem)
library(leaflet)

mbtk="pk.eyJ1Ijoid2htYWNrZW4iLCJhIjoiY2twaDVkNXU5MmJieTJybGE3cWRtY3Q4aCJ9.ISBkzSHFfrr78AVP2y2FeQ"
mblbsty = "whmacken/ckph5q6d21q1318nz4shnyp20"
mbsty="whmacken/ckph5e7y01fhr17qk5nhnpo10"

shinyApp(
  ui = fluidPage(
    actionButton("display","GeoTiff"),
    leafletOutput("map", height = 900)
  ),
  server = function(input, output, session) {
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(maxZoom = 12)) %>%
        setView(lng = -122.77222, lat = 54.2665, zoom = 6) %>%
        leaflet::addTiles(
          urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
          attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
          group = "Hillshade",
          options = leaflet::pathOptions(pane = "mapPane")) %>%
        leaflet::addTiles(
          urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbsty, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
          attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
          group = "Cities",
          options = leaflet::pathOptions(pane = "overlayPane")) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                                  options = leaflet::pathOptions(pane = "mapPane")) %>%
        #addPlugin() %>%
        addLayersControl(
          baseGroups = c("Hillshade","Satellite"),
          overlayGroups = c("Cities"),
          position = "topright")
    })
    
    observeEvent(input$display,{
      leafletProxy("map") |>
        addGeotiff("./app/MeanChange_2041_2060_C4_Pl.tif",
                   resolution = 120,
                   rgb = TRUE)
    })
    
  }
)