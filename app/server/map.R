# Map main
output$bec_map <- renderLeaflet({
  leaflet::leaflet() %>%
    leaflet::setView(lng = -122.77222, lat = 51.2665, zoom = 7) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels, group = "Positron",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatterNoLabels, group = "DarkMatter",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                              options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Hillshade",
      options = leaflet::pathOptions(pane = "mapPane")) %>%
    addVectorGridTilesDev() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "Positron Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatterOnlyLabels, group = "DarkMatter Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Mapbox Labels",
      options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::hideGroup("Zones") %>%
    leaflet::hideGroup("DarkMatter Labels") %>%
    leaflet::hideGroup("Positron Labels") %>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE, hideMarkerOnCollapse = TRUE, autoCollapse = TRUE, zoom = 11)) %>%
    leaflet::addLayersControl(
      baseGroups = c("Positron", "DarkMatter", "Satellite", "OpenStreetMap", "Hillshade"),
      overlayGroups = c("Zones", "Subzones Variants", "Positron Labels", "DarkMatter Labels", "Mapbox Labels"),
      position = "topright") %>%
    leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
})

# Map proxy
map_proxy <- leaflet::leafletProxy('bec_map')

clear_mk <- function() {
  leaflet::clearMarkers(map_proxy)
}

draw_mk <- function(data = userpoints$dt) {
  non_na_idx <- which(!is.na(data$Long) & !is.na(data$Lat))
  if (length(non_na_idx)) {
    leaflet::addMarkers(
      map_proxy, data = data[i = non_na_idx], lng = ~Long, lat = ~Lat,
      popup = ~popups, label = ~ID
    )
  }
}

set_map_bound <- function(data = userpoints$dt) {
  if (nrow(data) > 0) {
    bbox <- dbBbox(pool, data, 1000)
    leaflet::fitBounds(map_proxy, bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) 
  }
}

## Map click logic
observeEvent(input$bec_map_click, {
  pos <- input$bec_map_click
  points <- new_points(data.table(Lat = pos$lat, Long = pos$lng))
  insert_points(points)
})
