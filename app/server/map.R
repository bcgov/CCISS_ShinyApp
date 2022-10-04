
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
    # leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
    #                           options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Hillshade",
      options = leaflet::pathOptions(pane = "mapPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "Positron Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatterOnlyLabels, group = "DarkMatter Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Mapbox Labels",
      options = leaflet::pathOptions(pane = "overlayPane")) %>%
    addBGC() %>%
    #invokeMethod(data = subzones_colours_ref, method = "addBGCTiles", ~classification, ~colour) %>%
    leaflet::hideGroup("DarkMatter Labels") %>%
    leaflet::hideGroup("Positron Labels") %>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE, hideMarkerOnCollapse = TRUE, autoCollapse = TRUE, zoom = 11)) %>%
    leaflet::addLayersControl(
      baseGroups = c("Positron", "DarkMatter", "Satellite", "Hillshade"),
      overlayGroups = c("Subzones Variants","Districts", "Positron Labels", "DarkMatter Labels", "Mapbox Labels"),
      position = "topright") %>%
    ##leaflet::addPolygons(color = "purple") %>% 
    leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
})

# Map proxy
map_proxy <- leaflet::leafletProxy('bec_map')

clear_mk <- function() {
  leaflet::clearMarkers(map_proxy)
}

# Draw markers on the proxy map
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
    # Uses a 1000m buffer around points
    bbox <- dbBbox(pool, data, 1000)
    leaflet::fitBounds(map_proxy, bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) 
  }
}

## Map click logic, on click add point
observeEvent(input$bec_map_click, {
  if(input$preselected == "N"){
    print("In BGC Click")
    uData$bec_click_flag <- TRUE
    pos <- input$bec_map_click
    points <- new_points(data.table(Lat = pos$lat, Long = pos$lng))
    insert_points(points)
  }
})

observeEvent(input$preselected,{
  print(input$preselected)
  if(input$preselected == "BGC_Dist"){
    userpoints$dt <- uData$basepoints
    clear_mk()
    session$sendCustomMessage("selectDist","puppy")
  }else if(input$preselected == "BGC"){
    userpoints$dt <- uData$basepoints
    clear_mk()
    session$sendCustomMessage("selectBGC","puppy")
    session$sendCustomMessage("typeFlag","select")
  }else{
    session$sendCustomMessage("clearBGC","puppy")
    session$sendCustomMessage("selectBGC","puppy")
    session$sendCustomMessage("typeFlag","click")
  }
})

observeEvent(input$bgc_click,{
  uData$bgc_select <- input$bgc_click
  output$bgc_click_show <- renderText({
    c("Selected BGCs:",input$bgc_click)
  })
})

observeEvent(input$dist_click,{
  uData$dist_select <- input$dist_click
  output$dist_click_show <- renderText({
    c("Selected District:",input$dist_click)
  })
})

observeEvent(input$clear_highlight,{
  session$sendCustomMessage("clearDist","puppy")
  session$sendCustomMessage("clearBGC","puppy")
  if(input$preselected == "BGC_Dist"){
    session$sendCustomMessage("selectDist","puppy")
  }
})

######################################################
### WNA MAP #########################################

# Map main
output$wna_map <- renderLeaflet({
  leaflet::leaflet() %>%
    leaflet::setView(lng = -122.77222, lat = 51.2665, zoom = 5) %>%
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
    leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronOnlyLabels, group = "Positron Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatterOnlyLabels, group = "DarkMatter Labels",
                              options = leaflet::pathOptions(pane = "overlayPane")) %>%
    leaflet::addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
      attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
      group = "Mapbox Labels",
      options = leaflet::pathOptions(pane = "overlayPane")) %>%
    add_wna() %>%
    #invokeMethod(data = subzones_colours_ref, method = "addBGCTiles", ~classification, ~colour) %>%
    leaflet::hideGroup("DarkMatter Labels") %>%
    leaflet::hideGroup("Positron Labels") %>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE, hideMarkerOnCollapse = TRUE, autoCollapse = TRUE, zoom = 11)) %>%
    leaflet::addLayersControl(
      baseGroups = c("Positron", "DarkMatter", "Satellite", "OpenStreetMap", "Hillshade"),
      overlayGroups = c("WNA_BEC","Positron Labels", "DarkMatter Labels", "Mapbox Labels"),
      position = "topright") %>%
    ##leaflet::addPolygons(color = "purple") %>% 
    leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE)
})

