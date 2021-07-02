# Map vector tiling plugin + opacity slider plugin
plugins <- {
  list(vgplugin = 
         htmltools::htmlDependency(
           name = "leaflet.vectorgrid",
           version = "1.3.0",
           src = system.file("htmlwidgets", package = "ccissdev"),
           script = "lfx-vgrid-prod.js"
         ),
       sliderplugin = htmltools::htmlDependency(
         name = "leaflet.slider",
         version = "1.0.0",
         stylesheet = "lfx-slider.css",
         src = system.file("htmlwidgets", package = "ccissdev"),
         script = "lfx-slider.js"
       )
  )
}
uData$plugins <- plugins

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
uData$registerPlugin <- registerPlugin

# Use VectorGrid to display ZoneSubZone layers and do some highlighting
# App mode is more interactive, report mode is static
addVectorGridTilesDev <- function(map, app = TRUE) {
  map <- registerPlugin(map, plugins$vgplugin)
  if (app) {
    map <- registerPlugin(map, plugins$sliderplugin)
  }
  # This is a custom javascript to enable VectorGrid with Shiny
  # https://leaflet.github.io/Leaflet.VectorGrid/vectorgrid-api-docs.html
  # It also adds a slider control for the layers opacity
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      ', paste0("var zoneColors = {", paste0("'", zones_colours_ref$classification, "':'", zones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      console.log(subzoneColors);
      L.bec_layer_opacity = 0.65;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var zLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_z", "', bcgov_tilelayer, '", true,
                          "tilePane", zoneColors, "ZONE", "OBJECTID")
      )
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_subz", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "OBJECTID")
      );
      console.log(subzLayer);
      this.layerManager.addLayer(zLayer, "tile", "bec_z", "Zones")
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants")
      
      ', if (app) {'
      
      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
      
      // Zone
      
      zLayer.bindTooltip(function(e) {
        return e.properties.ZONE
      }, {sticky: true, textsize: "10px", opacity: 1})
      
      // Subzones
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1})
      
      subzLayer.on("mouseover", function(e) {
        if (e.layer.properties) {
          var properties = e.layer.properties
  			  highlight = properties.OBJECTID
  			  var style = {
            weight: 1,
            color: "#555",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 0.1,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.OBJECTID, style);
        }
      })
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
      
      updateOpacity = function(value) {
        L.bec_layer_opacity = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="www/opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        // Starting opacity value for bec map layers
        value:0.65,
        showValue:true
      })
      
      opacityslider.addTo(this)
      
      '} else {''}, '
    }'
  ))
  map
}
uData$addVectorGridTilesDev <- addVectorGridTilesDev
# Map main
output$bec_map <- renderLeaflet({
  leaflet::leaflet(tOut) %>%
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
    leaflet::addPolygons(color = "purple") %>% 
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
  pos <- input$bec_map_click
  points <- new_points(data.table(Lat = pos$lat, Long = pos$lng))
  insert_points(points)
})
