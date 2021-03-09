# Map main

wms <- {
  list(usgs_hs = 
    list(
      baseUrl = "https://basemap.nationalmap.gov:443/arcgis/services/USGSShadedReliefOnly/MapServer/WmsServer?",
      layers = "0",
      options = leaflet::WMSTileOptions(
        format = "image/png",
        crs = leaflet::leafletCRS(crsClass = "L.CRS.EPSG4326"),
        transparent = FALSE),
      attribution = '<a href="https://catalog.data.gov/dataset/usgs-hill-shade-base-map-service-from-the-national-map">USGS</a>'
    ),
  bec_wms = 
    list(
      #WMS config for BEC layer (for greater visual precision at higher zoom level)
      baseUrl = "https://openmaps.gov.bc.ca/geo/pub/WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY/ows?",
      layers = "pub:WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY",
      options = leaflet::WMSTileOptions(
        format = "image/png",
        crs = leaflet::leafletCRS(crsClass = "L.CRS.EPSG4326"),
        transparent = TRUE,
        style = "1409_1410"),
      attribution = '<a href="https://catalogue.data.gov.bc.ca/dataset/bec-map">BEC Map</a>'
    )
  )
}

plugins <- {
  list(vgplugin = 
    htmltools::htmlDependency(
      name = "leaflet.vectorgrid",
      version = "1.3.0",
      src = system.file("htmlwidgets", package = "bccciss"),
      script = c("lfx-vgrid-prod.js", "bec-styling.js")
    )
  )
}

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

addVectorGridTilesDev <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  # This is a custom javascript to enable VectorGrid with Shiny
  # https://leaflet.github.io/Leaflet.VectorGrid/vectorgrid-api-docs.html
  map <- htmlwidgets::onRender(map, '
    function(el, x, data) {
    
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id, opacity) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fillOpacity: opacity,
                fill: true
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
        "http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf",
        vectorTileOptions("bec_z", "BECMap", true,
                          "overlayPane", zoneColors, "ZONE", "OBJECTID", 0.85)
      )
      var subzLayer = L.vectorGrid.protobuf(
        "http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf",
        vectorTileOptions("bec_subz", "BECMap", true,
                          "overlayPane", subzoneColors, "MAP_LABEL", "OBJECTID", 0.5)
      )
      this.layerManager.addLayer(zLayer, "tile", "bec_z", "Zones")
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants")
      
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
            fillOpacity: 0.75,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.OBJECTID, style);
        }
      })
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
    }'
  )
  map
}

output$bec_map <- renderLeaflet({
  leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addVectorGridTilesDev() %>%
    leaflet::hideGroup("Zones") %>%
    leaflet::setView(lng = -122.77222, lat = 51.2665, zoom = 7) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
    leaflet::addWMSTiles(baseUrl = wms$usgs_hs$baseUrl,
                         layers = wms$usgs_hs$layers,
                         options = wms$usgs_hs$options, group = "Hillshade",
                         attribution = wms$usgs_hs$attribution) %>%
    leaflet::addWMSTiles(baseUrl = wms$bec_wms$baseUrl,
                         layers = wms$bec_wms$layers,
                         options = wms$bec_wms$options, group = "BEC WMS",
                         attribution = wms$bec_wms$attribution) %>%
    leaflet::hideGroup("BEC WMS") %>%
    leaflet::addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters") %>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE)) %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite", "Hillshade"),
      overlayGroups = c("Zones", "Subzones Variants", "BEC WMS"),
      position = "topright") %>%
    leaflet::addMiniMap(toggleDisplay = TRUE)
})

# Map methods
map_proxy <- leaflet::leafletProxy('bec_map')

clear_mk <- function() {
  leaflet::clearMarkers(map_proxy)
}

draw_mk <- function(data = uData$points) {
  non_na_idx <- which(!is.na(data$Longitude) & !is.na(data$Latitude))
  leaflet::addCircleMarkers(map_proxy, lng = ~Longitude, lat = ~Latitude,
                            data = data[i = non_na_idx],
                            radius = 6, color = "#444", fillColor = "orangered", stroke = TRUE,
                            fillOpacity = 0.6, opacity = 0.8, weight = 2,
                            popup = ~popups) 
}

## Map click logic
observeEvent(input$bec_map_click, {
  pos <- input$bec_map_click
  points <- new_points(data.table(Latitude = pos$lat, Longitude = pos$lng))
  insert_points(points)
})
