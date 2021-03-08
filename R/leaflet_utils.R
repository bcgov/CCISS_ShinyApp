#' Main CCISS APP leaflet map
#' @param render A character. Where the map will be rendered. Either in a Shiny app or a report.
#' Default to app.
#' @return A Leaflet map object.
#' @inheritParams dbGetHexID
#' @importFrom leaflet leaflet setView addProviderTiles addWMSTiles hideGroup addMeasure addLayersControl providers
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom htmlwidgets onRender
#' @export
bccciss_map <- function(points, xName = "Long", yName = "Lat", render = c("app", "report")) {
  l <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addVectorGridTilesDev() %>%
    leaflet::hideGroup("Zones")
  
  if (!missing(points)) {
    pts_crs <- st_as_sf(points,coords = c(xName,yName), crs = 4326)
    bbox <- unname(sf::st_bbox(pts_crs))
    l <- leaflet::setMaxBounds(l, bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
      leaflet::addMarkers(lat = ~LATITUDE, lng = ~LONGITUDE, data = pts_crs)
  } else {
    l <- leaflet::setView(l, lng = -120.5687032, lat = 54.1414255, zoom = 7)
  }
  
  render <- match.arg(render)
  
  if (render == "app") {
    l <- leaflet::addProviderTiles(l, leaflet::providers$Esri.WorldImagery, group = "Satellite")  %>%
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
  }
  
  return(l)
}

#' Add VectorGrid tiles to map
#' @param map A leaflet htmlwidget
#' @export
addVectorGridTilesDev <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  # This is a custom javascript to enable VectorGrid with Shiny
  # https://leaflet.github.io/Leaflet.VectorGrid/vectorgrid-api-docs.html
  map <- htmlwidgets::onRender(map, '
    function(el, x, data) {
      var zLayer = L.vectorGrid.protobuf(
        "http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf",
        L.vectorTileOptions("bec_z", "BECMap", true,
                            "overlayPane", zoneColors, "ZONE", "OBJECTID", 0.85)
      )
      var subzLayer = L.vectorGrid.protobuf(
        "http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf",
        L.vectorTileOptions("bec_subz", "BECMap", true,
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

#' Additional leaflet plugins registration
#' @param map An htmlwidget. i.e. a leaflet map.
#' @param plugin An htmlDependency object from htmltools.
#' @details Adds plugin to the widget.
#' @export
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

#' Load extra plugins dependencies on package load
#' @details [VectorGrid](https://github.com/Leaflet/Leaflet.VectorGrid)
#' is a leaflet extension to display gridded vector data
#' (sliced GeoJSON, TopoJSON or protobuf vector tiles) in Leaflet 1.0.0
#' @importFrom htmltools htmlDependency
#' @export
plugins_loaded <- function() {
  list(plugins = 
    list(vgplugin = 
      htmltools::htmlDependency(
        name = "leaflet.vectorgrid",
        version = "1.3.0",
        system.file("htmlwidgets", package = "bccciss"),
        script = c("lfx-vgrid-prod.js", "bec-styling.js")
      )
    )
  )
}

#' Plugins
#'
#' List of loaded extra plugins dependencies
#'
#' @format A list of plugins.
#'
#' @name plugins
#' @export plugins
#' @rdname plugins
NULL
# Active binding added in zzz.R
"plugins"

#' Load extra WMS tiles providers on package load
#' @details USGS Hill Shade provides simple black and white
#' relief tiles.
#' @importFrom leaflet WMSTileOptions leafletCRS
#' @export
wms_loaded <- function() {
  list(wms =
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
  )
}

#' WMS tiles providers
#'
#' List of extra WMS tiles providers
#'
#' @format A list of wms
#'
#' @name wms
#' @export wms
#' @rdname wms
NULL
# Active binding added in zzz.R
"wms"