#' Main CCISS APP leaflet map
#' @param points A data.frame like object containing geoposition of
#' points of interest.
#' @param bec_tiles A vector tiles provider created with
#' [vector_tiles_provider] to fetch BEC layers.
#' @return A Leaflet map object.
#' @importFrom leaflet leaflet setView addProviderTiles addWMSTiles hideGroup addMeasure addLayersControl providers
#' @importFrom leaflet.extras addSearchOSM searchOptions
#' @importFrom htmlwidgets onRender
#' @export
bccciss_map <- function(points, bec_tiles) {
  l <- leaflet::leaflet() %>%
    leaflet::setView(lng = -120.5687032, lat = 54.1414255, zoom = 7) %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite")  %>%
    leaflet::addWMSTiles(baseUrl = wms$usgs_hs$baseUrl,
                layers = wms$usgs_hs$layers,
                options = wms$usgs_hs$options, group = "Hillshade",
                attribution = wms$usgs_hs$attribution) %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") %>%
    leaflet::addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      secondaryLengthUnit = "kilometers",
      primaryAreaUnit = "sqmeters") %>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = FALSE)) %>%
    registerPlugin(plugins$vgplugin) %>%
    # This is a custom javascript to enable VectorGrid with Shiny
    # using BEC geojson data
    # https://leaflet.github.io/Leaflet.VectorGrid/vectorgrid-api-docs.html
    htmlwidgets::onRender('
      function(el, x, data) {

        var vectorTileOptionsSubz = {
          vectorTileLayerName : "bec_subz",
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            BECMap: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: "#ccc",
                fillOpacity: 0.75,
                fill: true
              }
            }
          },
          // This allow each feature to have an identifier
          // equal to OBJECTID property
          getFeatureId: function(f) {
            console.log(f);
            //return f.properties["OBJECTID"];
          },
          // This assign the grid to the overlay layer
          // so it will drawn above the base map layer
          pane : "overlayPane"
        };

        var vectorTileOptionsZone = {
          vectorTileLayerName : "bec_z",
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            BECMap: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: "#aaa",
                fillOpacity: 0.75,
                fill: true
              }
            }
          },
          // This allow each feature to have an identifier
          // equal to OBJECTID property
          getFeatureId: function(f) {
            console.log(f);
            //return f.properties["OBJECTID"];
          },
          // This assign the grid to the overlay layer
          // so it will drawn above the base map layer
          pane : "overlayPane"
        };

        // Create the vector grid layer and add to the widget
        // using the same methods as would R leaflet package.

        var zLayer = L.vectorGrid.protobuf("http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf", vectorTileOptionsZone, {maxNativeZoom: 16});
        var subzLayer = L.vectorGrid.protobuf("http://159.203.39.184/data/tiles/{z}/{x}/{y}.pbf", vectorTileOptionsSubz, {maxNativeZoom: 16});
        this.layerManager.addLayer(
          zLayer, // layer
          "tile", // category
          "bec_z", // layerId
          "Zones" // group
        );
        this.layerManager.addLayer(
          subzLayer, // layer
          "tile", // category
          "bec_subz", // layerId
          "Subzones Variants" // group
        );

        //Define a js onClick event to trigger a Shiny event with bec_cur
        subzLayer.on("click",
         function(e) {
          console.log(e);
        //   if (e.layer.properties) {
        //     Shiny.setInputValue("bec_cur", {
          //    latlng: e.latlng,
          //     properties: e.layer.properties
        //    });
         // }
          }
        );
      }') %>%
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Hillshade", "OpenStreetMap"),
      overlayGroups = c("Zones", "Subzones Variants"),
      position = "topright") %>%
    leaflet::hideGroup("Zones")
}

#' Define VectorGrid tiles provider to use with
#' VectorGrid.Protobuf
#' @param name A character string. Name of the tiles provider.
#' @param uri_template A character string. URI template to fetch tiles.
#' @details See leaflet
#' [VectorGrid documentation](https://leaflet.github.io/Leaflet.VectorGrid/vectorgrid-api-docs.html#vectorgrid-protobuf)
#' for more details
#' @return A list of provider attributes.
#' @export 
vector_tiles_provider <- function(name, uri_template
                                  #, vectorTileLayerStyles,
                                  #subdomains, key, maxNativeZoom, token, ...
                                  ) {
  return()
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
        system.file("application/js", package = "bccciss"),
        script = "Leaflet.VectorGrid.bundled.min.js"
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