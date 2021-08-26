##javascript source
bcgov_tileserver <- "http://159.203.20.90/data/BC_BGC/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "BECMap"
district_tileserver <- "http://159.203.20.90/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"

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
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

addPlugin <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map
}

# jscode_bgc <- paste0('window.LeafletWidget.methods.addBGCTiles = function(BGC,Colour) {
#       var subzoneColors = {};
#       BGC.forEach((bec,i) => {
#         const col = Colour[i];
#         subzoneColors[bec] = col;
#       });
#       
#       var map = this;
#       var vectorTileOptions=function(layerName, layerId, activ,
#                                      lfPane, colorMap, prop, id) {
#         return {
#           vectorTileLayerName: layerName,
#           interactive: true, // makes it able to trigger js events like click
#           vectorTileLayerStyles: {
#             [layerId]: function(properties, zoom) {
#               return {
#                 weight: 0,
#                 fillColor: colorMap[properties[prop]],
#                 fill: true,
#                 fillOpacity: 1
#               };
#             }
#           },
#           pane : lfPane,
#           getFeatureId: function(f) {
#             return f.properties[id];
#           }
#         };
#       };
#       
#       var subzLayer = L.vectorGrid.protobuf(
#         "', bcgov_tileserver, '",
#         vectorTileOptions("bec_subz", "', bcgov_tilelayer, '", true,
#                           "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
#       );
#       //console.log(subzLayer);
#       this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants");
#       
#       subzLayer.on("mouseover", function(e){
#         Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
#         var properties = e.layer.properties;
#   			  highlight = properties.MAP_LABEL;
#   			  var style = {
#             weight: 1,
#             color: "#fc036f",
#             fillColor: subzoneColors[properties.MAP_LABEL],
#             fillOpacity: 1,
#             fill: true
#           };
#           subzLayer.setFeatureStyle(properties.MAP_LABEL, style);
#       });
#       
#       
#       var highlight;
# 		  var clearHighlight = function() {
# 		  	if (highlight) {
# 		  		subzLayer.resetFeatureStyle(highlight);
# 		  	}
# 		  	highlight = null;
# 		  };
# 		  
#       subzLayer.on("mouseout", function(e) {
#         clearHighlight();
#       });
# 		  
#       subzLayer.bindTooltip(function(e) {
#         return tooltipLabs[e.properties.MAP_LABEL];
#       }, {sticky: true, textsize: "10px", opacity: 1});
#       subzLayer.bringToFront();
#       
#       //Now districts regions
#       var vectorTileOptionsDist=function(layerName, layerId, activ,
#                                      lfPane, prop, id) {
#         return {
#           vectorTileLayerName: layerName,
#           interactive: true, 
#           vectorTileLayerStyles: {
#             [layerId]: function(properties, zoom) {
#               return {
#                 weight: 0.5,
#                 color: "#000000",
#                 fill: false,
#                 fillOpacity: 0
#               }
#             }
#           },
#           pane : lfPane,
#           getFeatureId: function(f) {
#             return f.properties[id];
#           }
#         }
#       };
#       var distLayer = L.vectorGrid.protobuf(
#         "', district_tileserver, '",
#         vectorTileOptionsDist("Districts", "', district_tilelayer, '", true,
#                           "tilePane", "dist_code", "dist_code")
#       )
#       this.layerManager.addLayer(distLayer, "tile", "Districts", "Districts")
#       // end districts
#       
#     };')

addBGC <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity2 = 0.65
      var selectHighlight;
      var flag = true;
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
                fillOpacity: L.bec_layer_opacity2
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_subz", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "OBJECTID")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants");
      
      subzLayer.on("mouseover", function(e){
        if(flag){
          var properties = e.layer.properties;
  			  highlight = properties.OBJECTID;
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(properties.OBJECTID, style);
        }
        
      });


      var highlight;
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  };

      subzLayer.on("mouseout", function(e) {
      if(flag){
          clearHighlight();
      }
      });
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
      
      Shiny.addCustomMessageHandler("typeFlag", function(val){
        if(val == "click"){
          flag = true;
          subzLayer.resetFeatureStyle(selectHighlight);
        }else{
          flag = false;
        }
      });
      
      //highlight on click
      var styleHL = {
            weight: 1.5,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
      };

      subzLayer.on("click", function(e){
        if(!flag){
          var properties = e.layer.properties;
          subzLayer.resetFeatureStyle(selectHighlight);
          selectHighlight = properties.OBJECTID;
          Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
          subzLayer.setFeatureStyle(properties.OBJECTID, styleHL);
        }
      });
      
            //Now districts regions
      var vectorTileOptionsDist=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true,
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0.5,
                color: "#000000",
                fill: false,
                fillOpacity: 0
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
      };
      var distLayer = L.vectorGrid.protobuf(
        "', district_tileserver, '",
        vectorTileOptionsDist("Districts", "', district_tilelayer, '", true,
                          "tilePane", "dist_code", "dist_code")
      )
      this.layerManager.addLayer(distLayer, "tile", "Districts", "Districts")
      // end districts
      
      updateOpacity = function(value) {
        L.bec_layer_opacity2 = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider2",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="www/opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        title: "Adjust BGC Opacity",
        // Starting opacity value for bec map layers
        value:0.65,
        showValue:true
      })
      opacityslider.addTo(this);
    }'
  ))
  map
}