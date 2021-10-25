##javascript source
bcgov_tileserver <- "https://tileserver.thebeczone.ca/data/BC_BGC/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "BECMap"
district_tileserver <- "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"
wna_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_MAP/{z}/{x}/{y}.pbf"
wna_tilelayer <- "WNA_MAP"

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

add_wna <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
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
                fillOpacity: 0.3
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
        "', wna_tileserver, '",
        vectorTileOptions("WNA_BEC", "', wna_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "WNA_BEC", "WNA_BEC");
      
      var curr_select = ["SBSdk","SBSmc2"];
      var cols;
      var bgc;
      
      Shiny.addCustomMessageHandler("colour_wna",function(col_map){
        console.log(col_map);
        cols = col_map.Col;
        bgc = col_map.BGC;

        curr_select.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });
        curr_select = bgc;
        bgc.forEach((ID,i) => {
         var styleHL = {
            weight: 0.5,
            color: cols[i],
            fillColor: cols[i],
            fillOpacity: 1,
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleHL);
        });

      });
      
      Shiny.addCustomMessageHandler("clearBEC",function(x){
        curr_select.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });
      });
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
    }'
  ))
  map
}

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
                          "tilePane", subzoneColors, "BGC", "BGC")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants");
      
      subzLayer.on("mouseover", function(e){
        if(flag){
          var properties = e.layer.properties;
  			  highlight = properties.BGC;
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.BGC],
            fillOpacity: 0.1,
            fill: true
          };
          subzLayer.setFeatureStyle(properties.BGC, style);
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
        return e.properties.BGC;
      }, {sticky: true, textsize: "10px", opacity: 1});
      
      Shiny.addCustomMessageHandler("typeFlag", function(val){
        if(val == "click"){
          console.log("click");
          flag = true;
          subzLayer.resetFeatureStyle(selectHighlight);
        }else{
          console.log("highlight");
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
      
      var bgcHL;
      var selectedNames = [];
      var selectedBGC = [];
      subzLayer.on("click", function(e){
        if(!flag){
          selectedBGC.push(e.layer.properties.BGC);
          selectedNames.push(e.layer.properties.BGC);
          Shiny.setInputValue("bgc_click",selectedNames);
  			  bgcHL = e.layer.properties.BGC;
          subzLayer.setFeatureStyle(bgcHL, styleHL);
        }else{
          Shiny.setInputValue("bgc_point_click",e.layer.properties.BGC);
        }
      });
      
      Shiny.addCustomMessageHandler("highlight_bec", function(bgcs){
        bgcs.forEach((ID) => {
          subzLayer.setFeatureStyle(ID,styleHL);
        })
      })
      
      Shiny.addCustomMessageHandler("clearBGC",function(x){
        selectedBGC.forEach((ID) => {
          subzLayer.resetFeatureStyle(ID);
        });
        selectedBGC = [];
        selectedNames = [];
        Shiny.setInputValue("bgc_click",selectedNames);
      });
      
      Shiny.addCustomMessageHandler("selectBGC",function(x){
        distLayer.resetFeatureStyle(distHL);
        subzLayer.bringToFront();
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
                fill: true,
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
      
      Shiny.addCustomMessageHandler("selectDist",function(x){
        distLayer.bringToFront();
      });
      
      var distHL;
      distLayer.on("click", function(e){
        distLayer.resetFeatureStyle(distHL);
        distHL = e.layer.properties.dist_code;
        Shiny.setInputValue("dist_click",distHL);
        distLayer.setFeatureStyle(distHL, styleHL);
        subzLayer.bringToFront();
        flag = false;
      });
      
      Shiny.addCustomMessageHandler("clearDist",function(x){
        distLayer.resetFeatureStyle(distHL);
        Shiny.setInputValue("dist_click",null);
      });
      
      distLayer.bindTooltip(function(e) {
        return e.properties.dist_code;
      }, {sticky: true, textsize: "10px", opacity: 1});
      
      // end districts
      
      subzLayer.bringToFront();
      
      updateOpacity = function(value) {
        L.bec_layer_opacity2 = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider2",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="/opacity.svg" />\',
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

##report map function

addBGC_report <- function(map,bgcSelect,distSelect) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el,x,data) {
      bgcHL = data.bgc;
      distHL = data.district;
      console.log(bgcHL);
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
                          "tilePane", subzoneColors, "BGC", "BGC")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants");
      
      
      //highlight on click
      var styleHL = {
            weight: 1.5,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
      };
    
      
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
                fill: true,
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
      
      if(bgcHL){
        if(!Array.isArray(bgcHL)){
          bgcHL = [bgcHL];
        }
        bgcHL.forEach((ID) => {
          subzLayer.setFeatureStyle(ID,styleHL);
        })
      }
      
      if(distHL){
        distLayer.setFeatureStyle(distHL[0],styleHL);
      }
     
    }'
  ), data = list(bgc = bgcSelect,district = distSelect))
  map
}