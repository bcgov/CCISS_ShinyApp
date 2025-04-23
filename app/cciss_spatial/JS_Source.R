bgc_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_v13/{z}/{x}/{y}.pbf"
bgc_tilelayer <- "WNA_v13"

district_tileserver <- "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf"
district_tilelayer <- "Districts"

plugins <- {
  list(vgplugin =
         htmltools::htmlDependency(
           name = "leaflet.vectorgrid",
           version = "1.3.0",
           src = system.file("htmlwidgets", package = "ccissr"),
           script = "lfx-vgrid-prod.js"
         )
  )
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

zone_colours <- fread("cciss_spatial/WNAv13_ZoneCols.csv")

addBGCTiles <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColorsBGC = {", paste0("'", subzones_colours_ref$classification, "':'", 
                                                subzones_colours_ref$colour,"'", collapse = ","), "};"),
                                           paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$colour, "':'", 
                                                                                  subzones_colours_ref$classification,"'", collapse = ","), "};"),
                                           paste0("var zoneColors = {", paste0("'", zone_colours$colour, "':'", 
                                                                               zone_colours$classification,"'", collapse = ","), "};"),
                                                '
      
      L.bec_layer_opacity = 1
      baseCols = Object.keys(subzoneColors);
      map = this;
      colorpicker = null;
      type = "SZ";
      distFlag = false;
      console.log("Working1");
      
      (function() {
        L.TileLayer.ColorPicker = L.TileLayer.extend({
          options: {
            crossOrigin: "anonymous"
          },
          getColor: function(latlng) {
            var size = this.getTileSize();
            var point = this._map.project(latlng, this._tileZoom).floor();
            var coords = point.unscaleBy(size).floor();
            var offset = point.subtract(coords.scaleBy(size));
            coords.z = this._tileZoom;
            var tile = this._tiles[this._tileCoordsToKey(coords)];
            if (!tile || !tile.loaded) return null;
            try {
              var canvas = document.createElement("canvas");
              canvas.width = 1;
              canvas.height = 1;
              var context = canvas.getContext(\'2d\');
              context.drawImage(tile.el, -offset.x, -offset.y, size.x, size.y);
              return context.getImageData(0, 0, 1, 1).data;
            } catch (e) {
              return null;
            }
          }
        });
        L.tileLayer.colorPicker = function(url, options) {
          return new L.TileLayer.ColorPicker(url, options);
        };
      })();
      
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
      
      subzLayer = L.vectorGrid.protobuf(
        "', bgc_tileserver, '",
        vectorTileOptions("bec_map", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColorsBGC, "BGC", "BGC")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
     
     subzLayer.on("click", function(e){
      Shiny.setInputValue("bgc_click",e.layer.properties.BGC);
     });
     
     subzLayer.on("mousemove",function(e){
        var a = colorpicker.getColor(e.latlng);
        //console.log(a);
        var content = "Current: " + e.layer.properties.BGC;
        if (a !== null & a[3] > 0) {
          var bgcCol = findNearestColor(prepRgb(a), baseCols);
          //console.log(bgcCol);
          if(type == "SZ"){
            var bgc = subzoneColors[bgcCol[0]];
            var mapped = e.layer.properties.BGC;
          }else{
            var bgc = zoneColors[bgcCol[0]];
            var mapped = e.layer.properties.BGC.match(/[A-Z]+/g).join("")
          }
          if (bgcCol[1] < 5) {
          infoBox.update(`
                  <b>Layer Info</b><br>
                  <b>Mapped BGC:</b> ${mapped}<br>
                  <b>Predicted BGC:</b> ${bgc}
              `)
          } else {
            infoBox.update(`
                  <b>Layer Info</b><br>
                  <b>Mapped BGC:</b> ${mapped}<br>
                  <b>Predicted BGC:</b> Zoom In
              `)
          }
        } else {
          infoBox.update(`
                  <b>Layer Info</b><br>
                  <b>Mapped BGC:</b> ${e.layer.properties.BGC}
              `)
        }
        
        
     })
     
     var selectHighlight = "SBSdk";
     subzLayer.on("click", function(e){
      console.log(e.layer.properties.BGC);
      subzLayer.resetFeatureStyle(selectHighlight);
      Shiny.setInputValue("bgc_click",e.layer.properties.BGC);
      var properties = e.layer.properties
			  highlight = properties.BGC
			  var style = {
          weight: 1,
          color: "#fc036f",
          fillColor: subzoneColorsBGC[properties.BGC],
          fillOpacity: 1,
          fill: true
        }
        subzLayer.setFeatureStyle(properties.BGC, style);
      });

      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
      
      //now for rasters
      
    function hexToRgb(hex) {
      hex = hex.replace(\'#\', \'\');
      const r = parseInt(hex.substring(0, 2), 16);
      const g = parseInt(hex.substring(2, 4), 16);
      const b = parseInt(hex.substring(4, 6), 16);
      return { r, g, b };
    }
    
    function prepRgb(rgb) {
      const r = rgb[0];
      const g = rgb[1];
      const b = rgb[2];
      return { r, g, b };
    }
    
    
    function findNearestColor(inputRgb, colorList) {
      let nearestColor = null;
      let smallestDistance = Infinity;
      
      for (const color of colorList) {
        const colorRgb = hexToRgb(color);
        
        // Calculate Euclidean distance
        const distance = Math.sqrt(
          Math.pow(colorRgb.r - inputRgb.r, 2) +
          Math.pow(colorRgb.g - inputRgb.g, 2) +
          Math.pow(colorRgb.b - inputRgb.b, 2)
        );
        
        if (distance < smallestDistance) {
          smallestDistance = distance;
          nearestColor = color;
        }
      }
      
      return [nearestColor, smallestDistance];
    }
    
    // setup for infobox
    var infoBox = L.control({ position: "topright" });

    infoBox.onAdd = function (map) {
        this._div = L.DomUtil.create("div", "info-box");
        this._div.innerHTML = "<b>Hover over a feature</b>";
        return this._div;
    };
    
    infoBox.update = function (content) {
        this._div.innerHTML = content;
    };
      
      Shiny.addCustomMessageHandler("clear_tiles", function(dat){
      if(colorpicker !== null){
        map.removeLayer(colorpicker);
      }
    });
    
    var style = document.createElement("style");
    style.innerHTML = `
        .info-box {
            padding: 10px;
            background: white;
            border-radius: 5px;
            box-shadow: 0px 0px 8px rgba(0,0,0,0.3);
            font-size: 14px;
            min-width: 150px;
            max-width: 300px;
        }
    `;
    document.head.appendChild(style);

    
    Shiny.addCustomMessageHandler("unclear_tiles", function(dat){
      if(colorpicker !== null){
        colorpicker.addTo(map);
      }
      if(distLayer !== null){
        distLayer.bringToFront();
      }
    });

    Shiny.addCustomMessageHandler("add_novelty", function(tile_url){
      t2 = tile_url + "?nocache";
      //console.log(t2);
      if(novelty !== null){
        map.removeLayer(novelty);
      }
      novelty = L.tileLayer.colorPicker(t2, {
        maxNativeZoom: 12,
        maxZoom: 14,
        minNativeZoom: 5,
        minZoom: 5,
      }).addTo(map);
      novelty.bringToFront();
      if(distLayer !== null){
        distLayer.bringToFront();
      }
    });

    Shiny.addCustomMessageHandler("remove_novelty", function(tile_url){
      if(novelty !== null){
        map.removeLayer(novelty);
      }
    });

    Shiny.addCustomMessageHandler("unclear_novelty", function(tile_url){
      if(novelty !== null){
        novelty.addTo(map);
      }
      if(distLayer !== null){
        distLayer.bringToFront();
      }
    });
    
    Shiny.addCustomMessageHandler("resize_map", function(x){
      setTimeout(() => { //make sure it happens after the resize
        map.invalidateSize();
      }, 600);
    });
    
    colorpicker = L.tileLayer.colorPicker("https://tileserver.thebeczone.ca/data/bgc_Historic_1961_1990/{z}/{x}/{y}.webp?nochache", {
        maxNativeZoom: 12,
        maxZoom: 14,
        minNativeZoom: 5,
        minZoom: 5,
      }).addTo(map);

    Shiny.addCustomMessageHandler("update_tiles", function(dat){
      console.log("working");
      tile_url = dat["url"];
      type = dat["type"];
      t2 = tile_url + "?nocache";
      console.log(t2);
      if(type == "SZ"){
        baseCols = Object.keys(subzoneColors);
      }else{
        baseCols = Object.keys(zoneColors);
      }
      
      if(colorpicker !== null){
        map.removeLayer(colorpicker);
      }
      //map.removeLayer(colorpicker);
      colorpicker = L.tileLayer.colorPicker(t2, {
        maxNativeZoom: 12,
        maxZoom: 14,
        minNativeZoom: 5,
        minZoom: 5,
      }).addTo(map);
      if(distLayer !== null){
        distLayer.bringToFront();
      }
    });
    
    colorpicker.on("click", function(e){
      Shiny.setInputValue("cciss_click", e.latlng);
    });

    this.on("overlayadd", function(e){
      console.log("adding BGC");
      if(colorpicker !== null){
        colorpicker.bringToFront();
      }
      if(novelty !== null){
        novelty.bringToFront();
      }
    });
    
    this.on("click", function(event) {
      if(type !== "CCISS"){
        var a = colorpicker.getColor(event.latlng);
        var bgcCol = findNearestColor(prepRgb(a), baseCols);
        var bgc = subzoneColors[bgcCol[0]];
        Shiny.setInputValue("bgc_pred_click",bgc);
      }
      
      });
    
    map.on("mouseout", function () {
        infoBox.update("")
    });
    
    var popup = L.popup({ closeButton: false, autoClose: false });

    map.on("mousemove", function(event) {
      infoBox.addTo(map);
      if(!map.hasLayer(subzLayer)){
        if(type !== "CCISS" & !distFlag){
        var a = colorpicker.getColor(event.latlng);
        //console.log(a);
        if (a !== null & a[3] > 0) {
          var bgcCol = findNearestColor(prepRgb(a), baseCols);
          if(type == "SZ"){
            var bgc = subzoneColors[bgcCol[0]];
          }else{
            var bgc = zoneColors[bgcCol[0]];
          }
          
          if(bgcCol[1] < 4.5){
            infoBox.update(`
                <b>Layer Info</b><br>
                <b>Predicted BGC:</b> ${bgc}
            `)
          } else {
            infoBox.update(`
                <b>Layer Info</b><br>
                <b>Predicted BGC:</b> Zoom In
            `)
          }
          
        } else {
          infoBox.update(`""
            `)
        }
      }
      }
      
    });
     
    }'
  ))
  map
}

##district tilelayers
addDistricts <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
            //Now districts regions
            
      district_flag = true;
      Shiny.setInputValue("dist_flag",false);
      var distHL = "DQU";
      var styleHL = {
            weight: 3,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: false
      };
      var vectorTileOptionsDist=function(layerName, layerId, activ,
                                     lfPane, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true,
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 1,
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
      
      distLayer = L.vectorGrid.protobuf(
        "', district_tileserver, '",
        vectorTileOptionsDist("Districts", "', district_tilelayer, '", true,
                          "tilePane", "dist_code", "dist_code")
      )
      //map_2.layerManager.addLayer(distLayer, "tile", "dist_code", "dist_code");
      
      Shiny.addCustomMessageHandler("addRegionTile",function(data){
        var url = data.url;
        var cname = data.name;
        var cid = data.id;
        console.log(url);
        map.removeLayer(distLayer);
        distLayer = L.vectorGrid.protobuf(url, vectorTileOptionsDist(cname, cname, true,
                          "tilePane", cid, cid)
        )
        map.layerManager.addLayer(distLayer, "tile", cid, cid);
        distLayer.bindTooltip(function(e) {
          const fieldNames = Object.keys(e.properties);
          return e.properties[fieldNames[0]];
        }, {sticky: true, textsize: "12px", opacity: 1});
        distLayer.bringToFront();
        distFlag = true;
        Shiny.setInputValue("dist_flag",distFlag);
        
        distLayer.on("click", function(e){
          distLayer.resetFeatureStyle(distHL);
          distHL = e.layer.properties[cid];
          Shiny.setInputValue("dist_click",distHL);
          distLayer.setFeatureStyle(distHL, styleHL);
          flag = false;
          distFlag = false;
          setTimeout(() => {
            Shiny.setInputValue("dist_flag",false);
          }, 600);
          });
      });
      
      Shiny.addCustomMessageHandler("clear_district",function(x){
        map.removeLayer(distLayer);
        distFlag = false;
      });

      Shiny.addCustomMessageHandler("selectDist",function(x){
        distLayer.bringToFront();
        distFlag = true;
      });
      
      Shiny.addCustomMessageHandler("clearTooltips",function(x){
        distLayer.unbindTooltip();
        distFlag = false;
      });
      
      Shiny.addCustomMessageHandler("reset_district",function(x){
        distLayer.resetFeatureStyle(distHL);
        distFlag = true;
        distLayer.bindTooltip(function(e) {
          const fieldNames = Object.keys(e.properties);
          return e.properties[fieldNames[0]];
        }, {sticky: true, textsize: "12px", opacity: 1});
        //Shiny.setInputValue("dist_click",null);
      });
      
      distLayer.bindTooltip(function(e) {
        const fieldNames = Object.keys(e.properties);
        return e.properties[fieldNames[0]];
      }, {sticky: true, textsize: "12px", opacity: 1});
      
      // end districts
    }'
  ))
  map
}

###find a BEC map
addSelectBEC <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      isGray = false;
      var bgc_ids = Object.keys(subzoneColors);
      var vectorTileOptionsBEC=function(layerName, layerId, activ,
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
                fillOpacity: 1
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      findBEC = L.vectorGrid.protobuf(
        "', bgc_tileserver, '",
        vectorTileOptionsBEC("bec_select", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "BGC", "BGC")
      )

      Shiny.addCustomMessageHandler("add_findabec",function(x){
        //console.log("In add findabec");
        map.layerManager.addLayer(findBEC, "tile", "BGC", "BGC");
        console.log(findBEC);
      })
      
      Shiny.addCustomMessageHandler("remove_findabec",function(x){
        map.removeLayer(findBEC);
      })
      
      //highlight on click
      var styleHL = {
            weight: 0,
            color: "#FFFB00",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
          };
          
      var style_gray = {
            weight: 0,
            color: "#b5b5b5",
            fillColor: "#b5b5b5",
            fillOpacity: 1,
            fill: true
          };
          
      var selectHighlight = [];
      findBEC.on("click", function(e){
        //console.log("click");
        if(isGray) {
          selectHighlight.forEach((ID,i) => {
            findBEC.setFeatureStyle(ID, style_gray);
          });
        } else {
           selectHighlight.forEach((ID,i) => {
            findBEC.resetFeatureStyle(ID);
          });
        }
        Shiny.setInputValue("becselect_click",e.layer.properties.BGC);
        var properties = e.layer.properties
			  selectHighlight = [properties.BGC];
        findBEC.setFeatureStyle(properties.BGC, styleHL);
      });
      
      Shiny.addCustomMessageHandler("gray_out",function(x){
        isGray = true;
        var toGrayOut = bgc_ids.filter(id => !selectHighlight.includes(id));
        toGrayOut.forEach((ID,i) => {
            findBEC.setFeatureStyle(ID, style_gray);
          });
      });
      
      Shiny.addCustomMessageHandler("ungray",function(x){
        isGray = false;
        var toreset = bgc_ids.filter(id => !selectHighlight.includes(id));
        toreset.forEach((ID,i) => {
            findBEC.resetFeatureStyle(ID);
          });
      });

      Shiny.addCustomMessageHandler("highlightBEC",function(BECSelect){
        //console.log(BECSelect);
        if(!Array.isArray(BECSelect)){
          BECSelect = [BECSelect];
        }
        if(isGray) {
          selectHighlight.forEach((ID,i) => {
            findBEC.setFeatureStyle(ID, style_gray);
          });
        } else {
           selectHighlight.forEach((ID,i) => {
            findBEC.resetFeatureStyle(ID);
          });
        }
       
        selectHighlight = BECSelect;
        BECSelect.forEach((ID,i) => {
          findBEC.setFeatureStyle(ID, styleHL);
        });
        Shiny.setInputValue("becselect_click",BECSelect);

      });
      
      Shiny.addCustomMessageHandler("clearBEC",function(gray){
        if(gray){
          bgc_ids.forEach((ID,i) => {
            findBEC.setFeatureStyle(ID, style_gray);
          });
        } else {
          bgc_ids.forEach((ID,i) => {
            findBEC.resetFeatureStyle(ID);
          });
        }
      });
      
      findBEC.bindTooltip(function(e) {
        return e.properties.BGC
      }, {sticky: true, textsize: "10px", opacity: 1});
      findBEC.bringToFront();
    }'
  ))
  map
}


##novelty plot
library(plotly)
library(EnvStats)
library(stats)
plot_analog_novelty <- function(clim.target, clim.analog, clim.point = NULL, analog.focal = NULL,
                                vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                                clim.icv = NULL, weight.icv = 0.5, sigma = TRUE,
                                threshold = 0.95, pcs = NULL, plot3d.pcs=c(1,2,3), biplot = TRUE){
  
  
  ## data cleaning
  clim.target <- clim.target[,..vars]
  clim.analog <- clim.analog[,..vars]
  clim.icv <- clim.icv[,..vars]
  if(!is.null(clim.point)) clim.point <- clim.point[, ..vars]
  
  
  clim.analog <- clim.analog[complete.cases(clim.analog)] # remove rows without data
  clim.analog <- clim.analog[, .SD, .SDcols = which(sapply(clim.analog, function(x) var(x, na.rm = TRUE) > 0))]  # Remove zero-variance columns
  clim.target <- clim.target[, .SD, .SDcols = names(clim.analog)]
  if(!is.null(clim.point)) clim.point <- clim.point[, .SD, .SDcols = names(clim.analog)]
  if(!is.null(clim.icv)) clim.icv <- clim.icv[complete.cases(clim.icv)]
  if(!is.null(clim.icv)) clim.icv <- clim.icv[, .SD, .SDcols = names(clim.analog)]
  
  ## log-transform ratio variables
  logVars <- TRUE
  if(logVars){
    clim.analog <- logVars(clim.analog, zero_adjust = TRUE)
    clim.target <- logVars(clim.target, zero_adjust = TRUE)
    clim.icv <- logVars(clim.icv, zero_adjust = TRUE)
    if(!is.null(clim.point)) clim.point <- logVars(clim.point, zero_adjust = TRUE)
    
    ## remove variables with non-finite values in the target population (this is an edge case that occurs when the target population has a variable (typically CMD) with only zeroes)
    clim.target <- clim.target[, lapply(.SD, function(x) if (all(is.finite(x))) x else NULL)]
    clim.analog <- clim.analog[, .SD, .SDcols = names(clim.target)]
    clim.icv <- clim.icv[, .SD, .SDcols = names(clim.target)]
    if(!is.null(clim.point)) clim.point <- clim.point[, .SD, .SDcols = names(clim.target)]
  }
  
  ## scale the data to the variance of the analog, since this is what we will ultimately be measuring the M distance in. 
  clim.mean <- clim.analog[, lapply(.SD, mean, na.rm = TRUE)]
  clim.sd <- clim.analog[, lapply(.SD, sd, na.rm = TRUE)]
  clim.analog[, (names(clim.analog)) := lapply(names(clim.analog), function(col) {
    (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
  })]
  clim.target[, (names(clim.target)) := lapply(names(clim.target), function(col) {
    (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
  })]
  if(!is.null(clim.point)) clim.point[, (names(clim.point)) := lapply(names(clim.point), function(col) {
    (get(col) - unlist(clim.mean)[col]) / unlist(clim.sd)[col]
  })]
  if(!is.null(clim.icv)) clim.icv[, (names(clim.icv)) := lapply(names(clim.icv), function(col) {
    (get(col) - unlist(clim.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(clim.sd)[col] # subtract mean of ICV to centre the ICV on zero. 
  })]
  
  ## PCA on pooled target and analog
  s <- sample(1:dim(clim.target)[1], dim(clim.analog)[1], replace = TRUE) # select a random sample of the target population to match the analog points. bootstrap if target population is smaller than analog points
  clim.target.sample <- clim.target[s,]
  pca <- prcomp(rbind(clim.analog, clim.target.sample), scale=FALSE)
  pcs.analog <- data.table(predict(pca, clim.analog))
  pcs.target <- data.table(predict(pca, clim.target))
  if(!is.null(clim.icv)) pcs.icv <- data.table(predict(pca, clim.icv))
  if(!is.null(clim.point)) pcs.point <- data.table(predict(pca, clim.point))
  
  if(is.null(pcs)){
    ## select number of pcs
    cumvar <- cumsum(pca$sdev^2 / sum(pca$sdev^2)) # vector of cumulative variance explained
    pcs <- which(cumvar >= threshold)[1]
    if(pcs<3) pcs <- 3
  }
  
  ## z-standardize the pcs to the variance of the analog. this is necessary for a metric that can be translated into sigma values. 
  weight.analog <- 1 - weight.icv
  pcs.mean.analog <- pcs.analog[, lapply(.SD, mean, na.rm = TRUE)]
  pcs.sd.analog <- pcs.analog[, lapply(.SD, sd, na.rm = TRUE)]
  if(!is.null(clim.icv)) pcs.sd.icv <- pcs.icv[, lapply(.SD, sd, na.rm = TRUE)]
  if(!is.null(clim.icv)) pcs.sd.combined <- weight.analog * pcs.sd.analog + weight.icv * pcs.sd.icv
  pcs.sd.use <- if(!is.null(clim.icv)) pcs.sd.combined else pcs.sd.analog
  pcs.analog[, (names(pcs.analog)) := lapply(names(pcs.analog), function(col) {
    (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
  })]
  pcs.target[, (names(pcs.target)) := lapply(names(pcs.target), function(col) {
    (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
  })]
  if(!is.null(clim.icv)) pcs.icv[, (names(pcs.icv)) := lapply(names(pcs.icv), function(col) {
    (get(col) - unlist(pcs.icv[, lapply(.SD, mean, na.rm = TRUE)])[col]) / unlist(pcs.sd.use)[col] # separately centering on the ICV mean becuase sometime the ICV is not centred on the centroid, and we want it to be. 
  })]
  if(!is.null(clim.point)) pcs.point[, (names(pcs.point)) := lapply(names(pcs.point), function(col) {
    (get(col) - unlist(pcs.mean.analog)[col]) / unlist(pcs.sd.use)[col]
  })]
  
  ## create a combined covariance matrix for spatial variation and ICV
  cov.analog <- var(pcs.analog[, 1:pcs])
  cov.icv <- if (!is.null(clim.icv)) var(pcs.icv[, 1:pcs]) else NULL
  if (!is.null(cov.icv)) {
    cov.combined <- weight.analog * cov.analog + weight.icv * cov.icv
  } else {
    cov.combined <- cov.analog
  }
  
  ## Mahalanobis distance and sigma dissimilarity
  md <- (mahalanobis(pcs.target[,1:pcs], rep(0, pcs), cov.combined))^0.5
  p <- pchi(md,pcs) # percentiles of the M distances on the chi distribution with degrees of freedom equaling the dimensionality of the distance measurement (PCs)
  q <- qchi(p,1) # values of the chi percentiles on a standard half-normal distribution (chi distribution with one degree of freedom)
  q[!is.finite(q)] <- 8 # set infinite values to 8 sigma (outside the decimal precision of pchi) 
  q[is.na(p)] <- NA # reset NA values as NA
  
  
  ## Plots for the final iteration of the for loop
  
  # Color Scheme for sigma novelty
  breakseq <- c(0,4,8)
  breakpoints <- c(seq(breakseq[1], breakseq[3], 0.01),199); length(breakpoints)
  ColScheme <- c(colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "black"))(length(breakpoints)))
  
  
  ## 3D scatterplot
  if(TRUE){
    
    # revert to the raw pcs (centered on the analog centroid), because standardization obscures the shape of the analog distribution
    a <- predict(pca, clim.analog)
    b <- predict(pca, clim.target)
    b <- sweep(b, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
    a <- sweep(a, 2, apply(a, 2, mean), '-') # centre the analog centroid on zero. this is done at a later stage than the pca in the distance calculation.
    
    b_colors <- ColScheme[cut(q, breakpoints)] # Define colors for points in 'b'
    
    # Create the 3D scatterplot
    plot <- plot_ly() %>%
      add_trace(
        x = a[, plot3d.pcs[1]], y = a[, plot3d.pcs[2]], z = a[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = "dodgerblue", opacity = 1),
        name = "Analog Points"
      ) %>%
      add_trace(
        x = b[, plot3d.pcs[1]], y = b[, plot3d.pcs[2]], z = b[, plot3d.pcs[3]],
        type = "scatter3d", mode = "markers",
        marker = list(size = 6, color = b_colors, opacity = 1),
        name = "Target Points"
      ) 
    # Add ICV points if they exist
    if(!is.null(clim.icv)) {
      c <- predict(pca, clim.icv)
      c <- sweep(c, 2, apply(c, 2, mean), '-') # centre the ICV on the analog centroid. this is done at a later stage than the pca in the distance calculation. 
      plot <- plot %>%
        add_trace(
          x = c[, plot3d.pcs[1]], y = c[, plot3d.pcs[2]], z = c[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 4, color = "black", opacity = 1),
          name = "ICV"
        )
    }
    # Add selected point if it exists
    if(!is.null(clim.point)) {
      f <- predict(pca, clim.point)
      f <- sweep(f, 2, apply(a, 2, mean), '-') # shift the target data so that the analog centroid is at zero. this is done at a later stage than the pca in the distance calculation.
      plot <- plot %>%
        add_trace(
          x = f[, plot3d.pcs[1]], y = f[, plot3d.pcs[2]], z = f[, plot3d.pcs[3]],
          type = "scatter3d", mode = "markers",
          marker = list(size = 20, color = "black", opacity = 1, symbol = 'cross'),
          hoverinfo = "none", # Turn off hover labels
          name = "Selected location"
        )
    }
    # Add biplot lines
    if(biplot) {
      loadings <- pca$rotation[, plot3d.pcs]
      scale_factor <- max(abs(c(a, b))) * 2
      scaled_loadings <- loadings * scale_factor
      for (i in 1:nrow(scaled_loadings)) {
        plot <- plot %>%
          add_trace(
            x = c(0, scaled_loadings[i, 1]),
            y = c(0, scaled_loadings[i, 2]),
            z = c(0, scaled_loadings[i, 3]),
            type = "scatter3d",
            mode = "lines+text",
            line = list(color = "black", width = 2),
            text = rownames(scaled_loadings)[i],
            textposition = "middle center",
            showlegend = FALSE, 
            name = paste("Loading:", rownames(scaled_loadings)[i])
          )
      }
    }
    
    if(!is.null(analog.focal)) tit = paste("\nNovelty of ",analog.focal," in", pcs, "PCs")
    else tit = paste("\nNovelty in", pcs, "PCs")
    plot <- plot %>%
      layout(
        scene = list(
          xaxis = list(title = paste0("PC", plot3d.pcs[1])),
          yaxis = list(title = paste0("PC", plot3d.pcs[2])),
          zaxis = list(title = paste0("PC", plot3d.pcs[3]))
        ),
        title = list(text = tit, x = 0.05)
      )
  }
  return(plot)
}

logVars <- function(dat,
                    elements = c("AHM", "DD", "Eref", "FFP", "NFFD", "PAS", "PPT", "SHM", "CMD"),
                    base = exp(1),
                    add.fields = FALSE,
                    zero_adjust = FALSE) {
  
  dat <- copy(dat)
  
  # Fields to operate on (generally these should be ratio (zero-limited) variable)
  logFields <- grep(paste(elements, collapse = "|"), names(dat), value = TRUE)
  dat.log <- dat[, .SD, .SDcols = logFields]
  
  # If specified by the user, give zero values a positive value that is one order of magnitude less than the minimum positive value
  if (zero_adjust) {
    dat.log <- dat.log[, lapply(.SD, function(x) {
      x[x <= 0] <- base^(log(min(x[x > 0], na.rm = TRUE), base = base) - 1)
      return(x)
    })]
  }
  
  # Perform log transformation
  dat.log <- dat.log[, lapply(.SD, function(x) log(x, base = base))]
  
  # Add 
  if(add.fields){
    setnames(dat.log, logFields, paste0(logFields, "_log"))
    dat <- cbind(dat, dat.log)
  } else {
    dat[, (logFields) := Map(x =.SD, xname = logFields, f = function(x, xname) {
      x <- dat.log[[xname]]
      return(x)
    }), .SDcols = logFields]
  }
  return(dat)
}

# const pxBounds = e.layer._pxBounds;
# 
# // Convert pixel bounds to geographic bounds
# const southwest = map_2.unproject(pxBounds.getBottomLeft(), map_2.getZoom());
# const northeast = map_2.unproject(pxBounds.getTopRight(), map_2.getZoom());
# 
# // Create a Leaflet LatLngBounds object
# const bounds = L.latLngBounds(southwest, northeast);
# 
# console.log("Corrected Geographic Bounds:", bounds);
# 
# // Zoom the map to fit these bounds
# map_2.fitBounds(bounds);