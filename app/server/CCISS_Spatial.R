
dist_nm <- reactiveVal()
globalLeg <- reactiveValues(Legend = NULL)

observeEvent(input$clear_map,{
  if(input$clear_map %% 2 != 0){
    session$sendCustomMessage("clear_tiles","Luna")
    session$sendCustomMessage("remove_novelty","Luna")
  }else{
    session$sendCustomMessage("unclear_tiles","Luna")
    if(input$novelty){
      session$sendCustomMessage("unclear_novelty","Luna")
    }
  }
  
})

observeEvent({c(input$novelty, input$period_feas, input$gcm_select, input$period_select)},{
  if(input$novelty & input$period_type != "Historic"){
    if(input$period_type == "obs"){
      pnm <- "Obs"
      prd <- "2001_2020"
    }else{
      pnm <- input$gcm_select
      prd <- input$period_select
    }
    if(input$type == "Suitability"){
      pnm <- "Ensemble"
      prd <- input$period_feas
    }
    tile_url <- gsub("GCM", pnm, novelty_tileserver)
    tile_url <- gsub("PERIOD", prd, tile_url)
    session$sendCustomMessage("add_novelty",tile_url)
  }else{
    session$sendCustomMessage("remove_novelty","puppy")
  }
})

curr_cell <- reactiveVal()

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
    addBGCTiles() %>%
    addDistricts() %>%
    addSelectBEC() %>%
    addLayersControl(
      baseGroups = c("Hillshade","Satellite"),
      overlayGroups = c("BGCs","Cities"),
      position = "topright") %>%
    hideGroup("BGCs")
})

##add tiles
observe({
  if(input$type == "BGC"){
    if(!input$novelty) {
      globalLeg$Legend <- NULL
    }
    
    pnm <- "Ensemble"
    prd <- "1961_1990"
    ens_type <- if(is.null(input$byzone)) "Subzone" else if(input$byzone) "Zone" else "Subzone"
    if(input$period_type == "Historic"){
      if(input$hist_type == "Mapped"){
        pnm <- "Mapped"
        prd <- "1961_1990"
      } else {
        pnm <- "Ensemble"
        prd <- "1961_1990"
      }

    } else if (input$period_type == "obs") {
      pnm <- "Ensemble"
      prd <- "2001_2020_obs"
    } else if (input$period_type == "Future") {
      if(!is.null(input$gcm_select)){
        pnm <- input$gcm_select
        prd <- input$period_select
      }
    }
    tile_url <- gsub("GCM", pnm, base_tileserver)
    tile_url <- gsub("PERIOD", prd, tile_url)
    tile_url <- gsub("SUBZONE", ens_type, tile_url)
    print(tile_url)
    dat <- list(url = tile_url, type = if(ens_type == "Zone") "Zone" else "SZ")
    #message("Sending to JS")
    session$sendCustomMessage("update_tiles",dat)
    # if(input$novelty){
    #   session$sendCustomMessage("remove_novelty","puppy")
    #   tile_url <- gsub("GCM", input$gcm_select, novelty_tileserver)
    #   tile_url <- gsub("PERIOD", input$period_select, tile_url)
    #   session$sendCustomMessage("add_novelty",tile_url)
    # }
  }
  if(!is.null(input$species_feas) & input$type != "BGC"){
    #browser()
    if(input$period_type == "Historic"){
      if(input$hist_type == "Mapped"){
        stat <- "NewFeas"
        period <- "1961_1990_mapped"
      } else {
        stat <- "NewFeas"
        period <- "1961_1990"
      }
      
    } else if (input$period_type == "obs") {
      stat <- input$map_stat
      period <- "2001_2020_obs"
    } else if (input$period_type == "Future") {
      stat <- input$map_stat
      period <- input$period_feas
    }
    if(!is.null(stat) & !is.null(period) &!is.null(input$edatope_feas)){
      tile_url <- gsub("STAT", stat, species_tileserver)
      tile_url <- gsub("PERIOD", period, tile_url)
      tile_url <- gsub("EDATOPE", input$edatope_feas, tile_url)
      tile_url <- gsub("SPECIES", input$species_feas, tile_url)
      #cat(tile_url)
      session$sendCustomMessage("remove_novelty", "puppy")
      dat <- list(url = tile_url, type = "CCISS")
      session$sendCustomMessage("update_tiles",dat)
    }
    
  }
  
})

##turn off novelty when type is changed
observeEvent(input$type,{
  updateCheckboxInput(session, "novelty", value = FALSE)
})

observeEvent({c(input$map_stat,input$type, input$novelty)},{
  if(input$novelty) {
    globalLeg$Legend <- c(0,2,4,6,8)
    globalLeg$Colours <- c("gray90", "gray50", "#FFF200", "#CD0000", "#000000")
    globalLeg$Title <- "Sigma Novelty"
  } else {
    if(input$type == "Suitability"){
      globalLeg$Legend <- c("E1: High","E2: Moderate","E3: Low","EX: Unsuitable")
      globalLeg$Colours <- c("#006400", "#1E90FF", "#EEC900","#F7F7F7")
      globalLeg$Title <- "Climatic Suitability"
    }
    if(input$map_stat == "MeanChange") {
      globalLeg$Legend <- c("-3","-2","-1","No change","+1","+2","+3","Becoming unsuitable","Newly Suitable (3)","Newly Suitable (2)","Newly Suitable (1)")
      globalLeg$Colours <- c("#67001F", "#D6604D", "#FDDBC7", "#F7F7F7", 
                             "#D1E5F0", "#4393C3", "#053061", "#000000", 
                             "#FFFFCC", "#FFEDA0", "#FED976")
      globalLeg$Title <- "Change in Suitability"
    }
  }
})

observe({
  if(!is.null(input$period_type) & input$type != "BGC") {
    if(input$period_type == "Historic"){
      globalLeg$Legend <- c("E1: High","E2: Moderate","E3: Low","EX: Unsuitable")
      globalLeg$Colours <- c("#006400", "#1E90FF", "#EEC900","#F7F7F7")
      globalLeg$Title <- "Climatic Suitability"
    }
  }
})

observe({
  if(!is.null(globalLeg$Legend)){
    leafletProxy("map") |>
      addLegend(position = "bottomright",
                labels = globalLeg$Legend,
                colors = globalLeg$Colours,
                title = globalLeg$Title,
                layerId = "map_legend")
  } else {
    leafletProxy("map") |>
      removeControl("map_legend")
  }
})

observeEvent(input$map_click,{
  lat <- input$map_click$lat
  lng <- input$map_click$lng
  #browser()
  if(!input$dist_flag & !input$findabec){
    if(input$type == "Suitability"){
      cell_click <- cellFromXY(t_rast, cbind(lng,lat))
      print(cell_click)
      curr_cell(cell_click)
      qry <- paste0("select * from bgc_preds where cellid = ",cell_click)
      #cat(qry)
      dat <- dbGetQuery(dbCon, qry)|> setDT()
      dat[,bgc_prop := bgc_prop / sum(bgc_prop), by = fp_code]
      
      output$bgc_plot <- renderPlotly({
        
        fig <- plot_ly(data = dat, x = ~fp_code,
                       y = ~bgc_prop, split = ~bgc_pred, type = 'bar',
                       color = ~bgc_pred, colors = colour_ref, hovertemplate = "%{y}",
                       text = ~bgc_pred, textposition = 'inside', textfont = list(color = "black", size = 12),
                       texttemplate = "%{text}") %>%
          layout(yaxis = list(title = "", tickformat = ".1%"),
                 xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                              ticktext = c("1961-1990","2001-2020 (obs)", "2001-2020", "2021-2040","2041-2060","2061-2080","2081-2100"),
                              tickvals = c(1961,1981,2001,2021,2041,2061,2081)),
                 barmode = 'stack')
        fig
      })
      
      
      
      if(input$species_feas %in% c("Ac","Ep","Pw","Ss","Bg")) {
        output$feas_plot <- NULL
        output$feas_message <- renderText("Sorry, plots for this species are not currently available.")
      } else {
        output$feas_plot <- renderGirafe({
          plot_suitability(dbCon, cellid = cell_click, edatope = input$edatope_feas, spp_name = input$species_feas)
        })
        output$feas_message <- NULL
      }
      
      showModal(modalDialog(
        title = paste0("BGC and Suitability Projections"),
        plotlyOutput("bgc_plot"),
        textOutput("feas_message"),
        girafeOutput("feas_plot"),
        easyClose = TRUE,
        footer = NULL,
        size = "m"
      ))
    } else {
      if(input$novelty){
        test_fut <- dbGetQuery(dbCon, paste0("select * from future_climate where \"GCM\" = '",input$gcm_select,
                                             "' and \"PERIOD\" = '",input$period_select,"' and bgc_pred = '",input$bgc_pred_click,"'")) |> as.data.table()
        test_hist <- dbGetQuery(dbCon, paste0("select * from historic_climate where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
        test_icv <- dbGetQuery(dbCon, paste0("select * from historic_icv where bgc = '",input$bgc_pred_click,"'")) |> as.data.table()
        
        elev_info_sql <- paste0("
          WITH pts4269 AS (SELECT st_transform(st_pointfromtext('POINT(", lng, " ", lat, ")', 4326), 4269) geom)
          
          SELECT MAX(ROUND(CAST(ST_Value(dem.rast, pts.geom) as NUMERIC), 2)) elevation_m
          FROM bc_elevation dem
          CROSS JOIN pts4269 pts
          WHERE ST_Intersects(dem.rast, pts.geom)
        ")
        elev <- dbGetQuery(pool, elev_info_sql)
        point_focal <- data.table(lon = lng, lat = lat, elev = elev$elevation_m[1], id = 1)
        if(input$gcm_select == "Ensemble") {
          point_clim <- climr::downscale(point_focal, gcms = gcms_use, 
                                            ssps = "ssp245", gcm_periods = input$period_select,
                                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                                            return_refperiod = FALSE)
          point_clim <- point_clim[,lapply(.SD, mean), .SDcols = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_"))]
        } else {
          point_clim <- climr::downscale(point_focal, gcms = input$gcm_select, 
                                            ssps = "ssp245", gcm_periods = input$period_select,
                                            run_nm = runs_use[gcms_use == input$gcm_select],
                                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                                            return_refperiod = FALSE)
          point_clim <- point_clim[PERIOD != "1961_1990",]
        }
        
        
        output$novelty_plot <- renderPlotly({
          plot_analog_novelty(clim.target = test_fut, clim.analog = test_hist, clim.icv = test_icv, clim.point = point_clim, analog.focal = input$bgc_pred_click, pcs = NULL)
        })
        
        showModal(modalDialog(
          title = paste0("Analog Novelty Plot"),
          plotlyOutput("novelty_plot", height = "70vh"),
          size = "l",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        cell_click <- cellFromXY(t_rast, cbind(lng,lat))
        curr_cell(cell_click)
        print(cell_click)
        fp <- substr(input$period_feas,1,4)
        qry <- paste0("select * from bgc_preds where cellid = ",cell_click)
        #cat(qry)
        dat <- dbGetQuery(dbCon, qry) |> setDT()
        dat[,bgc_prop := bgc_prop / sum(bgc_prop), by = fp_code]
        
        output$bgc_plot_2 <- renderPlotly({
          
          fig <- plot_ly(data = dat, x = ~fp_code,
                         y = ~bgc_prop, split = ~bgc_pred, type = 'bar',
                         color = ~bgc_pred, colors = colour_ref, hovertemplate = "%{y}",
                         text = ~bgc_pred, textposition = 'inside', textfont = list(color = "black", size = 12),
                         texttemplate = "%{text}") %>%
            layout(yaxis = list(title = "", tickformat = ".1%"),
                   xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                ticktext = c("1961-1990","2001-2020 (obs)", "2001-2020", "2021-2040","2041-2060","2061-2080","2081-2100"),
                                tickvals = c(1961,1981,2001,2021,2041,2061,2081)),
                   barmode = 'stack')
          fig
        })
        
        showModal(modalDialog(
          title = paste0("BGC Projections"),
          plotlyOutput("bgc_plot_2"),
          easyClose = TRUE,
          footer = NULL,
          size = "m"
        ))
      }
      
    }
  }
  
  
})

############FIND a BEC#######################
observeEvent(input$findabec,{
  if(input$findabec){
    session$sendCustomMessage("clear_tiles","waddles")
    session$sendCustomMessage("add_findabec","waddles")
  } else {
    session$sendCustomMessage("remove_findabec","waddles")
    session$sendCustomMessage("unclear_tiles","waddles")
  }
})

observeEvent(input$gray_out,{
  if(input$gray_out){
    session$sendCustomMessage("gray_out","waddles")
  } else {
    session$sendCustomMessage("ungray","waddles")
  }
})

observeEvent(input$selectBGC,{
  if(input$selectBGC == "(N)"){
    #browser()
    updatePickerInput(session,"selectSubzone",choices = subzones,selected = "")
    session$sendCustomMessage("clearBEC",input$gray_out)
  }else{
    session$sendCustomMessage("clearBEC",input$gray_out)
    temp <- subzones[grep(input$selectBGC,subzones)]
    updatePickerInput(session,"selectSubzone",choices = temp,selected = temp)
  }
})

observeEvent(input$clearFAB,{
  updatePickerInput(session,"selectBGC",selected = "(N)")
  session$sendCustomMessage("clearBEC",input$gray_out)
})

observeEvent(input$selectSubzone,{
  session$sendCustomMessage("highlightBEC",input$selectSubzone)
})

observeEvent(input$becselect_click,{
  output$selectedBEC <- renderText({
    if(length(input$becselect_click) > 1){
      c("Selected BGC: ",
        input$selectBGC)
    }else{
      c("Selected BGC: ",
        input$becselect_click)
    }
    
  })
})

##-----------------------------------------
## Summary Figures
##-----------------------------------------
plot_vals <- reactiveVal()
bubble_click <- reactiveVal(NULL)

observeEvent(input$region_type, {
  if(input$region_type == "None"){
    runjs("
      //console.log('Map clicked!');
      let map = document.getElementById('map-container');
      
      if (map.classList.contains('half-map')) {
        console.log('Expanding map...');
        map.classList.remove('half-map');
        //Shiny.setInputValue('toggle_plot', 'hide', {priority: 'event'});
      }
    ")
  } else {
    #print("I'm here!")
    runjs("
      console.log('Map clicked!');
      let map = document.getElementById('map-container');
      
      if (!map.classList.contains('half-map')) {
        console.log('Shrinking map...');
        map.classList.add('half-map');
        //Shiny.setInputValue('toggle_plot', 'show', {priority: 'event'});
      }
    ")
  }
})

observeEvent(input$region_type,{
  session$sendCustomMessage("resize_map","waddles")
})


observe({
  if(input$region_type != "None"){
    if(input$region_type == "District"){
      dat <- list(url = "https://tileserver.thebeczone.ca/data/Districts/{z}/{x}/{y}.pbf", name = "Districts", id = "dist_code")
    }else{
      dat <- list(url = "https://tileserver.thebeczone.ca/data/flp_2026/{z}/{x}/{y}.pbf", name = "flp_bnd", id = "ORG_UNIT")
    }
    session$sendCustomMessage("addRegionTile",dat)
    session$sendCustomMessage("reset_district","Luna")
  }else{
    session$sendCustomMessage("clear_district","Waddles")
  }
})

# observeEvent(input$dist_flag,{
#   print(input$dist_flag)
# })


observeEvent(input$dist_click,{
  temp <- dist_bnds[ORG_UNIT == input$dist_click,]
  print(temp)
  leafletProxy("map") %>%
    fitBounds(temp$xmin, temp$ymin, temp$xmax, temp$ymax)
})

observeEvent(input$reset_district,{
  session$sendCustomMessage("reset_district","Luna")
})

observe({
  if(input$type == "Suitability" & input$period_type != "Historic"){
    updateRadioButtons(session, "cs_plot_type", choices = c("Area", "Persistance/Expansion", "Alluvial"))
  } else if (input$type != "Historic"){
    updateRadioButtons(session, "cs_plot_type", choices = c("Area", "Persistance/Expansion"))
  }
  if(input$period_type == "Historic" & input$type == "BGC") {
    updateRadioButtons(session, "cs_plot_type", choices = c("Area"))
  } else if(input$period_type == "Historic") {
    updateRadioButtons(session, "cs_plot_type", choices = c("Area", "Alluvial"))
  }
})

output$summary_plot_base <- renderPlot({
  if(is.null(input$dist_click)) return(NULL)
  stdarea <- input$dist_click
  
  if(input$cs_plot_type == "Alluvial") {
    dat <- dbGetQuery(dbCon, glue_sql("select * from alluvial_area 
                                      where region = {stdarea} 
                                      AND spp = {input$species_feas} 
                                      AND edatope = {input$edatope_feas}", .con = dbCon))
    setDT(dat)
    setnames(dat, c("region", "bgc", "Edatope", "Spp", "FuturePeriod", "SppArea"))
    #browser()
    plot_alluvial(dat, spp = input$species_feas, edatope = input$edatope_feas)
  } else if (input$cs_plot_type == "Persistance/Expansion") {
    #print(input$dist_click)
    if(input$period_type  == "Historic"){
      return(NULL)
    } else if (input$period_type == "obs"){
      period_sel <- "2001_2020_obs"
    } else {
      if(input$type == "BGC") period_sel <- input$period_select
      else period_sel <- input$period_feas
    }
    
    if(input$type == "BGC"){
      dat <- dbGetQuery(dbCon, glue_sql("select ssp, gcm, run, period, bgc_pred, persistance, expansion from bgc_per_exp 
                                      where region = {input$dist_click}", .con = dbCon))
      names(dat) <- c("ssp", "gcm", "run", "period", "bgc_pred", "Persistance", "Expansion")
      setDT(dat)
      bgc_bubbleplot(dat, period = period_sel, unit.focal = bubble_click(), scenario = "ssp245")
    }else{
      dat <- dbGetQuery(dbCon, glue_sql("select * from spp_per_exp where region = {input$dist_click} and edatopic = {input$edatope_feas}", .con = dbCon))
      names(dat) <- c("region", "spp", "Edatopic", "ssp", "period", "MappedSuit", "Persistance", "Expansion")
      setDT(dat)
      dat <- na.omit(dat, cols = c("Persistance", "Expansion"))
      spp_bubbleplot(dat, species.focal = bubble_click(), period = period_sel, scenario = "ssp245", edatope = input$edatope_feas, xlim = c(0, 1.4))
    }
  }
})

observe({
  input$per_exp_focal
  input$species_feas
  if(input$per_exp_focal) {
    bubble_click(input$species_feas)
  } else {
    bubble_click(NULL)
  }
})

observeEvent(input$per_exp_click,{
  if(!is.null(input$per_exp_click)){
    if(input$type == "Suitability") {
      if (input$period_type == "obs"){
        period_sel <- "2001_2020_obs"
      } else {
        period_sel <- input$period_feas
      }
      dat <- dbGetQuery(dbCon, glue_sql("select spp, AVG(persistance) AS per, AVG(expansion) AS expand from spp_per_exp 
                                      where period = {period_sel} 
                                      and region = {input$dist_click} 
                                      and edatopic = {input$edatope_feas} 
                                      and ssp = 'ssp245'
                                      and persistance IS NOT NULL
                                      group by spp", .con = dbCon))
      setDT(dat)
      dat[, expand := log2(expand)]
      #browser()
      temp_near <- nearPoints(dat, input$per_exp_click, xvar = "per", yvar = "expand", threshold = 50)
      if(nrow(temp_near) == 0) bubble_click(NULL)
      else {
        print(temp_near$spp[1])
        bubble_click(temp_near$spp[1])
      } 
    } else {
      #browser()
      if (input$period_type == "obs"){
        period_sel <- "2001_2020_obs"
      } else {
        period_sel <- input$period_select
      }
      dat <- dbGetQuery(dbCon, glue_sql("select bgc_pred AS bgc, AVG(persistance) AS per, AVG(expansion) AS expand from bgc_per_exp 
                                      where period = {period_sel} 
                                      and region = {input$dist_click} 
                                      and ssp = 'ssp245'
                                      and persistance IS NOT NULL
                                      group by bgc_pred", .con = dbCon))
      setDT(dat)
      ylim <- -5
      dat[expand<2^(ylim[1]-1), expand := 2^(ylim[1]-1)]
      dat[, expand := log2(expand)]
      #browser()
      temp_near <- nearPoints(dat, input$per_exp_click, xvar = "per", yvar = "expand", threshold = 50)
      if(nrow(temp_near) == 0) bubble_click(NULL)
      else {
        print(temp_near$bgc[1])
        bubble_click(temp_near$bgc[1])
      }
    }
    
  }
})

output$summary_plot <- renderGirafe({
  if(is.null(input$dist_click) | input$cs_plot_type != "Area") return(NULL)
  stdarea <- input$dist_click
  print(input$dist_click)
  if(input$period_type %in% c("Historic","obs")){
    gcm_curr <- "ensembleMean"
    run_curr <- "ensembleMean"
  } else {
    if(grepl("Ensemble", input$gcm_select)){
      gcm_curr <- "ensembleMean"
      run_curr <- "ensembleMean"
    }else{
      gcm_curr <- input$gcm_select
      run_curr <- runs_use[gcms_use == input$gcm_select]
    }
  }
  
  if(input$type == "BGC"){
    smry <- "Zone"
    #else smry <- "Subzone"
    p <- plot_bgc(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, run_nm = run_curr, 
                                 unit = smry, focal_bgc = plot_vals(), plot_obs = input$plot_obs)

  }else{
    if(input$frac_suit) tname <- "spp_area_frac"
    else tname <- input$binary_type
    p <- plot_species(dbCon, stdarea, xvariable = input$xvariable, edatope = input$edatope_feas, table_name = tname,
                      spp_select = input$species_feas, focal_spp = plot_vals(), plot_obs = input$plot_obs)
  }
  x <- girafe(ggobj = p)
  x <- girafe_options(x,
                      opts_toolbar(hidden = c("lasso_select","lasso_deselect")))
  x
})

output$sum_plt_download <- downloadHandler(
  filename = function(){
    paste0(input$type, "_Summary_", input$dist_click, ".png")
  },
  content = function(file) {
    stdarea <- input$dist_click
    if(input$period_type %in% c("Historic","obs")){
      gcm_curr <- "ensembleMean"
      run_curr <- "ensembleMean"
    } else {
      if(grepl("Ensemble", input$gcm_select)){
        gcm_curr <- "ensembleMean"
        run_curr <- "ensembleMean"
      }else{
        gcm_curr <- input$gcm_select
        run_curr <- runs_use[gcms_use == input$gcm_select]
      }
    }
    
    ##plot type
    if(input$cs_plot_type == "Alluvial") {
      dat <- dbGetQuery(dbCon, glue_sql("select * from alluvial_area 
                                      where region = {stdarea} 
                                      AND spp = {input$species_feas} 
                                      AND edatope = {input$edatope_feas}", .con = dbCon))
      setDT(dat)
      setnames(dat, c("region", "bgc", "Edatope", "Spp", "FuturePeriod", "SppArea"))
      #browser()
      p <- plot_alluvial(dat, spp = input$species_feas, edatope = input$edatope_feas)
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    } else if (input$cs_plot_type == "Persistance/Expansion") {
      #print(input$dist_click)
      if(input$period_type  == "Historic"){
        return(NULL)
      } else if (input$period_type == "obs"){
        period_sel <- "2001_2020_obs"
      } else {
        if(input$type == "BGC") period_sel <- input$period_select
        else period_sel <- input$period_feas
      }
      
      png(filename = file, width = 8, height = 5, units = "in", res = 400)
      if(input$type == "BGC"){
        dat <- dbGetQuery(dbCon, glue_sql("select ssp, gcm, run, period, bgc_pred, persistance, expansion from bgc_per_exp 
                                      where region = {input$dist_click}", .con = dbCon))
        names(dat) <- c("ssp", "gcm", "run", "period", "bgc_pred", "Persistance", "Expansion")
        setDT(dat)
        bgc_bubbleplot(dat, period = period_sel, unit.focal = bubble_click(),ylim = c(-5,4), scenario = "ssp245")
      }else{
        dat <- dbGetQuery(dbCon, glue_sql("select * from spp_per_exp where region = {input$dist_click} and edatopic = {input$edatope_feas}", .con = dbCon))
        names(dat) <- c("region", "spp", "Edatopic", "ssp", "period", "MappedSuit", "Persistance", "Expansion")
        setDT(dat)
        dat <- na.omit(dat, cols = c("Persistance", "Expansion"))
        spp_bubbleplot(dat, species.focal = bubble_click(), period = period_sel, scenario = "ssp245", ylim = c(-5,4), edatope = input$edatope_feas, xlim = c(0, 1.4))
      }
      dev.off()
    } else {
      if(input$type == "BGC"){
        if(input$zone_sz) smry <- "Zone"
        else smry <- "Subzone"
        p <- plot_bgc(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, run_nm = run_curr, 
                      unit = smry, focal_bgc = plot_vals(), plot_obs = input$plot_obs)
      }else{
        #browser()
        p <- plot_species(dbCon, stdarea, xvariable = input$xvariable, gcm_nm = gcm_curr, 
                          run_nm = run_curr, edatope = input$edatope_feas, spp_select = input$species_feas, 
                          focal_species = plot_vals(), plot_obs = input$plot_obs)
      }
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
    
  }
)

observeEvent(input$summary_plot_selected,{
  plot_vals(input$summary_plot_selected)
})

observeEvent(input$zone_sz,{
  plot_vals(NULL)
})

observeEvent(input$reset_district,{
  plot_vals(NULL)
  bubble_click(NULL)
})

observeEvent(input$type,{
  plot_vals(NULL)
  bubble_click(NULL)
})

observeEvent(input$reset_plot,{
  plot_vals(NULL)
  bubble_click(NULL)
})

observeEvent(input$download_spatial, {
  if(is.null(input$dist_click)){
    showModal(modalDialog(
      title = "Download Provincial Raster",
      downloadButton("download_cciss","Download Raster"),
      uiOutput("download_legend",inline = F)
    ))
  } else {
    showModal(modalDialog(
      title = "Download Regional Raster",
      checkboxInput("clip_download","Clip Raster to Region?"),
      downloadButton("download_cciss","Download Raster"),
      uiOutput("download_legend",inline = F)
    ))
  }
})

observeEvent(input$dist_click, {
  if(is.null(input$dist_click)) {
    updateActionButton(session, "download_spatial", label = "Download Province")
  } else {
    updateActionButton(session, "download_spatial", label = "Download Region")
  }
})

output$download_legend <- renderUI(
  if(input$type == "BGC"){
    a(href="downloadable_docs/BGC_Legend.csv", "Download Legend", download=NA, target="_blank")
  }else{
    if(input$map_stat == "Feasibility"){
      a(href="downloadable_docs/Feasibility_Legend.csv", "Download Legend", download=NA, target="_blank")
    }else{
      a(href="downloadable_docs/MeanChange_Legend.csv", "Download Legend", download=NA, target="_blank")
    }
  }
)

output$download_cciss <- downloadHandler(
  filename = function(){
    if(is.null(input$dist_click)) distnm <- "Provincial"
    else distnm <- input$dist_click
    
    if(input$type == "BGC"){
      if(input$period_type == "Historic") {
        gcms <- "Reference"
        period <- "1961_1990"
      } else if(input$period_type == "obs") {
        gcms <- "Observed"
        period <- "2001_2020"
      } else {
        gcms <- input$gcm_select
        period <- input$period_select
      }
      if(input$novelty) bname <- "ClimaticNovelty_"
      else bname <- "BGCs_"
      paste0(bname,distnm, "_", gcms,"_", period,".tif")
    }else{
      period <- input$period_feas
      if(input$period_type == "Historic") {
        period <- "1961_1990" 
      } else if(input$period_type == "obs") {
        period <- "Observed_2001_2020"
      }
      if(input$novelty) bname <- "ClimaticNovelty_"
      else bname <- input$map_stat
      paste0(bname,distnm, "_", period,"_", input$species_feas,"_",input$edatopic_feas,".tif")
    }
  },
  content = function(file){
    if(input$type == "BGC"){
      if(input$period_type == "Historic") {
        gcms <- "Ensemble"
        period <- "1961_1990"
      } else if(input$period_type == "obs") {
        gcms <- "Ensemble"
        period <- "2001_2020_obs"
      } else {
        gcms <- input$gcm_select
        period <- input$period_select
      }
      sz <- "Subzone"
      if(!is.null(input$byzone)) if(input$byzone) sz <- "Zone"
      lname <- paste0("bgcRaw_",gcms,"_",period,"_",sz,".tif")
      print(lname)
      tname <- "cciss_download"
    }else{
      if(input$period_type == "Historic") {
        period <- "1961_1990"
      } else if(input$period_type == "obs") {
        period <- "2001_2020_obs"
      } else {
        period <- input$period_feas
      }
      sname <- switch(input$map_stat,
                      NewFeas = "FeasibilityRaw_",
                      MeanChange = "MeanChangeRaw_")
      lname <- paste0(sname,period,"_",input$edatope_feas,"_",input$species_feas,".tif")
      tname <- "cciss_download"
    }
    
    avail_lyrs <- dbGetQuery(dbCon, sprintf("select distinct filename from %s",tname))$filename
    if(!lname %in% avail_lyrs){
      showModal(modalDialog(
        title = "Selected Layer Not Available for Download"
      ))
    } else {
      if(!is.null(input$dist_click)){
        bnd <- dist_bnds[ORG_UNIT == input$dist_click,.(ymax, ymin, xmax, xmin)]
        boundary <- t(bnd)[,1]
        #browser()
        withProgress(min = 0, max = 5, value = 1, message = "Preparing Data", {
          rst <- dbGetFeasible(dbCon, table_name = tname, layer_name = lname, boundary = boundary)
        })
        
        if(input$clip_download){
          if(input$region_type == "FLP Area"){
            bnds <- vect("cciss_spatial/flp_bnds.gpkg")
          }else{
            bnds <- vect("cciss_spatial/district_bnds.gpkg")
          }
          bnd_shp <- bnds[bnds$ORG_UNIT == input$dist_click,]
          rst <- mask(rst, bnd_shp)
        }
        writeRaster(rst, file, datatype = "INT2S")
      } else {
        if(input$novelty) {
          withProgress(min = 0, max = 5, value = 1, message = "Preparing Data", {
            if(input$period_type == "obs") {
              dat <- dbGetQuery(dbCon, "select cellid, novelty from novelty_raw where model = 7")
            } else {
              id <- model_ids[grep(input$gcm_select,model), id]
              dat <- dbGetQuery(dbCon, paste0("select cellid, novelty from novelty_raw where model = ",id," and fp_code = ",substr(input$period_select,1,4)))
            }
            rout <- copy(t_rast)
            values(rout) <- NA
            rout[dat$cellid] <- dat$novelty / 100
          })
          writeRaster(rout, file, datatype = "FLT4S")
        } else {
          withProgress(min = 0, max = 5, value = 1, message = "Preparing Data", {
            rst <- dbGetFeasible(dbCon, table_name = tname, layer_name = lname, boundary = bc_bbox)
            incProgress(1, message = "Writing raster...")
          })
          writeRaster(rst, file, datatype = "INT2S")
        }
        
      }
        
    }
    }
    
    
)

# output$download_cciss_full <- downloadHandler(
#   filename = function(){
#     if(input$novelty){
#       paste0("Novelty_",input$gcm_select,"_",input$period_select,".tif")
#     } else {
#       if(input$type == "BGC"){
#         paste0("bgc_raw_",input$dist_click, "_", input$gcm_select,"_", input$period_select,".tif")
#       }else{
#         paste0(input$map_stat,input$dist_click, "_", input$period_feas,"_", input$species_feas,"_",input$edatopic_feas,".tif")
#       }
#     }
#     
#   },
#   content = function(file){
#     if(input$novelty) {

#     } else {
#       if(input$type == "BGC"){
#         lname <- paste0("bgc_raw_",input$gcm_select,"_",input$period_select,".tif")
#         tname <- "bgc_raw"
#       }else{
#         #browser()
#         sname <- switch(input$map_stat,
#                         NewFeas = "Feasibility_",
#                         MeanChange = "MeanChange_")
#         lname <- paste0(sname,input$period_feas,"_",input$edatope_feas,"_",input$species_feas,".tif")
#         tname <- switch(input$map_stat,
#                         NewFeas = "feasibility_raw",
#                         MeanChange = "meanchange_raw")
#       }
#       withProgress(min = 0, max = 5, value = 1, message = "Preparing Data", {
#         rst <- dbGetFeasible(dbCon, table_name = tname, layer_name = lname, boundary = bc_bbox)
#         incProgress(1, message = "Writing raster...")
#         writeRaster(rst, file, datatype = "INT2S")
#       })
#     }
#     
#   }
# )

