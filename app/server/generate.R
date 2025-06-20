observeEvent(input$generate_results, priority = 100, {

  ticker <- tic("Save Map Inputs")
  # On generate click, we are taking a snapshot of the current points
  # and calculating results. All relevant results will be stored in the
  # userdata environment for further reuse. User has the ability to update
  # results on demand instead of on app state change. This reduce the load
  # on the app and give some room in case computation get more costly
  # in the future. Shared functions will be stored in userdata environment
  # as well as they will be reused to build report. uData is an alias for
  # the userdata environment.
  
  # Input from the app
  if(input$acc == "acc2"){
    pointNums <- dbGetBGC(pool,bgc = uData$bgc_select,district = uData$dist_select, maxPoints = 150) #uData$dist_select
    userpoints$bgc_pts <- pointNums
    avg <- uData$avg <- TRUE
    pts <- uData$pts <- data.table(Site = pointNums)
    uData$dist_select <- NULL
    #print(pts$Site)
  }else{
    avg             <- uData$avg             <- as.logical(input$aggregation)
    pts             <- uData$pts             <- userpoints$dt
  }
  uData$session_params <- reactiveValuesToList(session_params) 
  #browser()
  # Results from processing
  tic("Fetch CCISS Data from DB", ticker)
  bgc             <- uData$bgc             <- bgc(pool, pts$Site, avg, session_params$modelWt, 
                                                  session_params$show_novelty, session_params$nov_c)
  tic("Process CCISS data", ticker)
  cciss           <- uData$cciss           <- cciss(bgc ,session_params$estabWt,session_params$futWt)
  tic("Format CCISS Results", ticker)
  cciss_results   <- uData$cciss_results   <- cciss_results(cciss, bgc, pts, avg, type = as.logical(avg))
  update_flag(update_flag() + 1) ##make sure things recalculate
  # UI select choices
  tic("Determine UI choices", ticker)
  siterefs        <- uData$siterefs        <- sort(unique(bgc$SiteRef))
  ss_opts <- sort(unique(uData$sspreds$SS_NoSpace))
  bgc_opts <- unique(uData$bgc$BGC)
  #prepare tree choices for portfolio selection
  suitTrees <- copy(cciss_results)
  suitTrees <- suitTrees[EstabFeas %in% c(1,2,3,4),.(Spp, BGC = ZoneSubzone)] ##need to fix this
  suitTrees <- unique(suitTrees)
  tree_opts <- suitTrees[BGC == bgc_opts[1],Spp]
  updateSelectInput(inputId = "tree_species",
                    choices = tree_opts,selected = tree_opts)
  uData$tree_opts <- suitTrees
  
  ssl <- lapply(siterefs, function(sr) {
    ss <- sort(unique(cciss_results[SiteRef %in% sr]$SS_NoSpace))
    if(!is.null(uData$ss_list) & isFALSE(avg) & (sr %in% pts$Site)){
      selected_ss <- uData$ss_list[[pts[Site == sr,ID][1]]]
      if(any(selected_ss %in% ss)){
        ss <- selected_ss
      }
    }
    names(ss) <- paste(
      ss,
      N1$SiteSeriesLongName[match(ss, N1$SS_NoSpace)]
    )
    ss
  })
  names(ssl) <- siterefs
  #print(ssl)
  
  ssa <- unique(unname(unlist(ssl)))
  names(ssa) <- paste(
    ssa,
    N1$SiteSeriesLongName[match(ssa, N1$SS_NoSpace)]
  )
  
  
  siteseries_list <- uData$siteseries_list <- ssl
  siteseries_all  <- uData$siteseries_all  <- ssa
  
  if (!isTRUE(avg)) {
    # ordering choices to match order in points table and create a name for each choice
    siterefs <- pts[Site %in% siterefs,
      {x <- Site; names(x) <- paste(ID, Site, sep = " / "); return(x)}
    ]
    uData$siterefs <- siterefs
  }
  
  # Dynamic UI select choices that depends on previous select choice
  #browser() ##issue for HG is that siteseries aren't correct. Need to investigate
  siteref <- head(siterefs, 1)
  siteseries <- siteseries_list[[siteref]]

  tic("Populate UI choices", ticker)
  updateSelectizeInput(inputId = "siteref_feas", choices = siterefs, selected = siteref, server = TRUE)
  updateSelectizeInput(inputId = "siteref_bgc_fut", choices = siterefs, selected = siteref,server = TRUE)
  updateSelectizeInput(inputId = "siteref_bgc_fut_spatial", choices = siterefs, selected = siteref,server = TRUE)
  updateSelectizeInput(inputId = "ss_bgc_fut", choices = siteseries, selected = siteseries[1],server = TRUE)
  updateSelectizeInput(inputId = "siteref_silv", choices = siterefs, selected = siteref,server = TRUE)
  updateSelectizeInput(inputId = "site_series_feas", choices = siteseries, selected = head(siteseries, 1),server = TRUE)
  updateSelectizeInput(inputId = "site_series_silv", choices = siteseries, selected = head(siteseries, 1),server = TRUE)
  updateSelectInput(inputId = "port_bgc", choices = bgc_opts, select = bgc_opts[1])
  if(length(siteseries_all) < 25){ ## app crashes with too many options here
    updateCheckboxGroupInput(inputId = "report_filter",choices = siteseries_all, selected = siteseries_all)
    
  }
  # Use UI injected javascript to show download button and hide generate button
  tic("Inject javascript", ticker)
  session$sendCustomMessage(type="jsCode", list(
    code= "$('#download_report_span').show()"))
  session$sendCustomMessage(type="jsCode", list(
    code= "$('#download_data_span').show()"))
  session$sendCustomMessage(type="jsCode", list(
    code= "$('#generate_results').prop('disabled', true)"))
  updateActionButton(inputId = "generate_results", label = "Refresh results")
  
  tocker <- toc(ticker)
  
  # Render models info + timings in About
  
  output$timings <- plotly::renderPlotly({
    tocker
  })
  #browser()
  print("done generate")
})

generateState <- function() {
  # This prevent the generate button from being enabled when
  # points do not have valid geometry. There is another
  # validation in new_points to make sure the newly
  # added points are located inside the cciss geometry.
  # Only valid points are used to calculated
  if (nrow(userpoints$dt[!is.na(Long) & !is.na(Lat)])) {
    session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', false)"))
  } else {
    session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
  }
}

##put selections into reactive list
observeEvent(input$siteref_feas,{selected_site$siteref <- input$siteref_feas})
observeEvent(input$site_series_feas,{selected_site$ss <- input$site_series_feas})
observeEvent(input$siteref_bgc_fut,{selected_site$siteref <- input$siteref_bgc_fut})
observeEvent(input$ss_bgc_fut,{selected_site$ss <- input$ss_bgc_fut})
observeEvent(input$siteref_silv,{selected_site$siteref <- input$siteref_silv})
observeEvent(input$site_series_silv,{selected_site$ss <- input$site_series_silv})

observe({
  updateSelectInput(session,"siteref_feas",selected = selected_site$siteref)
  updateSelectInput(session,"site_series_feas",selected = selected_site$ss)
  updateSelectInput(session,"siteref_bgc_fut",selected = selected_site$siteref)
  updateSelectInput(session,"ss_bgc_fut",selected = selected_site$ss)
  updateSelectInput(session,"siteref_silv",selected = selected_site$siteref)
  updateSelectInput(session,"site_series_silv",selected = selected_site$ss)
})

# These are the triggers to check if we need to change button state
observeEvent(userpoints$dt, {generateState()})
observeEvent(input$aggregation, {generateState()})
observeEvent(input$rcp_scenario, {generateState()})
#observeEvent(userdata$bgc_dt,{generateState()})

# Data processing
bgc <- function(con, siteno, avg, modWeights, novelty, nov_c = 5) {
  siteno <- siteno[!is.na(siteno)]
  withProgress(message = "Processing...", detail = "Futures", {
    if(novelty){
      dat <- dbGetCCISS_novelty(con, siteno, avg, modWeights = modWeights, nov_cutoff = nov_c)
    } else {
      dat <- dbGetCCISS_v13(con, siteno, avg, modWeights = modWeights)
    }
    
  })
  dat
}

# bgc <- dbGetCCISS(pool,siteno = 676813, avg = F, modWeights = all_weight)
# SSPreds <- edatopicOverlap(bgc, E1, E1_Phase)
# out <- ccissOutput(SSPred = SSPreds, suit = S1, rules = R1, feasFlag = F1,
#             histWeights = c(0.3,0.35,0.35), futureWeights = rep(0.25,4))

# bgc <- sqlTest(pool,siteno = c(6476259,6477778,6691980,6699297),avg = T, scn = "ssp370")


cciss <- function(bgc,estabWt,futWt) {
  if(session_params$show_novelty){
    bgc <- bgc[BGC.pred != "novel",]
  }
  edaOut <- edatopicOverlap(bgc, copy(E1), copy(E1_Phase))
  #browser()
  SSPred <- edaOut$NoPhase
  setorder(SSPred,SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,-SSratio)
  uData$eda_out <- edaOut$phase
  uData$SSPred <- SSPred
  suit <- copy(S1)
  if(!session_params$show_ohr) {
    suit <- suit[(!OHR),]
  }
  ccissOutput(SSPred = SSPred, suit = suit, rules = R1, feasFlag = F1, 
              histWeights = estabWt, futureWeights = futWt)
}


# test <- ccissOutput(SSPred = SSPred, suit = S1, rules = R1, feasFlag = F1,
#                     histWeights = c(0.3,0.3,0.35), futureWeights = rep(0.25,4))
#SSPred2 <- SSPred[SS_NoSpace == "ICHmw1/01",]
# This map is used to determine output labels from raw period
#uData$period_map <- c("1975" = "Historic", "2000" = "Current", "2025" = "2010-2040", "2055" = "2040-2070", "2085" = "2070-2100")
uData$period_map <- c("1961" = "Mapped", "1991" = "2001-2020 (obs)", "2001" = "2001-2020 (mod)", "2021" = "2021-2040", "2041" = "2041-2060", "2061" = "2061-2080","2081" = "2081-2100")

## SVGs for mid rot trend
swap_up_down <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><polyline points="464 208 352 96 240 208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="352" y1="113.13" x2="352" y2="416" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><polyline points="48 304 160 416 272 304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="160" y1="398" x2="160" y2="96" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
trending_up <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 144 464 144 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,368,169.37,246.63a32,32,0,0,1,45.26,0l50.74,50.74a32,32,0,0,0,45.26,0L448,160" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
trending_down <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 368 464 368 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,144,169.37,265.37a32,32,0,0,0,45.26,0l50.74-50.74a32,32,0,0,1,45.26,0L448,352" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
stable <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><line x1="118" y1="304" x2="394" y2="304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/><line x1="118" y1="208" x2="394" y2="208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/></svg>'

##function for creating full results table
cciss_results <- function(cciss, bgc, pts, avg, type, SS = ccissr::stocking_standards, period_map = uData$period_map) {
  withProgress(message = "Processing...", detail = "Feasibility results", {
    # use a copy to avoid modifying the original object
    results <- copy(cciss$Raw)
    sumResults <- copy(cciss$Summary)
    #browser()
    if(session_params$show_novelty){
      bgc_nov <- bgc[BGC.pred == "novel",]
      bgc_nov[,FuturePeriod := as.integer(FuturePeriod)]
      results[bgc_nov, NOV := i.BGC.prop, on = c("SiteRef","FuturePeriod")]
      results[is.na(NOV), NOV := 0]
      results[,`:=`(`1` = `1`*(1 - NOV),
                    `2` = `2`*(1 - NOV),
                    `3` = `3`*(1 - NOV),
                    `X` = `X`*(1 - NOV))]
      results <- dcast(results, SiteRef + SS_NoSpace + Spp + Curr ~ FuturePeriod,
                       value.var = c("NewSuit", "1", "2", "3", "X","NOV"))
      # Required columns, set them if not created by dcast (safety)
      reqj <- c(
        "1_1961","2_1961","3_1961","X_1961", "NewSuit_1961",
        "1_1991","2_1991","3_1991","X_1991", "NewSuit_1991",
        "1_2001","2_2001","3_2001","X_2001", "NewSuit_2001",
        "1_2021","2_2021","3_2021","X_2021", "NewSuit_2021",
        "1_2041","2_2041","3_2041","X_2041", "NewSuit_2041",
        "1_2061","2_2061","3_2061","X_2061", "NewSuit_2061",
        "1_2081","2_2081","3_2081","X_2081", "NewSuit_2081",
        "NOV_1961", "NOV_1991","NOV_2001","NOV_2021","NOV_2041","NOV_2061","NOV_2081"
      )
      set(results, j = reqj[!reqj %in% names(results)], value = NA_real_)
      setnafill(results, fill = 0, cols = c(
        "1_1961","2_1961","3_1961","X_1961",
        "1_1991","2_1991","3_1991","X_1991",
        "1_2001","2_2001","3_2001","X_2001",
        "1_2021","2_2021","3_2021","X_2021",
        "1_2041","2_2041","3_2041","X_2041",
        "1_2061","2_2061","3_2061","X_2061",
        "1_2081","2_2081","3_2081","X_2081",
        "NOV_1961", "NOV_1991","NOV_2001","NOV_2021","NOV_2041","NOV_2061","NOV_2081"
      ))
    } else {
      results <- dcast(results, SiteRef + SS_NoSpace + Spp + Curr ~ FuturePeriod,
                       value.var = c("NewSuit", "1", "2", "3", "X"))
      # Required columns, set them if not created by dcast (safety)
      reqj <- c(
        "1_1961","2_1961","3_1961","X_1961", "NewSuit_1961",
        "1_1991","2_1991","3_1991","X_1991", "NewSuit_1991",
        "1_2001","2_2001","3_2001","X_2001", "NewSuit_2001",
        "1_2021","2_2021","3_2021","X_2021", "NewSuit_2021",
        "1_2041","2_2041","3_2041","X_2041", "NewSuit_2041",
        "1_2061","2_2061","3_2061","X_2061", "NewSuit_2061",
        "1_2081","2_2081","3_2081","X_2081", "NewSuit_2081"
      )
      set(results, j = reqj[!reqj %in% names(results)], value = NA_real_)
      setnafill(results, fill = 0, cols = c(
        "1_1961","2_1961","3_1961","X_1961",
        "1_1991","2_1991","3_1991","X_1991",
        "1_2001","2_2001","3_2001","X_2001",
        "1_2021","2_2021","3_2021","X_2021",
        "1_2041","2_2041","3_2041","X_2041",
        "1_2061","2_2061","3_2061","X_2061",
        "1_2081","2_2081","3_2081","X_2081"
      ))
    }

    # Append region
    if(!type){
      region_map <- pts[[{if (avg) {"BGC"} else {"Site"}}]]
      results$Region <- pts$ForestRegion[match(results$SiteRef, region_map)]
      results$ZoneSubzone <- pts$BGC[match(results$SiteRef, region_map)]
    }else{
      results[,ZoneSubzone := SiteRef]
      results[BGCRegions, Region := i.Region, on = "ZoneSubzone"]
    }
    
    # Append Chief Forester Recommended Suitability
    results[
      SS, 
      `:=`(CFSuitability = as.character(i.Suitability),
           PrefAcc_Orig = i.PreferredAcceptable),
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species")
    ]
    # Append summary vars
    results[
      sumResults, 
      `:=`(EstabFeas = i.NewSuit,
           ccissFeas = i.ccissSuit,
           Improve = i.Improve,
           Decline = i.Decline,
           OrderCol = i.OrderCol,
           IncludeFlag = i.IncludeFlag),
      on = c("SiteRef","SS_NoSpace","Spp")
    ]
    
    results[cfrg_rules,PrefAcc := i.PrefAcc, on = c("Spp",ccissFeas = "Feasible")]
    results[is.na(PrefAcc),PrefAcc := "X"]
    results[,NoPref := if(any(PrefAcc == "P")) T else F, by = .(SS_NoSpace)]
    results[NoPref == F & PrefAcc == "A", PrefAcc := "P"]
    results[,NoPref := NULL]
    results[is.na(PrefAcc_Orig), PrefAcc_Orig := "X"]
    # Append custom generated feasibility svg bars and Trend + ETL
    
    #browser()
    #current = as.integer(names(period_map)[match("Current", period_map)])
    
    if(session_params$show_novelty){
      results[, `:=`(
        Species = T1[Spp, paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")],
        Period = paste0(period_map, collapse = "<br />"),
        ProjFeas = EstabFeas,
        PredFeasSVG = paste0(
          feasibility_svg(`1_1961`,`2_1961`,`3_1961`,`X_1961`,`NOV_1961`), "<br />",
          feasibility_svg(`1_1991`,`2_1991`,`3_1991`,`X_1991`,`NOV_1991`), "<br />",
          feasibility_svg(`1_2001`,`2_2001`,`3_2001`,`X_2001`,`NOV_2001`), "<br />",
          feasibility_svg(`1_2021`,`2_2021`,`3_2021`,`X_2021`,`NOV_2021`), "<br />",
          feasibility_svg(`1_2041`,`2_2041`,`3_2041`,`X_2041`,`NOV_2041`), "<br />",
          feasibility_svg(`1_2061`,`2_2061`,`3_2061`,`X_2061`,`NOV_2061`), "<br />",
          feasibility_svg(`1_2081`,`2_2081`,`3_2081`,`X_2081`,`NOV_2081`)
        )
      )]
    } else {
      results[, `:=`(
        Species = T1[Spp, paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")],
        Period = paste0(period_map, collapse = "<br />"),
        ProjFeas = EstabFeas,
        PredFeasSVG = paste0(
          feasibility_svg_nonov(`1_1961`,`2_1961`,`3_1961`,`X_1961`), "<br />",
          feasibility_svg_nonov(`1_1991`,`2_1991`,`3_1991`,`X_1991`), "<br />",
          feasibility_svg_nonov(`1_2001`,`2_2001`,`3_2001`,`X_2001`), "<br />",
          feasibility_svg_nonov(`1_2021`,`2_2021`,`3_2021`,`X_2021`), "<br />",
          feasibility_svg_nonov(`1_2041`,`2_2041`,`3_2041`,`X_2041`), "<br />",
          feasibility_svg_nonov(`1_2061`,`2_2061`,`3_2061`,`X_2061`), "<br />",
          feasibility_svg_nonov(`1_2081`,`2_2081`,`3_2081`,`X_2081`)
        )
      )]
      
    }
    
    
    results <- results[!is.na(ProjFeas),]
    results[,Curr := as.character(Curr)]
    for(i in c("Curr","EstabFeas","CFSuitability")){ ##set NA to X
      results[is.na(get(i)) | get(i) == 4, (i) := "X"]
    }
    #print(data[,.(CFSuitability,Curr,ccissFeas)])
    results[,OrderCol := CFSuitability]
    results[CFSuitability == '0', OrderCol := "S"]
    results[CFSuitability == "X" & Curr == "X" 
         & ccissFeas %in% c(1,2,3), EstabFeas := "Trial"]
    setorder(results, SiteRef, SS_NoSpace, OrderCol, -Improve, na.last = TRUE)
    return(results)
  })
}

feasibility_svg <- function(..., width = 220L, height = 18L, colors = c("limegreen", "deepskyblue", "gold", "grey","black")) {
  x <- list(...)
  #browser()
  col_x <- length(x)
  x <- matrix(unlist(x), ncol = col_x)
  row_x <- nrow(x)
  row_cumsums <- matrixStats::rowCumsums(x)
  # When cumsum is zero at X just output a 100% grey bar
  x[which(row_cumsums[,4L] == 0L), 4L] <- 1L
  pos_x <- row_cumsums
  pos_x[, 1L] <- 0L 
  pos_x[, 2L:5L] <- row_cumsums[, 1L:4L] * width
  width_el <- x * width
  pos_text <- width_el / 2 + pos_x
  xdt <- data.table("x" = x, "pos_x" = pos_x, "width_el" = width_el, "pos_text" = pos_text)
  xdt[,paste0(
    '<svg viewBox="0 0 ', width,' ', height,'" x="0px" y="0px" width="', width,'px" height="', height,'px">',
    pfsvg(x.V1, pos_x.V1, width_el.V1, pos_text.V1, height, colors[1L]),
    pfsvg(x.V2, pos_x.V2, width_el.V2, pos_text.V2, height, colors[2L]),
    pfsvg(x.V3, pos_x.V3, width_el.V3, pos_text.V3, height, colors[3L]),
    pfsvg(x.V4, pos_x.V4, width_el.V4, pos_text.V4, height, colors[4L]),
    pfsvg(x.V5, pos_x.V5, width_el.V5, pos_text.V5, height, colors[5L]),
    '</svg>'
  )]
}

feasibility_svg_nonov <- function(..., width = 220L, height = 18L, colors = c("limegreen", "deepskyblue", "gold", "grey")) {
  x <- list(...)
  #browser()
  col_x <- length(x)
  x <- matrix(unlist(x), ncol = col_x)
  row_x <- nrow(x)
  row_cumsums <- matrixStats::rowCumsums(x)
  # When cumsum is zero at X just output a 100% grey bar
  x[which(row_cumsums[,4L] == 0L), 4L] <- 1L
  pos_x <- row_cumsums
  pos_x[, 1L] <- 0L 
  pos_x[, 2L:4L] <- row_cumsums[, 1L:3L] * width
  width_el <- x * width
  pos_text <- width_el / 2 + pos_x
  xdt <- data.table("x" = x, "pos_x" = pos_x, "width_el" = width_el, "pos_text" = pos_text)
  xdt[,paste0(
    '<svg viewBox="0 0 ', width,' ', height,'" x="0px" y="0px" width="', width,'px" height="', height,'px">',
    pfsvg(x.V1, pos_x.V1, width_el.V1, pos_text.V1, height, colors[1L]),
    pfsvg(x.V2, pos_x.V2, width_el.V2, pos_text.V2, height, colors[2L]),
    pfsvg(x.V3, pos_x.V3, width_el.V3, pos_text.V3, height, colors[3L]),
    pfsvg(x.V4, pos_x.V4, width_el.V4, pos_text.V4, height, colors[4L]),
    '</svg>'
  )]
}

uData$feasibility_svg <- feasibility_svg

pfsvg <- function(x, pos_x, width_el, pos_text, height, color) {
  # Format svg text
  xtxt <- paste0(round(100*x), "%")
  # Avoid printing values lower than 7.5% as they are unreadable
  xtxt[which(x < 0.065)] <- ""
  svgs <- rep("", length.out = length(x))
  gzw <- width_el > 0
  svgs[gzw] <- paste0(
    '<rect x="', pos_x[gzw], '" y="0" width="', width_el[gzw], '" height="', height,
    '" style="fill: ', color, '" /><text text-anchor="middle" style="font: 600 ', height / 2 + 2,
    'px Arial" x="', pos_text[gzw], '" y="', height * 0.75, '">', xtxt[gzw], '</text>'
  )
  svgs
}
uData$pfsvg <- pfsvg

# Timings functions to build the "donut"
tic <- function(split = "unnamed block", var = numeric()) {
  name <- substitute(var)
  var <- c(var, `names<-`(.Internal(Sys.time()), split))
  if (is.name(name)) {
    name <- as.character(name)
    assign(name, var, parent.frame(), inherits = TRUE)
  }
  return(invisible(var))
}

toc <- function(var) {
  # timings in milliseconds
  timings <- (c(var, .Internal(Sys.time()))[-1] - var) * 1000L
  df <- data.frame(split = names(var), timings = timings)
  # the donut plot
  plotly::plot_ly(data = df, labels = ~split, values = ~timings,
                  textposition = 'inside',
                  texttemplate = "%{value:.0f} ms",
                  hovertemplate = "<extra></extra>%{label}") %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::add_annotations(text = paste(round(sum(timings), 0), "ms"),
                            showarrow = FALSE, yanchor = "middle", xanchor = "middle",
                            font = list(size = 40)) %>%
    plotly::layout(title = "", showlegend = FALSE,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}

