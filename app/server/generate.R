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
  avg             <- uData$avg             <- as.logical(input$aggregation)
  rcp             <- uData$rcp             <- input$rcp_scenario
  pts             <- uData$pts             <- userpoints$dt
  
  # Results from processing
  tic("Fetch CCISS Data from DB", ticker)
  bgc             <- uData$bgc             <- bgc(pool, pts$Site, avg, rcp)
  tic("Process CCISS data", ticker)
  cciss           <- uData$cciss           <- cciss(bgc)
  tic("Format CCISS Results", ticker)
  cciss_results   <- uData$cciss_results   <- cciss_results(cciss, pts, avg)
  tic("Format CCISS Summary", ticker)
  cciss_summary   <- uData$cciss_summary   <- cciss_summary(cciss, pts, avg)
  
  
  # UI select choices
  tic("Determine UI choices", ticker)
  siterefs        <- uData$siterefs        <- sort(unique(bgc$SiteRef))
  
  ssl <- lapply(siterefs, function(sr) {
    ss <- sort(unique(cciss_results[SiteRef %in% sr]$SS_NoSpace))
    names(ss) <- paste(
      ss,
      stocking_info$SiteSeriesName[match(ss, stocking_info[, paste(ZoneSubzone, SiteSeries, sep = "/")])]
    )
    ss
  })
  names(ssl) <- siterefs
  
  ssa <- sort(unique(cciss_results$SS_NoSpace))
  names(ssa) <- paste(
    ssa,
    stocking_info$SiteSeriesName[match(ssa, stocking_info[, paste(ZoneSubzone, SiteSeries, sep = "/")])]
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
  siteref <- head(siterefs, 1)
  siteseries <- siteseries_list[[siteref]]

  tic("Populate UI choices", ticker)
  updateSelectInput(inputId = "siteref_feas", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "siteref_bgc_fut", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "siteref_silv", choices = siterefs, selected = siteref)
  updateSelectInput(inputId = "site_series_feas", choices = siteseries, selected = head(siteseries, 1))
  updateSelectInput(inputId = "site_series_silv", choices = siteseries, selected = head(siteseries, 1))
  updateCheckboxGroupInput(inputId = "report_filter",choices = siteseries_all, selected = siteseries_all)
  
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
  output$modelsinfo <- function() {
    knitr::kable(models_info, format = "html", table.attr = 'class="table table-hover table-centered"') 
  }
  output$timings <- plotly::renderPlotly({
    tocker
  })
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

# These are the triggers to check if we need to change button state
observeEvent(userpoints$dt, {generateState()})
observeEvent(input$aggregation, {generateState()})
observeEvent(input$rcp_scenario, {generateState()})

# Data processing
bgc <- function(con, siteno, avg, rcp) {
  siteno <- siteno[!is.na(siteno)]
  withProgress(message = "Processing...", detail = "Futures", {
    dbGetCCISS(con, siteno, avg, rcp)
  })
}

cciss <- function(bgc) {
  SSPred <- edatopicOverlap(bgc, Edatope = E1)
  ccissOutput(SSPred = SSPred, suit = S1, rules = R1, feasFlag = F1)
}

cciss_summary <- function(cciss, pts, avg, SS = bccciss::stocking_standards, period_map = uData$period_map) {
  withProgress(message = "Processing...", detail = "Feasibility summary", {
    # use a copy to avoid modifying the original object
    summary <- copy(cciss$Summary)
    # Append region
    region_map <- pts[[{if (avg) {"BGC"} else {"Site"}}]]
    summary$Region <- pts$ForestRegion[match(summary$SiteRef, region_map)]
    summary$ZoneSubzone <- pts$BGC[match(summary$SiteRef, region_map)]
    # Append Chief Forester Recommended Suitability
    summary[
      SS, 
      CFSuitability := as.character(i.Suitability),
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species"),
    ]
    summary[is.na(CFSuitability), CFSuitability := "X"]
    current = names(period_map)[match("Current", period_map)]
    # Format for printing
    summary[, `:=`(
      Species = T1[Spp, paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")],
      ProjFeas = NewSuit,
      Period = paste0(period_map[names(period_map) > current], collapse = "<br />"),
      FutProjFeas = paste0(Suit2025, "<br />", Suit2055, "<br />", Suit2085),
      FailRisk = paste0(FailRisk2025, "<br />", FailRisk2055, "<br />", FailRisk2085)
    )]
    # Order
    setorder(summary, SiteRef, NewSuit, Curr, Spp)
    return(summary)
  })
}

# This map is used to determine output labels from raw period
uData$period_map <- c("1975" = "Historic", "2000" = "Current", "2025" = "2010-2040", "2055" = "2040-2070", "2085" = "2070-2100")

cciss_results <- function(cciss, pts, avg, SS = bccciss::stocking_standards, period_map = uData$period_map) {
  withProgress(message = "Processing...", detail = "Feasibility results", {
    # use a copy to avoid modifying the original object
    results <- copy(cciss$Raw)
    # dcast (pivot)
    results <- dcast(results, SiteRef + SS_NoSpace + Spp ~ FuturePeriod,
                     value.var = c("Curr", "NewSuit", "1", "2", "3", "X", "ModAgree", "SuitDiff"))
    # Required columns, set them if not created by dcast (safety)
    reqj <- c(
      "1_1975","2_1975","3_1975","X_1975", "NewSuit_1975",
      "1_2000","2_2000","3_2000","X_2000", "NewSuit_2000",
      "1_2025","2_2025","3_2025","X_2025", "NewSuit_2025",
      "1_2055","2_2055","3_2055","X_2055", "NewSuit_2055",
      "1_2085","2_2085","3_2085","X_2085", "NewSuit_2085"
    )
    set(results, j = reqj[!reqj %in% names(results)], value = NA_real_)
    setnafill(results, fill = 0, cols = c(
      "1_1975","2_1975","3_1975","X_1975",
      "1_2000","2_2000","3_2000","X_2000",
      "1_2025","2_2025","3_2025","X_2025",
      "1_2055","2_2055","3_2055","X_2055",
      "1_2085","2_2085","3_2085","X_2085"
    ))
    # Append region
    region_map <- pts[[{if (avg) {"BGC"} else {"Site"}}]]
    results$Region <- pts$ForestRegion[match(results$SiteRef, region_map)]
    results$ZoneSubzone <- pts$BGC[match(results$SiteRef, region_map)]
    # Append Chief Forester Recommended Suitability
    results[
      SS, 
      CFSuitability := as.character(i.Suitability),
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species")
    ]
    results[is.na(CFSuitability), CFSuitability := "X"]
    # Append custom generated feasibility svg bars and Trend + ETL
    current = as.integer(names(period_map)[match("Current", period_map)])
    results[, `:=`(
      Species = T1[Spp, paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")],
      Period = paste0(period_map, collapse = "<br />"),
      PredFeasSVG = paste0(
        feasibility_svg(`1_1975`,`2_1975`,`3_1975`,`X_1975`), "<br />",
        feasibility_svg(`1_2000`,`2_2000`,`3_2000`,`X_2000`), "<br />",
        feasibility_svg(`1_2025`,`2_2025`,`3_2025`,`X_2025`), "<br />",
        feasibility_svg(`1_2055`,`2_2055`,`3_2055`,`X_2055`), "<br />",
        feasibility_svg(`1_2085`,`2_2085`,`3_2085`,`X_2085`)
      ),
      ProjFeas = {
        x <- as.character(round(NewSuit_2000))
        x[x %in% c(NA, "4")] <- "X"
        x
      },
      MidRotTrend = feasibility_trend(data.table("T1" = NewSuit_2000, "T2" = NewSuit_2025,
                                                 "T3" = NewSuit_2055, "T4" = NewSuit_2085)),
      MeanSuit = rowMeans(data.table(NewSuit_2000, NewSuit_2025, NewSuit_2055, NewSuit_2085), na.rm = TRUE)
    )]
    setorder(results, SiteRef, SS_NoSpace, NewSuit_2000, MeanSuit, na.last = TRUE)
    return(results)
  })
}

#' @param ... a list of numeric vector, column names will be used as color. This
#' function assumes that x rowSums are all equal to 1 and that there is no NA values.
#' @param width output width of svg
#' @param height output height of svg
#' @param colors character vector of colors to use for svg, same length as
#' ncol x.
#' @return an svg image of feasibility prediction, one per row in data.frame
feasibility_svg <- function(..., width = 220L, height = 14L, colors = c("limegreen", "deepskyblue", "gold", "grey")) {
  x <- list(...)
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

# Replace trend image with svg so they can be embedded
swap_up_down <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><polyline points="464 208 352 96 240 208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="352" y1="113.13" x2="352" y2="416" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><polyline points="48 304 160 416 272 304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="160" y1="398" x2="160" y2="96" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
trending_up <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 144 464 144 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,368,169.37,246.63a32,32,0,0,1,45.26,0l50.74,50.74a32,32,0,0,0,45.26,0L448,160" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
trending_down <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 368 464 368 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,144,169.37,265.37a32,32,0,0,0,45.26,0l50.74-50.74a32,32,0,0,1,45.26,0L448,352" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'
stable <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><line x1="118" y1="304" x2="394" y2="304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/><line x1="118" y1="208" x2="394" y2="208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/></svg>'
trends <- c(swap_up_down, trending_up, trending_down, stable)

#' Return a feasibility trend icon
#' @param x A data.table.
#' @return a trend icon
feasibility_trend <- function(x) {
  # shifted compare mod = Xi+1 - Xi
  mod <- x[, -4] - x[, -1]
  trend <- apply(mod, 1, function(x) {
    if (isTRUE(any(x > 0) & any(x < 0))) {
      return(1L)
      # Increase no decrease
    } else if (isTRUE(any(x > 0))) {
      return(2L)
      # Decrease no increanse
    } else if (isTRUE(any(x < 0))) {
      return(3L)
    }
    # Neither increase nor decrease
    return(4L)
  })
  trends[trend]
}

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

