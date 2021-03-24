observeEvent(input$generate_results, priority = 100, {
  
  ticker <- tic("Save Map Inputs")
  # On generate click, we are taking a snapshot of the current points
  # and calculating results. All relevant results will be stored in the
  # userdata environment for further reuse. User has the ability to update
  # results on demand instead of on app state change. This reduce the load
  # on the app and give some room in case computation get more costly
  # in the future.
  
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
    ss <- sort(unique(cciss_results[SiteRef %in% sr]$`Site Series`))
    names(ss) <- paste(
      ss,
      stocking_info$SiteSeriesName[match(ss, stocking_info[, paste(ZoneSubzone, SiteSeries, sep = "/")])]
    )
    ss
  })
  names(ssl) <- siterefs
  
  ssa <- sort(unique(cciss_results$`Site Series`))
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
  
  # Use ui injected javascript to show download button and hide generate button
  tic("Inject javascript", ticker)
  session$sendCustomMessage(type="jsCode", list(code= "$('#download_span').show()"))
  session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
  updateActionButton(inputId = "generate_results", label = "Refresh results")
  
  tocker <- toc(ticker)
  
  # Render models info + timings in About
  output$modelsinfo <- renderTable({models_info}, )
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

observeEvent(userpoints$dt, {generateState()})
observeEvent(input$aggregation, {generateState()})
observeEvent(input$rcp_scenario, {generateState()})

# Data
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

cciss_summary <- function(cciss, pts, avg, SS = bccciss::stocking_standards) {
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
      CFRS := i.Suitability,
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species")
    ]
    summary$Curr <- as.character(summary$CFRS)
    # Replaces 4 and NA with X
    summary[is.na(Curr), Curr := "X"]
    summary$NewSuit <- as.character(summary$NewSuit)
    summary[NewSuit > "3", NewSuit := "X"]
    # Removing these columns as we will use the one in the results cciss
    # to determine silviculture information
    summary$Region <- NULL
    summary$ZoneSubzone <- NULL
    summary$CFRS <- NULL
    # Add Tree english name
    summary[, Spp := T1[Spp, paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")]]
    # Order
    setorder(summary, SiteRef, NewSuit, Curr, Spp)
    # Rename columns
    setnames(
      summary,
      names(summary),
      c("SiteRef", "Site Series", "Tree Species",
        "Chief Forester Recommended Suitability", "Projected Feasibility", "Flag",
        "Projected Feasibility 2025", "Fail Risk 2025",
        "Projected Feasibility 2055", "Fail Risk 2055",
        "Projected Feasibility 2085", "Fail Risk 2085")
    )
    return(summary)
  })
}

uData$period_map <- c("1975" = "Historic", "2000" = "Current", "2025" = "2010-2040", "2055" = "2040-2070", "2085" = "2070-2100")

cciss_results <- function(cciss, pts, avg, SS = bccciss::stocking_standards, period_map = uData$period_map) {
  withProgress(message = "Processing...", detail = "Feasibility results", {
    # use a copy to avoid modifying the original object
    results <- copy(cciss$Raw)
    # Append region
    region_map <- pts[[{if (avg) {"BGC"} else {"Site"}}]]
    results$Region <- pts$ForestRegion[match(results$SiteRef, region_map)]
    results$ZoneSubzone <- pts$BGC[match(results$SiteRef, region_map)]
    # Append Chief Forester Recommended Suitability
    results[
      SS, 
      CFRS := as.character(i.Suitability),
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species")
    ]
    # Replaces 4 and NA with X
    results[is.na(CFRS), CFRS := "X"]
    # Append visuals
    current = 2000
    results[, `:=`(
      NewSuitRound = round(NewSuit, 0),
      SuitDiff = round(SuitDiff, 0),
      FeasSVG = feasibility_svg(`1`, `2`, `3`, `X`),
      Period = period_map[as.character(FuturePeriod)]
    )]
    default_svg <- feasibility_svg(0,0,0,1)
    setorder(results, SiteRef, SS_NoSpace, Spp, FuturePeriod)
    results <- results[, list(
      "Site Series" = SS_NoSpace,
      "Tree Species" = T1[unique(Spp), paste(paste0("<b>", TreeCode, "</b>"), EnglishName, sep = ": ")],
      "&nbsp;&nbsp;Period&nbsp;&nbsp;" = paste(period_map, collapse = "<br/>"),
      "Predicted Feasibility" =  {
        svgs <- FeasSVG[match(period_map, Period)]
        svgs[is.na(svgs)] <- default_svg
        paste(svgs, collapse = "<br/>")
      },
      # Use silviculture
      "Chief Forester Recommended Suitability" = {
        cfr <- CFRS[FuturePeriod == current]
        if (length(cfr)) cfr else "X"
      },
      "Projected Feasibility" = {
        pf <- NewSuitRound[FuturePeriod == current]
        pf <- as.character(pf)
        if (length(pf)) pf else "X"
      },
      # TODO :
      # Validate trend logic
      "Continuing Trend at Mid Rotation (2040-2070)" = feasibility_trend(NewSuitRound),
      MeanSuit = mean(NewSuit, na.rm = TRUE),
      OrderSuit = {
        s <- NewSuit[FuturePeriod == current]
        if (length(s)) s else 4
      }
    ), by=c("Region", "ZoneSubzone", "SiteRef", "SS_NoSpace", "Spp")]
    setorder(results, SiteRef, SS_NoSpace, OrderSuit, MeanSuit, `Tree Species`)
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
feasibility_svg <- function(..., width = 220, height = 14, colors = c("limegreen", "deepskyblue", "gold", "grey")) {
  # TODO
  # Maybe optimize by using a fixed template?
  x <- list(...)
  col_x <- length(x)
  x <- matrix(unlist(x), ncol = col_x)
  row_x <- nrow(x)
  row_cumsums <- matrixStats::rowCumsums(x)
  if (row_x <= 1) {
    pos_x <- c(row_x, row_cumsums[,-col_x]) * width
  } else {
    pos_x <- cbind(integer(row_x), row_cumsums[,-col_x]) * width
  }
  width_el <- x * width
  pos_text <- width_el / 2 + pos_x
  # Format svg text
  xtxt <- paste0(round(100*x,0), "%")
  # Avoid printing values lower than 7.5% as they are unreadable
  xtxt[which(unlist(x) < 0.065)] <- ""
  svg <- paste0('<rect x="', pos_x, '" y="0" width="', width_el, '" height="', height,'" style="fill: ', rep(colors, each = row_x), '" /><text text-anchor="middle" style="font: 600 ', height / 2 + 2, 'px Arial" x="', pos_text, '" y="', height * 0.75, '">', xtxt, '</text>')
  svg <- vapply(1:row_x, function(i) {
    paste0('<svg viewBox="0 0 ', width,' ', height,'" x="0px" y="0px" width="', width,'px" height="', height,'px">',
           # Also drop 0 width rect
           paste0(svg[seq(i, by = row_x, length.out = col_x)][width_el[i, ]>0], collapse = ""),
           '</svg>')
  }, character(1))
  return(svg)
}

# Replace trend image with svg so they can be embedded
reorder_two <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><line x1="118" y1="304" x2="394" y2="304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/><line x1="118" y1="208" x2="394" y2="208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:44px"/></svg>'

swap_vertical <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><polyline points="464 208 352 96 240 208" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="352" y1="113.13" x2="352" y2="416" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><polyline points="48 304 160 416 272 304" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><line x1="160" y1="398" x2="160" y2="96" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'

trending_down <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 368 464 368 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,144,169.37,265.37a32,32,0,0,0,45.26,0l50.74-50.74a32,32,0,0,1,45.26,0L448,352" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'

trending_up <- '<svg xmlns="http://www.w3.org/2000/svg" width="30px" height="30px" viewBox="0 0 512 512"><title>ionicons-v5-c</title><polyline points="352 144 464 144 464 256" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/><path d="M48,368,169.37,246.63a32,32,0,0,1,45.26,0l50.74,50.74a32,32,0,0,0,45.26,0L448,160" style="fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px"/></svg>'

trends <- list(swap_vertical, trending_up, trending_down, reorder_two)

#' Return a feasibility trend icon
#' @param x A numeric vector.
#' @return a trend icon
feasibility_trend <- function(x) {
  mod <- head(x, -1) - tail(x, -1)
  if (any(mod > 0) & any(mod < 0)) {
    return(trends[[1]])
  } else if (any(mod > 0)) {
    return(trends[[2]])
  } else if (any(mod < 0)) {
    return(trends[[3]])
  }
  return(trends[[4]])
}

# Timings functions
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
  # timings into milliseconds
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

