# Data

bgc_react <- reactive({
  avg <- input$aggregation
  rcp <- input$rcp_scenario
  # to invalidate reactive with points table change
  # and force this value and all others that depends
  # on it to recompute
  pts_react <- input$points_table_rows_all
  
  pts <- uData$points[!is.na(Long) & !is.na(Lat), list(Long, Lat)]
  if (nrow(pts) == 0) return(NULL)
  res <- dbGetCCISS(pool, pts, as.logical(avg), rcp)
  return(res[FuturePeriod > "1975"])
})

cciss_react <- reactive({
  bgc <- bgc_react()
  if (is.null(bgc)) return(NULL)
  SSPred <- edatopicOverlap(bgc, Edatope = E1)
  ccissOutput(SSPred = SSPred, suit = S1, rules = R1, feasFlag = F1)
})

cciss_summary <- reactive({
  cciss <- cciss_react()
  if (is.null(cciss)) return(NULL)
  cciss_summary <- cciss$Summary
  # TODO
  # Validate how they want the table to look like
  cciss_summary[, Spp := T1[Spp, paste(TreeCode, EnglishName, sep = ": ")]]
  setnames(
    cciss_summary,
    names(cciss_summary),
    # Using &nbsp; as using DT::datatable options to set column width breaks display
    c("SiteRef", "Site Series", "Tree Species",
      "Chief Forester Recommended Suitability", "Projected Feasibility", "Flag",
      "Projected Feasibility 2025", "Fail Risk 2025",
      "Projected Feasibility 2055", "Fail Risk 2055",
      "Projected Feasibility 2085", "Fail Risk 2085")
  )
  return(cciss_summary)
})

cciss_raw <- reactive({
  cciss <- cciss_react()
  if (is.null(cciss)) return(NULL)
  cciss_raw <- cciss$Raw
  period_map <- c("2000" = "Current", "2025" = "2010-2040", "2055" = "2040-2070", "2085" = "2070-2100")
  cciss_raw[, `:=`(
    NewSuit = round(NewSuit, 0),
    SuitDiff = round(SuitDiff, 0),
    SuitSVG = suitability_svg(`1`, `2`, `3`, `X`),
    Period = period_map[as.character(FuturePeriod)])
  ]
  setorder(cciss_raw, SiteRef, SS_NoSpace, Spp, FuturePeriod)
  cciss_raw <- cciss_raw[, list(
    "Site Series" = SS_NoSpace,
    "Tree Species" = T1[unique(Spp), paste(TreeCode, EnglishName, sep = ": ")],
    "&nbsp;&nbsp;Period&nbsp;&nbsp;" = paste(Period, collapse = "<br/>"),
    "Predicted Suitability" =  paste(SuitSVG, collapse = "<br/>"),
    "Chief Forester Recommended Suitability" = Curr[FuturePeriod == 2000],
    # TODO :
    # How do you determine projected feasibility and what logic drive round flag color vs SuitDiff
    "Projected Feasibility" = NewSuit[FuturePeriod == 2000],
    # TODO :
    # Validate trend logic
    "Continuing Trend at Mid Rotation (2040-2070)" = suitability_trend(NewSuit),
    ModAgree = mean(ModAgree, na.rm = TRUE)
  ), by=c("SiteRef", "SS_NoSpace", "Spp")]
  # some do not have a current value, setting it to 4
  cciss_raw[is.na(`Chief Forester Recommended Suitability`), `Chief Forester Recommended Suitability` := 4]
  cciss_raw[is.na(`Projected Feasibility`), `Projected Feasibility` := 4]
  setorder(cciss_raw, SiteRef, SS_NoSpace, `Chief Forester Recommended Suitability`, `Projected Feasibility`, -ModAgree)
  cciss_raw[, c("Spp", "SS_NoSpace", "ModAgree") := NULL]
  cciss_raw
})

# Graph

#' @param data BGC data.table
cm_bcg_fplot <- function(data) {
  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    orientation = 'h',
    y = 1.1,
    x = -0.05)
  color_ref <- {
    colors <- subzones_colours_ref[unique(data$BGC.pred)]
    col <- colors$colour
    names(col) <- colors$classification
    col
  }
  plotly::plot_ly(data = data, x = ~FuturePeriod,
                  y = ~BGC.prop, split = ~BGC.pred, type = 'bar',
                  color = ~BGC.pred, colors = color_ref) %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   barmode = 'stack', legend = l, hovermode = 'x unified')
}

#' @param ... a list of numeric vector, column names will be used as color. This
#' function assumes that x rowSums are all equal to 1 and that there is no NA values.
#' @param width output width of svg
#' @param height output height of svg
#' @param colors character vector of colors to use for svg, same length as
#' ncol x.
#' @return an svg image of suitability prediction, one per row in data.frame
suitability_svg <- function(..., width = 200, height = 14, colors = c("limegreen", "deepskyblue", "gold", "grey")) {
  x <- list(...)
  col_x <- length(x)
  x <- matrix(unlist(x), ncol = col_x)
  row_x <- nrow(x)
  row_cumsums <- matrixStats::rowCumsums(x)
  pos_x <- cbind(integer(row_x), row_cumsums[,-col_x]) * width
  width_el <- x * width
  pos_text <- width_el / 2 + pos_x
  svg <- paste0('<rect x="', pos_x, '" y="0" width="', width_el, '" height="', height,'" style="fill: ', rep(colors, each = row_x), '"/><text text-anchor="middle" style="font: 600 ', height / 2 + 2, 'px Arial" x="', pos_text, '" y="', height * 0.75, '">', round(x*100,1), '%</text>')
  svg <- vapply(1:row_x, function(i) {
    paste0('<svg viewBox="0 0 ', width,' ', height,'" x="0px" y="0px" width="', width,'px" height="', height,'px">',
           paste0(svg[seq(i, by = row_x, length.out = col_x)], collapse = ""),
           '</svg>')
  }, character(1))
  return(svg)
}

suitability_legend <- function(text = c("Primary", "Secondary", "Tertiary", "Not Suitable"),
                               height = 14,
                               colors = c("limegreen", "deepskyblue", "gold", "grey")) {
  HTML(
    paste0(
      '<svg viewBox="0 0 1 1" height="',
      height,
      'px" width="',
      height,
      'px"><rect height=1 width=1 style="fill : ',
      colors,
      '"><span>&nbsp;',
      text,
      '</span>',
      collapse = "<br />"
    )
  )
}

#' Return a suitability trend icon
#' @param x A numeric vector.
#' @return a trend icon
suitability_trend <- function(x) {
  trends <- paste0('<img src="www/', c("swap-vertical", "trending-up", "trending-down", "reorder-two"),'.svg" width="30px" height="30px" />')
  mod <- head(x, -1) - tail(x, -1)
  if (any(mod > 0) & any(mod < 0)) {
    return(trends[1])
  } else if (any(mod > 0)) {
    return(trends[2])
  } else if (any(mod < 0)) {
    return(trends[3])
  }
  return(trends[4])
}

#' Return a suitability flag icon
#' @param x A numeric vector.
#' @return a flag icon
suitability_flag <- function(x) {
}

# UI elements

radio_select_siteref <- function(subid) {
  renderUI({
    b <- bgc_react()
    avg <- input$aggregation
    rcp <- paste(input$rcp_scenario, collapse = ", ")
    if (is.null(b)) return(span("Add points to generate selection menu."))
    choices <- unique(b$SiteRef)
    if (!isTRUE(as.logical(avg))) {
      IDs <- uData$points$ID[match(choices, uData$points$Site)]
      IDs[is.na(IDs)] <- seq_len(length(IDs))[is.na(IDs)]
      names(choices) <- paste(choices, IDs, sep = "/")
    }
    list(
      radioButtons(paste("current_siteref", subid, sep = "_"), label = "Results:", choices = choices),
      span("Averaged: ", br(), avg, br(), br(),
           "RCP scenratio: ", br(), rcp)
    )
  })
}
