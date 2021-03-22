# Reusing Shiny session userData environment
uData <- session$userData

# Points dataframe
uData$basepoints <- data.table(
  ID = character(),
  Site = character(),
  Lat = numeric(),
  Long = numeric(),
  Elev = numeric(),
  BGC = character(),
  ForestRegion = character(),
  popups = character()
)

# All columns indexes
uData$pts_col <- 1L:ncol(uData$basepoints)
# Exclude popups column, column indexes to show in the UI
uData$pts_show_col <- 1L:(ncol(uData$basepoints) - 2L)

userpoints <- reactiveValues(dt = uData$basepoints)

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
    # Removing these columns as we will use the one in the detailed cciss
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

period_map <- c("1975" = "Historic", "2000" = "Current", "2025" = "2010-2040", "2055" = "2040-2070", "2085" = "2070-2100")

cciss_detailed <- function(cciss, pts, avg, SS = bccciss::stocking_standards) {
  withProgress(message = "Processing...", detail = "Feasibility detailed", {
    # use a copy to avoid modifying the original object
    detailed <- copy(cciss$Raw)
    # Append region
    region_map <- pts[[{if (avg) {"BGC"} else {"Site"}}]]
    detailed$Region <- pts$ForestRegion[match(detailed$SiteRef, region_map)]
    detailed$ZoneSubzone <- pts$BGC[match(detailed$SiteRef, region_map)]
    # Append Chief Forester Recommended Suitability
    detailed[
      SS, 
      CFRS := as.character(i.Suitability),
      on = c(Region = "Region", ZoneSubzone = "ZoneSubzone", SS_NoSpace = "SS_NoSpace", Spp = "Species")
    ]
    # Replaces 4 and NA with X
    detailed[is.na(CFRS), CFRS := "X"]
    # Append visuals
    current = 2000
    detailed[, `:=`(
      NewSuitRound = round(NewSuit, 0),
      SuitDiff = round(SuitDiff, 0),
      FeasSVG = feasibility_svg(`1`, `2`, `3`, `X`),
      Period = period_map[as.character(FuturePeriod)]
    )]
    default_svg <- feasibility_svg(0,0,0,1)
    setorder(detailed, SiteRef, SS_NoSpace, Spp, FuturePeriod)
    detailed <- detailed[, list(
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
    setorder(detailed, SiteRef, SS_NoSpace, OrderSuit, MeanSuit, `Tree Species`)
    return(detailed)
  })
}

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data) {
  l <- list(
    font = list(
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
                  color = ~BGC.pred, colors = color_ref,
                  text = ~BGC.pred, textposition = 'inside', textfont = list(color = "black", size = 20),
                  texttemplate = "%{text}", hovertemplate = "%{y}") %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                ticktext = unname(period_map),
                                tickvals = names(period_map)),
                   barmode = 'stack', legend = l, hovermode = "x unified")
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
  # Maybe optimize
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

#' Return a feasibility trend icon
#' @param x A numeric vector.
#' @return a trend icon
feasibility_trend <- function(x) {
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

# Silviculture UI Element

# Function to format Species with footnotes
sppnotes <- function(spp, notes) {
  ret <- vector("list", length(spp))
  for (i in seq_len(length(spp))) {
    fn <- paste0(sort(as.integer(unique(unlist(notes[i])))), collapse = ",")
    ret[[i]] <- tags$span(spp[i], tags$sup(fn, .noWS = htmltools:::noWSOptions), if (i < length(spp)) {", "} else {""}, .noWS = htmltools:::noWSOptions)
  }
  do.call(span, ret)
}

# Function to create a Standards block for each Standard in the site serie
standardblocks <- function(siteref, siteserie, cciss_detailed) {
  sc <- cciss_detailed[
    SiteRef %in% siteref & SS_NoSpace %in% siteserie,
    c("Region", "ZoneSubzone", "SS_NoSpace", "Projected Feasibility", "Spp")
  ]
  sc[, pf := `Projected Feasibility`]
  ss <- stocking_standards[
    Region %in% sc$Region & ZoneSubzone %in% sc$ZoneSubzone & SS_NoSpace %in% sc$SS_NoSpace
  ]
  do.call(span, lapply(unique(ss$Standard), standardblock, ss = ss, sc = sc))
}

# Function to create a formatted Standard block
standardblock <- function(std, ss, sc) {
  ss <- ss[Standard %in% std]
  sc <- sc[Spp %in% ss$Species]
  si <- stocking_info[Standard == std]
  sh <- stocking_height[Standard == std]
  list(
    tags$small("Site Series"),
    tags$p(tags$b(paste(si$ZoneSubzone, si$SiteSeries, sep = "/"), si$SiteSeriesName)),
    tags$small("Forest Region"),
    tags$p(tags$b(si$Region)),
    splitLayout(
      cellArgs = list(style = "overflow:visible"),
      div(
        tags$small(tags$b("Regeneration")),
        tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
        splitLayout(
          span(
            tags$span(tags$b("Standards ID")), tags$br(),
            tags$span("Primary"), tags$br(),
            tags$span("Preferred (p)"), tags$br(),
            tags$span("Secondary"), tags$br(),
            tags$span("Acceptable (a)"), tags$br(),
            tags$span("Tertiary")
          ),
          span(
            tags$span(tags$b(paste(ss[!is.na(Standard), unique(Standard)], collapse = ", "))), tags$br(),
            ss[!is.na(Species) & Suitability %in% 1L, sppnotes(Species, Footnotes)], tags$br(),
            ss[!is.na(Species) & PreferredAcceptable %in% "P", sppnotes(Species, Footnotes)], tags$br(),
            ss[!is.na(Species) & Suitability %in% 2L, sppnotes(Species, Footnotes)], tags$br(),
            ss[!is.na(Species) & PreferredAcceptable %in% "A", sppnotes(Species, Footnotes)], tags$br(),
            ss[!is.na(Species) & Suitability %in% 3L, sppnotes(Species, Footnotes)]
          ),
          span(
            tags$span(tags$b("Climate Change")), tags$br(),
            tags$span(paste(sc[!is.na(Spp) & pf %in% "1", unique(Spp)], collapse = ", ")), tags$br(),
            tags$span(""), tags$br(),
            tags$span(paste(sc[!is.na(Spp) & pf %in% "2", unique(Spp)], collapse = ", ")), tags$br(),
            tags$span(""), tags$br(),
            tags$span(paste(sc[!is.na(Spp) & pf %in% "3", unique(Spp)], collapse = ", "))
          )
        ),
        tags$br(),
        tags$small(tags$b("Footnotes")),
        tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
        {
          fn <- ss[PreferredAcceptable %in% c("A", "P") | Suitability %in% 1:3, sort(as.integer(unique(unlist(Footnotes))))]
          fnt <- footnotes[match(fn, `Revised Footnote`), `Revised Footnote Text`]
          fnshiny <- mapply(function(footnote, text) {list(tags$sup(footnote), tags$small(text), tags$br())}, fn, fnt, SIMPLIFY = FALSE, USE.NAMES = FALSE)
          do.call(span, fnshiny)
        }
      ),
      span(
        tags$small(tags$b("Stocking (i) - well spaced/ha")),
        tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
        splitLayout(
          cellWidths = c("20%","20%","20%", "40%"),
          span(
            tags$span(tags$b("Target")), tags$br(),
            tags$span(si$StockingTarget)
          ),
          span(
            tags$span(tags$b("Min pa")), tags$br(),
            tags$span(si$StockingMINpa)
          ),
          span(
            tags$span(tags$b("Min p")), tags$br(),
            tags$span(si$StockingMINp)
          ),
          span(
            tags$span(tags$b("Regen Delay (max yrs)")), tags$br(),
            tags$span(si$StockingDelay)
          )
        ),
        tags$br(),
        tags$small(tags$b("Free Growing Guide")),
        tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
        splitLayout(
          cellWidths = c("23%", "23%", "27%", "27%"),
          span(
            tags$span(tags$b("Earliest (yrs)")), tags$br(),
            tags$span(si$AssessmentEarliest)
          ),
          span(
            tags$span(tags$b("Latest(yrs)")), tags$br(),
            tags$span(si$AssessmentLatest)
          ),
          span(
            tags$span(tags$b("Min Height (m)")), tags$br(),
            tags$span(sh[!Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")])
          ),
          span(
            tags$span(tags$b("Min Height (m)")), tags$br(),
            tags$span(sh[Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")])
          )
        )
      )
    ),
    tags$br(),
    tags$hr(style= "padding: 0; margin: 0 0 15px 0; height: 1px; background-color: #dee2e6; border: 0px")
  )
}
