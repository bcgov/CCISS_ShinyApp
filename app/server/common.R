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
  popups = character()
)

# All columns indexes
uData$pts_col <- 1L:ncol(uData$basepoints)
# Exclude popups column, column indexes to show in the UI
uData$pts_show_col <- 1L:(ncol(uData$basepoints) - 1L)

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

cciss_summary <- function(cciss) {
  withProgress(message = "Processing...", detail = "Feasibility summary", {
    summary <- cciss$Summary
    # TODO
    # Validate how they want the table to look like
    summary[, Spp := T1[Spp, paste(TreeCode, EnglishName, sep = ": ")]]
    setnames(
      summary,
      names(summary),
      # Using &nbsp; as using DT::datatable options to set column width breaks display
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

cciss_detailed <- function(cciss) {
  withProgress(message = "Processing...", detail = "Feasibility detailed", {
    detailed <- cciss$Raw
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
        cfr <- Curr[FuturePeriod == current]
        cfr <- as.character(cfr)
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
    ), by=c("SiteRef", "SS_NoSpace", "Spp")]
    setorder(detailed, SiteRef, SS_NoSpace, OrderSuit, MeanSuit, `Tree Species`)
    detailed[, c("Spp", "SS_NoSpace", "OrderSuit") := NULL]
    return(detailed)
  })
}

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data) {
  l <- list(
    font = list(
      family = "BCSans",
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
  plotly::plot_ly(data = data[FuturePeriod > "1975"], x = ~FuturePeriod,
                  y = ~BGC.prop, split = ~BGC.pred, type = 'bar',
                  color = ~BGC.pred, colors = color_ref,
                  text = ~BGC.pred, textposition = 'inside', textfont = list(color = "black", size = 20),
                  texttemplate = "%{text}", hovertemplate = "%{y}") %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   xaxis = list(showspikes = FALSE, title = list(text = "Period")),
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
