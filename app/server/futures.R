# `siteref_bgc_fut` Sites select update in generate.R at the same time

# Update the label telling which BGC is displayed
output$current_bgc_fut <- renderText({
  siteref <- input$siteref_bgc_fut
  if (is.null(uData$bgc)) return(NULL)
  uData$bgc[SiteRef == siteref, unique(BGC)]
})

# Update the BGC futures plot
output$bgc_fut_plot <- plotly::renderPlotly({
  siteref <- input$siteref_bgc_fut
  sseries <- input$ss_bgc_fut
  if (is.null(uData$sspreds)) return(NULL)
  bgc_fut_plotly(uData$sspreds, siteref, sseries)
})

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data, siteref, sseries, period_map = uData$period_map, ...) {
  data <- data[SiteRef == siteref & SS_NoSpace == sseries]
  l <- list(
    font = list(
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    orientation = 'h',
    y = 1.25,
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
                  text = ~SSLab, textposition = 'inside', textfont = list(color = "black", size = 12),
                  texttemplate = "%{text}", hovertemplate = "%{y}", ...) %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                ticktext = unname(period_map),
                                tickvals = names(period_map)),
                   barmode = 'stack', legend = l, hovermode = "x unified")
}
uData$bgc_fut_plotly <- bgc_fut_plotly