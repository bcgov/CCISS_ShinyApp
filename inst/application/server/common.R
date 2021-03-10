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
    y = 1.1)
  color_ref <- {
    colors <- subzones_colours_ref[unique(data$BGC.pred)]
    col <- colors$colour
    names(col) <- colors$classification
    col
  }
  plotly::plot_ly(data = data[FuturePeriod > "1975"], x = ~FuturePeriod,
                  y = ~BGC.prop, split = ~BGC.pred, type = 'bar',
                  color = ~BGC.pred, colors = color_ref) %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   barmode = 'stack', legend = l, hovermode = 'x unified')
}
