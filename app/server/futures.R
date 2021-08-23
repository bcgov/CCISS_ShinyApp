# `siteref_bgc_fut` Sites select update in generate.R at the same time

# Update the label telling which BGC is displayed
output$current_bgc_fut <- renderText({
  siteref <- input$siteref_bgc_fut
  if (is.null(uData$bgc)) return(NULL)
  uData$bgc[SiteRef == siteref, unique(BGC)]
})

observeEvent(input$siteref_bgc_fut,{
  siteseries_list <- uData$siteseries_list
  siteseries <- siteseries_list[[input$siteref_bgc_fut]]
  updateSelectInput(inputId = "ss_bgc_fut", choices = siteseries, selected = siteseries[1])
})

# Update the BGC futures plot
output$bgc_fut_plot <- plotly::renderPlotly({
  siteref <- input$siteref_bgc_fut
  sseries <- input$ss_bgc_fut
  minallow <- input$min_ssoverlap
  update_flag()
  if (is.null(uData$eda_out)) return(NULL)
  bgc_fut_plotly(copy(uData$eda_out), siteref, sseries, minallow)
})

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data, siteref, sseries, minallow, period_map = uData$period_map, ...) {
  data <- data[SSratio > minallow,]
  #data[,allOverlap := allOverlap/sum(allOverlap), by = .(SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,BGC.prop)]
  data[,Lab := paste(SS.pred,round(SSratio,digits = 2),sep = ": ")]
  data <- data[,.(SSLab = paste(Lab,collapse = "<br>")), 
                    by = .(SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,BGC.prop)]
  data <- data[SiteRef == siteref & SS_NoSpace == sseries,]
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