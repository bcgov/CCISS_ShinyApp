# `siteref_bgc_fut` Sites select update in generate.R at the same time

# Update the label telling which BGC is displayed
output$current_bgc_fut <- renderText({
  siteref <- selected_site$siteref
  if (is.null(uData$bgc)) return(NULL)
  uData$bgc[SiteRef == siteref, unique(BGC)]
})

observeEvent(input$siteref_bgc_fut,{
  siteseries_list <- uData$siteseries_list
  siteseries <- siteseries_list[[selected_site$siteref]]
  updateSelectInput(inputId = "ss_bgc_fut", choices = siteseries, selected = selected_site$ss)
})

# Update the BGC futures plot
output$bgc_fut_plot <- plotly::renderPlotly({
  siteref <- selected_site$siteref
  sseries <- selected_site$ss
  minallow <- input$min_ssoverlap
  update_flag()
  if (is.null(uData$eda_out)) return(NULL)
  bgc_fut_plotly(copy(uData$eda_out), siteref, sseries, minallow)
})

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data, siteref, sseries, minallow, period_map = uData$period_map, ...) {
  data <- data[SSratio > minallow,]
  #browser()
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
                  texttemplate = "%{text}", hovertemplate = "%{y}") %>%
    plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                   xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                ticktext = unname(period_map),
                                tickvals = names(period_map)),
                   barmode = 'stack', legend = l, hovermode = "x unified")
}
uData$bgc_fut_plotly <- bgc_fut_plotly

###map

observe({
  siteref <- input$siteref_bgc_fut_spatial
  timeper <- input$bgc_spatial_period
  update_flag()
  data <- copy(uData$bgc)
  if(!is.null(data) & !is.null(siteref)){
    data <- data[SiteRef == siteref & FuturePeriod == timeper,]
    data <- data[BGC.prop > 0.034,] ##exclude single model predictions
    dat <- data.table(BGC = c(data$BGC.pred,data$BGC[1]),
                      Col = c(colourvalues::colour_values(c(-1,1,data$BGC.prop),palette = "greys",
                                                          include_alpha = F)[-(1:2)],"#FFFB00"))
    session$sendCustomMessage("colour_wna",dat)
  }
})
