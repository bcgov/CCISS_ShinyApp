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
  data <- copy(uData$eda_out)
  #browser()
  data[,Lab := paste(SS.pred,round(SSratio,digits = 2),sep = ": ")]
  data <- data[,.(SSLab = paste(Lab,collapse = "<br>")),
               by = .(SiteRef,SS_NoSpace,FuturePeriod,BGC.pred)]
  dat_bgc <- copy(uData$bgc)
  dat_bgc[,BGC := NULL]
  plotdat <- merge.data.table(data,dat_bgc, on = c("SiteRef","FuturePeriod","BGC.pred"), all= TRUE)
  bgc_fut_plotly(plotdat, siteref, sseries, minallow)
})

# Graph

#' @param data BGC data.table
bgc_fut_plotly <- function(data, siteref, sseries, minallow, period_map = uData$period_map, ...) {
  #data <- data[SSratio > minallow,]
  
  dat_order <- data.table(FuturePeriod = c("1961","1991","2001","2021","2041","2061", "2081"), fpCode = c(1,2,3.5,4.5,5.5,6.5,7.5))
  #browser()
  #data[,allOverlap := allOverlap/sum(allOverlap), by = .(SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,BGC.prop)]
  
  #data <- data[SiteRef == siteref & SS_NoSpace == sseries,]
  data2 <- unique(data[SiteRef == siteref,.(FuturePeriod,BGC.pred,BGC.prop)])
  data2[dat_order, fpCode := i.fpCode, on = "FuturePeriod"]
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
    colors <- subzones_colours_ref[unique(data2$BGC.pred)]
    col <- colors$colour
    names(col) <- colors$classification
    col
  }
  if(input$future_showss == "BGC"){
    plotly::plot_ly(data = data2, x = ~fpCode,
                    y = ~BGC.prop, split = ~BGC.pred, type = 'bar',
                    color = ~BGC.pred, colors = color_ref, hovertemplate = "%{y}",
                    text = ~BGC.pred, textposition = 'inside', textfont = list(color = "black", size = 12),
                    texttemplate = "%{text}") %>%
      plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                     xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                  ticktext = unname(period_map),
                                  tickvals = dat_order$fpCode),
                     barmode = 'stack', legend = l, hovermode = "x unified")
  }else{
    data <- data[SiteRef == siteref & SS_NoSpace == sseries,]
    data_ss <- merge(data2, data, on = c("SiteRef","FuturePeriod","BGC.pred"), all = T)
    data_ss[dat_order, fpCode := i.fpCode, on = "FuturePeriod"]
    plotly::plot_ly(data = data_ss, x = ~fpCode,
                    y = ~BGC.prop, split = ~BGC.pred, type = 'bar',
                    color = ~BGC.pred, colors = color_ref,
                    text = ~SSLab, textposition = 'inside', textfont = list(color = "black", size = 12),
                    texttemplate = "%{text}", hovertemplate = "%{y}") %>%
      plotly::layout(yaxis = list(title = "", tickformat = ".1%"),
                     xaxis = list(showspikes = FALSE, title = list(text = "Period"),
                                  ticktext = unname(period_map),
                                  tickvals = dat_order$fpCode),
                     barmode = 'stack', legend = l, hovermode = "x unified")
  }
  
}
uData$bgc_fut_plotly <- bgc_fut_plotly

###map

observe({
  if(input$cciss_navbar == "wna_map"){
    #print(input$cciss_navbar)
    siteref <- input$siteref_bgc_fut_spatial
    timeper <- input$bgc_spatial_period
    #update_flag()
    data <- copy(uData$bgc)
    if(!is.null(data) & !is.null(siteref)){
      
      data <- data[SiteRef == siteref & FuturePeriod == timeper,]
      data <- data[BGC.prop > 0.034,] ##exclude single model predictions
      # dat <- data.table(BGC = c(data$BGC.pred,data$BGC[1]),
      #                   Col = c(colourvalues::colour_values(c(-1,1,data$BGC.prop),palette = "greys",
      #                                                       include_alpha = F)[-(1:2)],"#FFFB00"))
      grey_limit = 
      dat <- data.table(BGC = c(data$BGC.pred,data$BGC[1]),
                        Col = c(colourvalues::colour_values(c(-1,1,data$BGC.prop),palette = "greys",
                                                            include_alpha = F)[-(1:2)],"#fb00ff"))#dd1c77
       #print(dat)
      session$sendCustomMessage("colour_wna",dat)
    }
  }
    
})
