# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

# CCISS Results
output$results_feas <- function() {
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  update_flag()
  cciss_results <- copy(uData$cciss_results)
  feas_filter <- input$filter_feas
  if (is.null(cciss_results)) return(NULL)
  cciss_results_dt(cciss_results, siteref, siteserie, feas_filter)
}

output$edaplot <- renderPlot({
  SS <- input$site_series_feas
  eda <- unique(E1[SS_NoSpace == SS,Edatopic])
  if(length(eda) >= 1){
    edaGrid[,Col := "grey"]
    edaGrid[edatopic %in% eda, Col := "red"]
    ggplot()+
      geom_blank()+
      scale_y_discrete(limits = c("7","6","5","4","3","2","1","0"))+
      scale_x_discrete(limits = c("A","B","C","D","E"))+
      geom_raster(data = edaGrid, aes(x = X,y = Y, fill = Col))+
      geom_hline(aes(yintercept = grd1y), linetype = "dashed")+
      geom_vline(aes(xintercept = grd1x), linetype = "dashed")+
      theme_bw(base_size = 16)+
      theme(panel.grid.major = element_blank())+
      labs(x = "SNR", y = "SMR")+
      theme(plot.background = element_rect(fill = "transparent", colour = NA),
            legend.position = "none")+
      coord_fixed()
  }
},bg="transparent")

grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)

# Dual utility function to format dt, app mode and report mode use different
# format. Report has no javascript, just a plain table.
cciss_results_dt <- function(data, siteref, siteserie, filter, format = "html") {
  data[,Curr := as.character(Curr)]
  for(i in c("Curr","EstabFeas","CFSuitability")){ ##set NA to X
    data[is.na(get(i)) | get(i) == 4, (i) := "X"]
  }
  #print(data[,.(CFSuitability,Curr,ccissFeas)])
  data[CFSuitability == "X" & Curr == "X" 
          & ccissFeas %in% c(1,2,3), EstabFeas := "Trial"]
  if(filter == "a"){
    for(i in c("NewSuit_1991","NewSuit_2021","NewSuit_2041","NewSuit_2061","NewSuit_2081")){ ##set NA to X
      data[is.na(get(i)), (i) := 4]
    }
    data <- data[(IncludeFlag) | Curr %in% c(1,2,3) | CFSuitability %in% c(1,2,3),]
  }else if (filter == "f"){
    data <- data[(ccissFeas %in% c(1,2,3) | EstabFeas %in% c(1,2,3) | Curr %in% c(1,2,3) | CFSuitability %in% c(1,2,3)),]
    
  }
  data[,Trend := paste0("SU", Improve,"EUPUPPY",Decline)] ##have to do this so the br isn't escaped by cell_spec
  data <- data[!is.na(Trend) & !is.na(Improve),]
  #print(data[,.(Trend,Improve)])
  data[,Trend := cell_spec(Trend,"html", color = fifelse(Improve > 67,'green',fifelse(Improve < 33, "red","purple")))]
  data[,Trend := gsub("PUPPY","<br />",Trend)]
  data[,Trend := gsub("SU","<u>",Trend)]
  data[,Trend := gsub("EU","</u>",Trend)]
  
  data[,Period := gsub("<br />","BRK",Period)]
  data[,Period := cell_spec(Period,font_size = 12)]
  data[,Period := gsub("BRK","<br />",Period)]

  data <- data[SiteRef == siteref & SS_NoSpace %in% siteserie,
               .(Species, Period, PredFeasSVG, CFSuitability, PrefAcc_Orig, PrefAcc, 
                 Curr, EstabFeas, ccissFeas, Trend)]
  if(nrow(data) > 0){
    data[Curr == 4,Curr := "X"]
    data[Curr != "X", Curr := paste0("E", Curr)]
    data[!EstabFeas %in% c("X","Trial"), EstabFeas := paste0("E", EstabFeas)]
    data[ccissFeas != "X", ccissFeas := paste0("E", ccissFeas)]
    # for(i in 1:ncol(data)){
    #   data[,(i) := cell_spec(get(names(data)[i]),
    #                          popover = spec_popover(
    #                            content = hoverText[i],
    #                            title = NULL,                           # title will add a Title Panel on top
    #                            position = "right"
    #                          ))]
    # }
    # data[,lapply(.SD, function(x){cell_spec(x,font_size = 24)}),
    #      .SDcols = c("CFSuitability","Curr","EstabFeas","ccissFeas")]
    
    tempTable <- knitr::kable(
      data, format = format, align = c("l","c","c","c","c","c","c","c"), escape = FALSE,
      col.names = c("Tree Species", "Period", "Modelled Feasibility",
                    "Suitability", "P/A", "P/A (cciss)", "Environmental","Establishment",
                    "Future (cciss)","<u>Improve/Same</u><br />Decline/Unsuitable"),
      table.attr = 'class="table table-hover"') %>%
      add_header_above(c(" " = 2, "Raw Votes" = 1, "CFRG" = 2, 
                         "CCISS" = 4, "Trend" = 1)) %>%
      column_spec(4:10,bold = T, extra_css = "vertical-align:middle;") %>%
      column_spec(1, tooltip = hoverText[1]) %>%
      column_spec(2, tooltip = hoverText[2], width = "3cm") %>%
      column_spec(3, tooltip = hoverText[3]) %>%
      column_spec(4, tooltip = hoverText[4]) %>%
      column_spec(5, tooltip = hoverText[5]) %>%
      column_spec(6, tooltip = hoverText[6]) %>%
      column_spec(7, tooltip = hoverText[7]) %>%
      column_spec(8, tooltip = hoverText[8]) %>%
      kable_styling(full_width = F, font_size = 14)
  }else{
    NULL
  }
  
}
uData$cciss_results_dt <- cciss_results_dt
