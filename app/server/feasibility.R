# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 500, {
  if (is.null(uData$cciss_results)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

output$results_feas_all <- renderUI({
  input$ohr_feas
  if(input$feas_type){
    tableOutput("results_feas")
  }else{
    #browser()
    siteref <- selected_site$siteref
    siteserie <- selected_site$ss
    cciss_results <- uData$cciss_results
    if (is.null(cciss_results)) return(NULL)
    temp <- standardblocks(cciss_results, siteref, siteserie)
    temp
  }
})

# CCISS Results
output$results_feas <- function() {
  #browser()
  siteref <- input$siteref_feas
  siteserie <- input$site_series_feas
  update_flag()
  if(input$ohr_feas == session_params$show_ohr){
    cciss_results <- copy(uData$cciss_results)
  } else {
    if(!input$ohr_feas & session_params$show_ohr){
      suit <- copy(S1)
      suit <- suit[(!OHR),]
      session_params$show_ohr <- FALSE
    } else if(input$ohr_feas & !session_params$show_ohr) {
      suit <- copy(S1)
      session_params$show_ohr <- TRUE
    } 
    cciss_res <- ccissOutput(SSPred = uData$SSPred, suit = suit, rules = R1, feasFlag = F1, 
                            histWeights = session_params$estabWt, futureWeights = session_params$futWt)
    cciss_results   <- uData$cciss_results   <- cciss_results(cciss_res, uData$bgc, uData$pts, uData$avg, type = as.logical(input$aggregation))
    
  }
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
      ggtitle("Edatopic Position") +
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
  #browser()
  if(filter == "a"){
    for(i in c("NewSuit_1991","NewSuit_2001","NewSuit_2021","NewSuit_2041","NewSuit_2061","NewSuit_2081")){ ##set NA to X
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
               .(Species, Period, PredFeasSVG, CFSuitability, PrefAcc_Orig, 
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
      data, format = format, align = c("l","c","c","c","c","c","c"), escape = FALSE,
      col.names = c("Tree Species", "Period", "Modelled Feasibility",
                    "CFRG Suitability", "CFRG P/A", "Environmental","Establishment",
                    "Maturity","<u>Improve/Same</u><br />Decline/Unsuitable"),
      table.attr = 'class="table table-hover"') %>%
      add_header_above(c(" " = 3, "Historic" = 3, 
                         "CCISS" = 2, "Trend" = 1)) %>%
      column_spec(4:9,bold = T, extra_css = "vertical-align:middle; font-size:22px;") %>%
      column_spec(3, tooltip = tooltip_text$model_agree) %>%
      column_spec(4:5, tooltip = tooltip_text$cfrg, width = "3cm") %>%
      column_spec(6, tooltip = tooltip_text$es,border_left = T,border_right = T) %>%
      column_spec(7, tooltip = tooltip_text$cciss_estab) %>%
      column_spec(8, tooltip = tooltip_text$cciss_mature) %>%
      column_spec(9, tooltip = tooltip_text$trends) %>%
      kable_styling(full_width = F, font_size = 14)
  }else{
    NULL
  }
  
}
uData$cciss_results_dt <- cciss_results_dt

###simple output
output$silviculture_block <- renderUI({
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  cciss_results <- uData$cciss_results
  if (is.null(cciss_results)) return(NULL)
  standardblocks(cciss_results, siteref, siteserie)
})

standardblocks <- function(data, siteref, siteserie) {
  sc <- data[
    SiteRef %in% siteref & SS_NoSpace %in% siteserie,
    list(Region, ZoneSubzone, SS_NoSpace, ccissFeas,EstabFeas, Improve,PrefAcc, Spp)
  ]
  ss <- stocking_standards[
    Region %in% sc$Region & ZoneSubzone %in% sc$ZoneSubzone & SS_NoSpace %in% sc$SS_NoSpace
  ]
  do.call(span, lapply(unique(ss$Standard), standardblock, ss = ss, sc = sc))
}

uData$standardblocks <- standardblocks

