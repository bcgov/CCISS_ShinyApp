# Update site series selector when site ref is modified
observeEvent(input$siteref_feas, priority = 50, {
  if (is.null(uData$cciss_results)) return(NULL)
  siteref <- input$siteref_feas
  updateSelectInput(inputId = "site_series_feas", choices = uData$siteseries_list[[siteref]])
})

output$results_feas_all <- renderUI({
  if(input$feas_type){
    tableOutput("results_feas")
  }else{
    uiOutput("silviculture_block")
  }
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

standardblock <- function(std, ss, sc) {
  ss <- ss[Standard %in% std]
  ss[, TextStyle := ""]
  sc[,TxtCciss := ""]
  
  # Some logic to flag specie with different Suitability than CCISS
  ss[sc, on = "Species==Spp", ProjFeas := suppressWarnings(as.integer(i.ccissFeas))]
  setnafill(ss, fill = 4L, cols = "ProjFeas")
  ss[Suitability > ProjFeas, TextStyle := "color:green"]
  ss[Suitability < ProjFeas, TextStyle := "color:red"]
  ss[!ProjFeas %in% c(1,2,3), TextStyle := "color:red;text-decoration:line-through"]
  ss[Suitability == 0, TextStyle := NA]
  #browser()
  # cciss colouring
  sc[ss, on = "Spp == Species", CFRGSuit := i.Suitability]
  sc[ccissFeas < CFRGSuit, TxtCciss := "color:green"]
  sc[ccissFeas > CFRGSuit, TxtCciss := "color:red"]
  sc[!CFRGSuit %in% c(1,2,3), TxtCciss := "color:purple"]
  sc[CFRGSuit == 0, TxtCciss := NA]
  
  si <- stocking_info[Standard == std]
  sh <- stocking_height[Standard == std]
  list(
    tags$small("Forest Region: ", tags$b(si$Region, .noWS = c("before", "after")), .noWS = "inside"),
    tags$small("\nStandards ID: ",tags$b(paste(ss[!is.na(Standard), unique(Standard)], collapse = ", "),.noWS = c("before", "after"))),
    tags$table(style = "max-width: 100%; white-space: nowrap;",
               # Report formatting gray out the first row, so faking a row
               tags$tr(
                 tags$td(width = "50%", style = "vertical-align: top; padding:0; background-color:white; border:none",
                         tags$small(tags$b("Regeneration")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
                         tags$table(
                           width = "100%",
                           tags$tr(
                             tags$td(tags$b("Feasibility")),
                             tags$td(tags$b("CFRG"),style = "border-right: 2px solid;"),
                             tags$td(tags$b("CCISS"))
                           ),
                           tags$tr(
                             tags$td("Primary/E1"),
                             ss[!is.na(Species) & Suitability %in% 1L, sppnotes(Species, Footnotes, TextStyle)],
                             sc[!is.na(Spp) & ccissFeas %in% "1", sppnotes_cciss(Spp,TxtCciss)]
                           ),
                           
                           tags$tr(
                             tags$td("Secondary/E2"),
                             ss[!is.na(Species) & Suitability %in% 2L, sppnotes(Species, Footnotes, TextStyle)],
                             sc[!is.na(Spp) & ccissFeas %in% "2", sppnotes_cciss(Spp,TxtCciss)]
                           ),
                           
                           tags$tr(
                             tags$td("Tertiary/E3"),
                             ss[!is.na(Species) & Suitability %in% 3L, sppnotes(Species, Footnotes, TextStyle)],
                             sc[!is.na(Spp) & ccissFeas %in% "3", sppnotes_cciss(Spp,TxtCciss)]
                           ),
                           
                           tags$tr(
                             tags$td("Trial"),
                             tags$td(""),
                             sc[!is.na(Spp) & EstabFeas == "Trial", sppnotes_cciss(Spp,TxtCciss)]
                           ),
                           tags$tr(
                             tags$td("Broadleaf"),
                             ss[!is.na(Species) & Suitability %in% 0L, sppnotes(Species, Footnotes, TextStyle)],
                             tags$td("",style = "border-left: 2px solid;"),
                             style = "border-bottom:1px solid black;"
                           ),
                           
                           tags$tr(
                             tags$td("Preferred (p)"),
                             ss[!is.na(Species) & PreferredAcceptable %in% "P", sppnotes(Species, Footnotes, TextStyle)],
                             sc[!is.na(Spp) & PrefAcc %in% "P", sppnotes_cciss(Spp,TxtCciss)],
                           ),
                           tags$tr(
                             tags$td("Acceptable (a)"),
                             ss[!is.na(Species) & PreferredAcceptable %in% "A", sppnotes(Species, Footnotes, TextStyle)],
                             sc[!is.na(Spp) & PrefAcc %in% "A", sppnotes_cciss(Spp,TxtCciss)],
                           )
                         )
                 ),
                 tags$td(width = "50%", style = "vertical-align: top; padding:0px 0px 0px 8px; background-color:white; border:none",
                         tags$small(tags$b("Stocking (i) - well spaced/ha")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
                         tags$table(
                           width = "100%",
                           tags$tr(
                             tags$td(tags$b("Target")),
                             tags$td(tags$b("Min pa")),
                             tags$td(tags$b("Min p")),
                             tags$td(tags$b("Regen Delay (max yrs)"))
                           ),
                           tags$tr(
                             tags$td(si$StockingTarget),
                             tags$td(si$StockingMINpa),
                             tags$td(si$StockingMINp),
                             tags$td(si$StockingDelay)
                           )
                         ),
                         tags$br(),
                         tags$small(tags$b("Free Growing Guide")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
                         tags$table(
                           width = "100%",
                           tags$tr(
                             tags$td(tags$b("Earliest (yrs)")),
                             tags$td(tags$b("Latest(yrs)")),
                             tags$td(tags$b("Min Height (m)")),
                             tags$td(tags$b("Min Height (m)"))
                           ),
                           tags$tr(
                             tags$td(si$AssessmentEarliest),
                             tags$td(si$AssessmentLatest),
                             tags$td(style = "white-space: normal;", sh[!Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")]),
                             tags$td(style = "white-space: normal;", sh[Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")])
                           )
                         )
                 )
               ),
               tags$tr(
                 tags$td(colspan = "2", style = "white-space:normal; vertical-align: top; padding:0; background-color:white; border:none",
                         tags$small(tags$b("Footnotes")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
                         {
                           fn <- ss[PreferredAcceptable %in% c("A", "P") | Suitability %in% 1:3, sort(as.integer(unique(unlist(Footnotes))))]
                           fnt <- footnotes[match(fn, `Revised Footnote`), `Revised Footnote Text`]
                           fnshiny <- mapply(function(footnote, text) {list(tags$sup(footnote), tags$small(text), tags$br())}, fn, fnt, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                           do.call(span, fnshiny)
                         }
                 )
               )
    )
  )
}


uData$standardblock <- standardblock
