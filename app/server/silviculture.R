observeEvent(input$siteref_silv, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- input$siteref_silv
  updateSelectInput(inputId = "site_series_silv", choices = uData$siteseries_list[[siteref]])
})

output$silviculture_block <- renderUI({
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  cciss_results <- uData$cciss_results
  if (is.null(cciss_results)) return(NULL)
  standardblocks(cciss_results, siteref, siteserie)
})

output$silvics_tol_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_tol, cciss_results, siteref, siteserie, silv_filter)
})

output$silvics_resist_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_resist, cciss_results, siteref, siteserie, silv_filter)
})

output$silvics_regen_dt <- DT::renderDT({
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  cciss_results <- uData$cciss_results
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_regen, cciss_results, siteref, siteserie, silv_filter)
})

output$silvics_mature_dt <- DT::renderDT({
  cciss_results <- uData$cciss_results
  siteref <- input$siteref_silv
  siteserie <- input$site_series_silv
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_mature, cciss_results, siteref, siteserie, silv_filter)
})

# Ref template for other tables
silv_ref_dt <- function(silv, data, siteref, siteserie, filter, app = TRUE) {
  if (filter == "f") {
    data <- data[SiteRef %in% siteref & `Site Series` %in% siteserie]
    silv <- silv[`Tree Code` %in% data[`Projected Feasibility` %chin% c("1", "2", "3"), Spp]]
  }
  if (isTRUE(app)) {
    DT::datatable(silv, escape = FALSE, rownames = FALSE, options = list(info = FALSE,
      scrollCollapse = FALSE, lengthChange = FALSE, ordering = FALSE, autoWidth = TRUE, 
      searching = FALSE, pageLength = nrow(silv), paging = FALSE, columnDefs = list(
        list(className = 'dt-center', targets = (seq_len(ncol(silv))-1)[-3])
      )
    ))
  } else {
    tableHTML::tableHTML(silv, rownames = FALSE, escape = FALSE, border = 0) %>%
      tableHTML::add_css_column(css = list("text-align", "center"), columns = c(1:2, 4:ncol(silv)))
  }
}
uData$silv_ref_dt <- silv_ref_dt

# Silviculture UI Element

# Function to format Species with footnotes
sppnotes <- function(spp, notes) {
  ret <- vector("list", length(spp))
  for (i in seq_len(length(spp))) {
    fn <- paste0(sort(as.integer(unique(unlist(notes[i])))), collapse = ",")
    ret[[i]] <- tags$span(spp[i], tags$sup(fn, .noWS = htmltools:::noWSOptions), if (i < length(spp)) {", "} else {""}, .noWS = htmltools:::noWSOptions)
  }
  do.call(tags$td, ret)
}
uData$sppnotes <- sppnotes

# Function to create a Standards block for each Standard in the site serie
standardblocks <- function(data, siteref, siteserie) {
  sc <- data[
    SiteRef %in% siteref & SS_NoSpace %in% siteserie,
    c("Region", "ZoneSubzone", "SS_NoSpace", "Projected Feasibility", "Spp")
  ]
  sc[, pf := `Projected Feasibility`]
  ss <- stocking_standards[
    Region %in% sc$Region & ZoneSubzone %in% sc$ZoneSubzone & SS_NoSpace %in% sc$SS_NoSpace
  ]
  do.call(span, lapply(unique(ss$Standard), standardblock, ss = ss, sc = sc))
}
uData$standardblocks <- standardblocks

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
    div(class = "shiny-split-layout", style = "white-space: nowrap;",
        div(style = "width:50%; overflow:visible; display: inline-block; vertical-align: top; box-sizing: border-box",
            tags$small(tags$b("Regeneration")),
            tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
            tags$table(
              width = "100%",
              tags$tr(
                tags$td(tags$b("Standards ID")),
                tags$td(tags$b(paste(ss[!is.na(Standard), unique(Standard)], collapse = ", "))),
                tags$td(tags$b("Climate Change"))
              ),
              tags$tr(
                tags$td("Primary"),
                ss[!is.na(Species) & Suitability %in% 1L, sppnotes(Species, Footnotes)],
                tags$td(paste(sc[!is.na(Spp) & pf %in% "1", unique(Spp)], collapse = ", "))
              ),
              tags$tr(
                tags$td("Preferred (p)"),
                ss[!is.na(Species) & PreferredAcceptable %in% "P", sppnotes(Species, Footnotes)],
                tags$td("")
              ),
              tags$tr(
                tags$td("Secondary"),
                ss[!is.na(Species) & Suitability %in% 2L, sppnotes(Species, Footnotes)],
                tags$td(paste(sc[!is.na(Spp) & pf %in% "2", unique(Spp)], collapse = ", "))
              ),
              tags$tr(
                tags$td("Acceptable (a)"),
                ss[!is.na(Species) & PreferredAcceptable %in% "A", sppnotes(Species, Footnotes)],
                tags$td("")
              ),
              tags$tr(
                tags$td("Tertiary"),
                ss[!is.na(Species) & Suitability %in% 3L, sppnotes(Species, Footnotes)],
                tags$td(paste(sc[!is.na(Spp) & pf %in% "3", unique(Spp)], collapse = ", "))
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
        div(style = "width:50%; overflow:visible; display: inline-block; vertical-align: top; box-sizing: border-box",
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
    tags$br(),
    tags$hr(style= "padding: 0; margin: 0 0 15px 0; height: 1px; background-color: #dee2e6; border: 0px")
  )
}
uData$standardblock <- standardblock
