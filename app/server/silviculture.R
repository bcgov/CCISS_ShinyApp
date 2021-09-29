observeEvent(input$siteref_silv, priority = 50, {
  if (is.null(uData$cciss_results) | is.null(uData$cciss_summary)) return(NULL)
  siteref <- selected_site$siteref
  updateSelectInput(inputId = "site_series_silv", choices = uData$siteseries_list[[siteref]])
})

output$silviculture_block <- renderUI({
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  cciss_results <- uData$cciss_results
  if (is.null(cciss_results)) return(NULL)
  standardblocks(cciss_results, siteref, siteserie)
})

output$silvics_tol_dt <- function() {
  cciss_results <- uData$cciss_results
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_tol, cciss_results, siteref, siteserie, silv_filter,
              title = "Tolerance comparisons")
}

output$silvics_resist_dt <- function() {
  cciss_results <- uData$cciss_results
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_resist, cciss_results, siteref, siteserie, silv_filter,
              title = "Resistance and potential risk comparisons")
}

output$silvics_regen_dt <- function() {
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  cciss_results <- uData$cciss_results
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_regen, cciss_results, siteref, siteserie, silv_filter,
              title = "Comparison of silvical characteristics; regeneration stage")
}

output$silvics_mature_dt <- function() {
  cciss_results <- uData$cciss_results
  siteref <- selected_site$siteref
  siteserie <- selected_site$ss
  silv_filter <- input$filter_silv
  if (is.null(cciss_results)) return(NULL)
  silv_ref_dt(silvics_mature, cciss_results, siteref, siteserie, silv_filter,
              title = "Comparison of silvical characteristics; maturing stage")
}

# Ref template for other tables
silv_ref_dt <- function(silv, data, siteref = NULL, siteserie = NULL, filter, format = "html", title = "") {
  if (filter == "f") {
    if(!is.null(siteref)){
      data <- data[SiteRef == siteref & SS_NoSpace == siteserie,]
    }
    silv <- silv[`Tree Code` %in% data[Curr %in% c(1,2,3) | CFSuitability %in% c(1,2,3) | ccissFeas %in% c(1,2,3),Spp]]
  }
  colalign <- rep("c", length.out = ncol(silv))
  colalign[3] <- "l"
  knitr::kable(silv, format = format, escape = FALSE, align = colalign, table.attr = 'class="table table-hover"',
               caption = paste(title ,"<br />", "From Klinka et al. 2000. Ecological and Silvical Characteristics of Tree Species")
  )
}
uData$silv_ref_dt <- silv_ref_dt

# Silviculture UI Element

# Function to format Species with footnotes
sppnotes <- function(spp, notes, textstyle) {
  ret <- vector("list", length(spp))
  for (i in seq_len(length(spp))) {
    fn <- paste0(sort(as.integer(unique(unlist(notes[i])))), collapse = ",")
    ret[[i]] <- tags$span(tags$span(style = textstyle[i], spp[i]),
                          tags$sup(fn, .noWS = htmltools:::noWSOptions),
                          if (i < length(spp)) {", "} else {""}, .noWS = htmltools:::noWSOptions)
  }
  ret[["style"]] <- "white-space:normal;"
  do.call(tags$td, ret)
}
uData$sppnotes <- sppnotes

# Function to create a Standards block for each Standard in the site serie
standardblocks <- function(data, siteref, siteserie) {
  sc <- data[
    SiteRef %in% siteref & SS_NoSpace %in% siteserie,
    list(Region, ZoneSubzone, SS_NoSpace, ProjFeas, Spp)
  ]
  ss <- stocking_standards[
    Region %in% sc$Region & ZoneSubzone %in% sc$ZoneSubzone & SS_NoSpace %in% sc$SS_NoSpace
  ]
  do.call(span, lapply(unique(ss$Standard), standardblock, ss = ss, sc = sc))
}
uData$standardblocks <- standardblocks

# Function to create a formatted Standard block
standardblock <- function(std, ss, sc) {
  ss <- ss[Standard %in% std]
  ss[, TextStyle := ""]
  
  # Some logic to flag specie with different Suitability than CCISS
  ss[sc, on = "Species==Spp", ProjFeas := suppressWarnings(as.integer(i.ProjFeas))]
  setnafill(ss, fill = 4L, cols = "ProjFeas")
  ss[Suitability > ProjFeas, TextStyle := "color:blue"]
  ss[Suitability < ProjFeas, TextStyle := "color:orange"]
  ss[ProjFeas == 4L, TextStyle := "color:red;text-decoration:line-through"]
  
  si <- stocking_info[Standard == std]
  sh <- stocking_height[Standard == std]
  list(
    tags$small("Forest Region: ", tags$b(si$Region, .noWS = c("before", "after")), .noWS = "inside"),
    tags$table(width = "100%", style = "white-space: nowrap;",
      # Report formatting gray out the first row, so faking a row
      tags$tr(
        tags$td(width = "50%", style = "vertical-align: top; padding:0; background-color:white; border:none",
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
              ss[!is.na(Species) & Suitability %in% 1L, sppnotes(Species, Footnotes, TextStyle)],
              tags$td(paste(sc[!is.na(Spp) & ProjFeas %in% "1", unique(Spp)], collapse = ", "))
            ),
            tags$tr(
              tags$td("Preferred (p)"),
              ss[!is.na(Species) & PreferredAcceptable %in% "P", sppnotes(Species, Footnotes, TextStyle)],
              tags$td("")
            ),
            tags$tr(
              tags$td("Secondary"),
              ss[!is.na(Species) & Suitability %in% 2L, sppnotes(Species, Footnotes, TextStyle)],
              tags$td(paste(sc[!is.na(Spp) & ProjFeas %in% "2", unique(Spp)], collapse = ", "))
            ),
            tags$tr(
              tags$td("Acceptable (a)"),
              ss[!is.na(Species) & PreferredAcceptable %in% "A", sppnotes(Species, Footnotes, TextStyle)],
              tags$td("")
            ),
            tags$tr(
              tags$td("Tertiary"),
              ss[!is.na(Species) & Suitability %in% 3L, sppnotes(Species, Footnotes, TextStyle)],
              tags$td(paste(sc[!is.na(Spp) & ProjFeas %in% "3", unique(Spp)], collapse = ", "))
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
