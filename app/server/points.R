# Proxy objects
DT_proxy <- DT::dataTableProxy("points_table")

# Input management ----

## Points dataframe
uData <- session$userData

uData$points <- uData$basepoints <- data.table(
  ID = character(),
  Site = character(),
  Lat = numeric(),
  Long = numeric(),
  Elev = numeric(),
  BGC = character(),
  popups = character()
)

uData$pts_col <- 1L:ncol(uData$basepoints)
## Exclude popups column
uData$pts_show_col <- 1L:(ncol(uData$basepoints) - 1L)

output$points_table <- DT::renderDataTable({
  DT::datatable(
    uData$points[,uData$pts_show_col, with = FALSE], rownames = FALSE,
    options = list(searching = FALSE, lengthChange = TRUE, pageLength = 5, scrollX = FALSE, scrollY = "175px", scrollCollapse = FALSE),
    editable = list(target = "row", disable = list(columns = c(1,5)))
  )
})

## DT methods
update_DT <- function() {
  DT::replaceData(DT_proxy, uData$points[,uData$pts_show_col, with = FALSE], clearSelection = "all", rownames = FALSE)
}

## Points update logic
update_ID <- function() {
  uData$points[is.na(ID), ID := as.character(seq_len(nrow(uData$points))[is.na(uData$points$ID)])]
}

new_points <- function(points) {
  beg <- nrow(points)
  withProgress(min = 0, max = 3, detail = "Adding points ...", {
    setProgress(1, message = "Fetch points details")
    points <- points[!is.na(Long) & !is.na(Lat)]
    points[,`:=`(Long = round(Long, 5), Lat = round(Lat, 5))]
    res <- dbGetBecInfo(pool, points[, list(Long, Lat)])
    points[, BGC := res$bgc_label]
    points[, Site := dbGetHexID(pool, points[, list(Long, Lat)])]
    # TODO
    # Fetch elevation from where?
    # points[is.na(Elevevation), Elev := bcmaps::cded(...)]
    setProgress(2, message = "Create popup info")
    onbcland <- res$onbcland
    popups <- res[, onbcland := NULL][, 
      paste("<b>", tools::toTitleCase(gsub("_", " ", names(.SD))), ":</b>", .SD, collapse = "<br />"),
      by=1:NROW(res)]$V1
    points[, popups := sapply(popups, HTML)]
    points <- points[!is.na(BGC) & onbcland]
    points <- rbindlist(list(uData$basepoints, points), fill = TRUE)
    setProgress(3, message = "Done")
  })
  end <- nrow(points)
  if (beg != end) {
    showModal(
      modalDialog(
        title = "Warning",
        paste("Points outside of coverage zones were dropped or not updated." ),
        easyClose = TRUE
      )
    )
  } else {
    removeModal()
  }
  return(points)
}

insert_points <- function(points) {
  if (nrow(points)) {
    uData$points <- rbindlist(list(uData$points, points), fill = TRUE)
    update_ID()
    update_DT()
    draw_mk(tail(uData$points, nrow(points)))
  }
}

## Upload points logic
observeEvent(input$upload_button,{
  showModal(
    modalDialog(
      fileInput(inputId = "points_upload", label =  "Upload points in a delimited files",
                accept = c(".csv", ".txt"), buttonLabel = "Browse...", placeholder = "My points")
    )
  )
})

observeEvent(input$points_upload,{
  insert_points_file(input$points_upload$datapath)
})

insert_points_file <- function(datapath){
  # Columns detection using regular expression, hopefully column names have a match
  points <- fread(datapath)
  nm <- names(points)
  id_j <- head(grep("^id", nm, ignore.case = TRUE), 1)
  lat_j <- head(grep("^lat|latitude", nm, ignore.case = TRUE), 1)
  lng_j <- head(grep("^lng|^long|longitude", nm, ignore.case = TRUE), 1)
  ele_j <- head(grep("^elev|elevation", nm, ignore.case = TRUE), 1)
  cln_j <- function(j) {if (length(j) > 0) {as.numeric(points[[j]])} else {NA_real_}}
  clc_j <- function(j) {if (length(j) > 0) {as.character(points[[j]])} else {NA_character_}}
  points <- data.table(ID = clc_j(id_j), Long = cln_j(lng_j), Lat = cln_j(lat_j),
                       Elev = cln_j(ele_j))
  points <- new_points(points)
  insert_points(points)
  set_map_bound()
}

## Add a point logic
observeEvent(input$add_button,{
  uData$points <- rbindlist(list(uData$points, data.table(ID=NA_integer_)), fill = TRUE)
  update_ID()
  update_DT()
})

## Delete selected points logic
observeEvent(input$delete_button,{
  if(length(input$points_table_rows_selected)>=1) {
    uData$points <- uData$points[-input$points_table_rows_selected]
    update_DT()
    clear_mk()
    draw_mk()
  } else {
    showModal(
      modalDialog(
        title = "Warning",
        paste("Please select row(s)." ),
        easyClose = TRUE
      )
    )
  }
})

## Clear all points logic
observeEvent(input$clear_button,{
  uData$points <- uData$basepoints
  update_DT()
  clear_mk()
})

## Edit points logic
observeEvent(input$points_table_cell_edit, {
  info = input$points_table_cell_edit
  points <- data.table(
    ID = as.character(info$value[1]),
    Site = as.character(1),
    Lat = as.numeric(info$value[3]),
    Long = as.numeric(info$value[4]),
    Elev = as.numeric(info$value[5]),
    BGC = as.character(1)
  )
  points <- new_points(points)
  if (nrow(points)) {
    set(uData$points, i = info$row[1], j = uData$pts_col, value = points)
    clear_mk()
    draw_mk()
  }
  update_DT()
})
