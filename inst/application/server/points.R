# Proxy objects
DT_proxy <- DT::dataTableProxy("points_table")

# Input management ----

## Points dataframe
uData <- session$userData

uData$points <- uData$basepoints <- data.table(
  ID = integer(),
  Latitude = numeric(),
  Longitude = numeric(),
  Elevation = numeric(),
  BGC = character(),
  popups = character()
)

## Exclude popups column
output$points_table <- DT::renderDataTable({
  DT::datatable(
    uData$points[j = 1L:5L], rownames = FALSE,
    options = list(searching = FALSE, lengthChange = TRUE, pageLength = 5, scrollX = FALSE, scrollY = "175px", scrollCollapse = FALSE),
    editable = list(target = "row", disable = list(columns = c(0,4)))
  )
})

## DT methods
update_DT <- function() {
  DT::replaceData(DT_proxy, uData$points[j = 1L:5L], clearSelection = "all", rownames = FALSE)
}

## Points update logic
update_ID <- function() {
  uData$points[, ID := seq_len(nrow(uData$points))]
}

new_points <- function(points) {
  beg <- nrow(points)
  withProgress(min = 0, max = 3, detail = "Adding points ...", {
    setProgress(1, message = "Fetch points details")
    points <- points[!is.na(Longitude) & !is.na(Latitude)]
    points[,`:=`(Longitude = round(Longitude, 5), Latitude = round(Latitude, 5))]
    res <- dbGetBecInfo(pool, points[, list(Longitude, Latitude)])
    points[, BGC := res$bgc_label]
    # TODO
    # Fetch elevation from where?
    # points[is.na(Elevevation), Elevation := bcmaps::cded(...)]
    setProgress(2, message = "Create popup info")
    popups <- res[, paste("<b>", names(.SD), ":</b>", .SD, collapse = "<br />"), by=1:NROW(res)]$V1
    points[, popups := sapply(popups, HTML)]
    points <- points[!is.na(BGC) & res$onbcland]
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
    draw_mk(points)
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
  lat_j <- head(grep("^lat|latitude", names(points), ignore.case = TRUE), 1)
  lng_j <- head(grep("^lng|^long|longitude", names(points), ignore.case = TRUE), 1)
  ele_j <- head(grep("^elev|elevation", names(points), ignore.case = TRUE), 1)
  cln_j <- function(j) {
    if (length(j) > 0) {
      as.numeric(points[[j]])
    } else {
      NA_real_
    }
  }
  points <- data.table(Longitude = cln_j(lng_j), Latitude = cln_j(lat_j),
                       Elevation = cln_j(ele_j))
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
    ID = as.numeric(info$value[1]),
    Longitude = as.numeric(info$value[2]),
    Latitude = as.numeric(info$value[3]),
    Elevation = as.numeric(info$value[4])
  )
  points <- new_points(points)
  if (nrow(points)) {
    set(uData$points, info$row[1], points)
    clear_mk()
    draw_mk()
  }
  update_DT()
})
