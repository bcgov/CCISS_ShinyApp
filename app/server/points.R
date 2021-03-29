# Points dataframe
uData$basepoints <- data.table(
  ID = character(),
  Site = character(),
  Lat = numeric(),
  Long = numeric(),
  Elev = numeric(),
  BGC = character(),
  ForestRegion = character(),
  popups = character()
)

# All columns indexes
uData$pts_col <- 1L:ncol(uData$basepoints)
# Exclude popups column, column indexes to show in the UI
uData$pts_show_col <- 1L:(ncol(uData$basepoints) - 2L)

userpoints <- reactiveValues(dt = uData$basepoints)

# Input management ----
output$points_table <- DT::renderDataTable({
  pts <- userpoints$dt
  DT::datatable(
    pts[, uData$pts_show_col, with = FALSE], rownames = FALSE,
    options = list(searching = FALSE, lengthChange = TRUE, pageLength = 5,
                   scrollX = FALSE, scrollY = "185px", scrollCollapse = FALSE),
    editable = list(target = "row", disable = list(columns = c(1,5)))
  )
})

## Points update logic
update_ID <- function() {
  pts <- userpoints$dt
  pts[is.na(ID), ID := as.character(seq_len(nrow(pts))[is.na(pts$ID)])]
  userpoints$dt <- pts
}

# All new point go through this processing step to fetch additionnal information
# and create popups
new_points <- function(points) {
  withProgress(message = "Processing..", detail = "New points", {
    beg <- nrow(points)
    points <- points[!is.na(Long) & !is.na(Lat)]
    if (nrow(points) == 0L) {
      points <- uData$basepoints
    } else {
      points[,`:=`(Long = round(Long, 5), Lat = round(Lat, 5))]
      res <- dbPointInfo(pool, points)
      points[, `:=`(
        BGC = res$map_label,
        ForestRegion = res$forest_region,
        Site = res$site_no,
        # Only replace elevation with DEM when not provided by the user
        Elev = {
          x <- points$Elev;
          if (is.null(x)) {
            res$elevation_m
          } else {
            x[is.na(x)] <- res$elevation_m[is.na(x)];
            x
          }
        }
      )]
      # We will reuse this information to make sure points are inside BC
      onbcland <- res$onbcland
      popups <- res[, onbcland := NULL][, 
                                        paste("<b>", tools::toTitleCase(gsub("_", " ", names(.SD))), ":</b>", .SD, collapse = "<br />"),
                                        by=1:NROW(res)]$V1
      points[, popups := sapply(popups, HTML)]
      points <- points[!is.na(BGC) & onbcland]
      points <- rbindlist(list(uData$basepoints, points), fill = TRUE)
    }
    end <- nrow(points)
    # If points were dropped, alert user
    if (beg != end) {
      showModal(
        modalDialog(
          title = "Points dropped",
          paste("Points with invalid geometry or outside coverage zone are dropped."),
          easyClose = TRUE
        )
      )
    } else {
      removeModal()
    }
    return(points)
  })
}

insert_points <- function(points) {
  if (nrow(points) > 0) {
    userpoints$dt <- rbindlist(list(userpoints$dt, points), fill = TRUE)
    update_ID()
    draw_mk(tail(userpoints$dt, nrow(points)))
  }
}

## Upload points logic
observeEvent(input$upload_button,{
  showModal(
    modalDialog(
      title = "Upload points",
      fileInput(inputId = "points_upload", label =  "Delimited file (csv, txt)",
                accept = c(".csv", ".txt"), buttonLabel = "Browse...", placeholder = "My points"),
      span("The app will detect the first case-insensitive column names that match",
           tags$code("id"), ",",
           tags$code("latitude"), ",",
           tags$code("longitude"), "and",
           tags$code("elevation"), ".",
           "Short names are also supported",
           tags$code("id"), ",",
           tags$code("lat"), ",",
           tags$code("long/lng"), "and",
           tags$code("elev"), "."
      ),
      easyClose = TRUE
    )
  )
})

observeEvent(input$points_upload,{
  insert_points_file(input$points_upload$datapath)
})

insert_points_file <- function(datapath){
  # Columns detection using regular expression, hopefully column names have a match
  events <- function(e) {
    showModal(
      modalDialog(
        title = "Invalid file",
        paste("Your file could not be read, please select another file."),
        easyClose = TRUE
      )
    )
    return(NULL)
  }
  points <- tryCatch({ fread(datapath) }, error = events, warning = events)
  
  if (is.null(points)) { return(NULL) }
  
  if (nrow(points) == 0) {
    showModal(
      modalDialog(
        title = "No point in file",
        paste("Your file has no rows."),
        easyClose = TRUE
      )
    )
    return(NULL)
  }
  
  nm <- names(points)
  id_j <- head(grep("^id", nm, ignore.case = TRUE), 1)
  lat_j <- head(grep("^lat|latitude", nm, ignore.case = TRUE), 1)
  lng_j <- head(grep("^lng|^long|longitude", nm, ignore.case = TRUE), 1)
  ele_j <- head(grep("^elev|elevation", nm, ignore.case = TRUE), 1)
  
  if (length(lat_j) > 0 && length(lng_j) > 0) {
    showModal(
      modalDialog(
        title = "Column detection",
        "Using columns",
        span(tags$code(nm[id_j]), tags$code(nm[lat_j]),tags$code(nm[lng_j]), tags$code(nm[ele_j])),
        easyClose = TRUE
      )
    )
  } else {
    showModal(
      modalDialog(
        title = "Column detection",
        "Could not find latitude and longitude pair.",
        easyClose = TRUE
      )
    )
    return(NULL)
  }

  cln_j <- function(j) {if (length(j) > 0) {as.numeric(points[[j]])} else {NA_real_}}
  clc_j <- function(j) {if (length(j) > 0) {as.character(points[[j]])} else {NA_character_}}
  points <- data.table(ID = clc_j(id_j), Long = cln_j(lng_j), Lat = cln_j(lat_j),
                       Elev = cln_j(ele_j))
  points <- new_points(points)
  insert_points(points)
  set_map_bound()
}

## Add a point logic
observeEvent(input$add_dialog,{
  showModal(
    modalDialog(
      span("Enter point information"),
      textInput("add_point_id", label = "ID", placeholder = "My point", width = "220px"),
      numericInput("add_point_lat", label = "Latitude", value = 54, width = "160px", min = 48, max = 65, step = 0.001),
      numericInput("add_point_long", label = "Longitude", value = -122, width = "160px", min = -140, max = -110, step = 0.001),
      numericInput("add_point_elev", label = "Elevation", value = NULL, width = "160px", min = -500, max = 5000, step = 1),
      actionButton("add_point_submit", label = "Add point", icon("plus")),
      easyClose = TRUE
    )
  )
})

observeEvent(input$add_point_submit, {
  ID <- input$add_point_id
  ID[ID==""] <- NA_character_
  Lat <- as.numeric(input$add_point_lat)
  Long <- as.numeric(input$add_point_long)
  Elev <- as.numeric(input$add_point_elev)
  points <- new_points(data.table(ID = ID, Lat = Lat, Long = Long, Elev = Elev))
  insert_points(points)
})

## Delete selected points logic
observeEvent(input$delete_button,{
  if (length(input$points_table_rows_selected)>=1) {
    userpoints$dt <- userpoints$dt[-input$points_table_rows_selected]
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
  userpoints$dt <- uData$basepoints
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
  # Using a copy to trigger reactive change in `userpoints$dt`
  pts <- copy(userpoints$dt)
  if (nrow(points)) {
    set(pts, i = info$row[1], j = uData$pts_col, value = points)
    userpoints$dt <- pts
    clear_mk()
    draw_mk()
  } else {
    # When the point is invalid, only keep the user provided ID
    set(pts, i = info$row[1], j = 1L, value = as.character(info$value[1]))
    userpoints$dt <- pts
  }
})
