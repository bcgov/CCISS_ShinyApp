#' Append colour code to x.
#' @param x A data.frame like object.
#' @param key A character vector of columns in x to paste together
#' to create a merge key. Default to `c("ZONE", "SUBZONE", "VARIANT")`.
#' @param colour A data.table with a predefined key.
#' Default to `subzones_colours_ref` in this package namespace.
#' @param name A character string. Name of the column to create
#' or replace in x. Default to `fillColor`.
#' @return x with `name` column appended.
#' @details Internal dataset `zones_colours_ref` and `subzones_colours_ref`
#' use `classification` as `key`. This classification is either equal
#' to `ZONE` or a combination of `ZONE`, `SUBZONE` and `VARIANT`.
#' `NA` values are replaced by `""`.
#' @export
append_colour <- function(x, key = c("ZONE", "SUBZONE", "VARIANT"),
                          colour = subzones_colours_ref, name = "fillColor") {
  
  # Remove NAs from key columns and paste together
  clean_key <- function(key) {
    key <- lapply(key, function(k) {
      k <- x[[k]]
      k[is.na(k)] <- ""
      k
    })
    return(do.call(paste0, key))
  }
  
  # Append colours to x
  x[[name]] <- colour[clean_key(key)]$colour
  return(x)
  
}
