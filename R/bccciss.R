# import compiled code from this package
#' @useDynLib bccciss, .registration = TRUE
NULL

bccciss_envir <- environment()
.onLoad <- function(...) {
  makeActiveBinding("plugins", function() {
    bccciss::plugins_loaded()$plugins
  }, env = bccciss_envir)
  
  makeActiveBinding("wms", function() {
    bccciss::wms_loaded()$wms
  }, env = bccciss_envir)
}