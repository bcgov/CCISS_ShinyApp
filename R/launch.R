
#' Launch CCISS APP in browser()
#' @importFrom rmarkdown run
#' @export
launch_app <- function() {
  rmarkdown::run(
    file = system.file("application/demo.Rmd", package = "CCISS"),
    shiny_args = list(appDir = system.file("application", package = "CCISS"))
  )
}