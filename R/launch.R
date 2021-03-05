
#' Launch CCISS APP in browser()
#' @importFrom rmarkdown run
#' @export
launch_app <- function() {
  rmarkdown::run(
    file = system.file("application/app.Rmd", package = "bccciss"),
    shiny_args = list()
  )
}