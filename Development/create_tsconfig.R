library(tools)
library(jsonlite)

config <- list(options = list(paths = list(root = "", styles = "", mbtiles = ""), 
                              formatQuality = list(jpeg = 80L, webp = 90L), maxScaleFactor = 3L, 
                              maxSize = 2048L, pbfAlias = "pbf", serveAllFonts = FALSE, 
                              serveAllStyles = FALSE, serveStaticMaps = TRUE, tileMargin = 0L), 
               styles = list(basic = list(serve_rendered = FALSE, tilejson = list(
                 type = "overlay", bounds = c(-139.06131, 48.29681, -114.05415, 
                                              60.00478))), rendered = list(serve_rendered = TRUE, tilejson = list(
                                                type = "overlay", bounds = c(-139.06131, 48.29681, -114.05415, 
                                                                             60.00478)))))


files <- list.files("./", pattern = "*.mbtiles")
bnm <- file_path_sans_ext(files)

config$data <- list()
for(i in 1:length(bnm)){
  config$data[[bnm[i]]] <- list("mbtiles" = files[i])
}

writeLines(jsonlite::prettify(jsonlite::toJSON(config, auto_unbox = TRUE)), con = "config.json") 
