# Save internal csv to package data
# You run this manually to update package data from the csvs

library(data.table)
library(usethis)
E1 <- fread("./data-raw/data_tables/Edatopic_v11_22.csv")
S1 <- fread("./data-raw/data_tables/Feasibility_v11_22.csv")
R1 <- fread("./data-raw/data_tables/RuleTable.csv")
F1 <- fread("./data-raw/data_tables/FeasibilityLabels.csv")
zones_colours_ref <- fread("./data-raw/data_tables/WNAv11_Zone_Colours.csv", key = "classification")
subzones_colours_ref <- fread("./data-raw/data_tables/WNAv11_Subzone_Colours.csv", key = "classification")

use_data(E1, S1, R1, F1, zones_colours_ref, subzones_colours_ref, overwrite = TRUE)
# see version in ?usethis::use_data, if you all use R 3.5 and up. You should bump to version 3
# use_data(E1, S1, R1, F1, zones_colours_ref, subzones_colours_ref, overwrite = TRUE, version = 3)

# This will document your dataset in R/_data.R. See https://roxygen2.r-lib.org/articles/rd.html#datasets
# if you want to document them individually
writeLines(c(
"#' Data to be included in bccciss package",
"#'",
"#' @name bccciss-data",
"#' @docType data",
"#' @keywords data",
paste("#' @aliases", paste(gsub("\\.rda", "", dir("./data")), collapse = " ")),
"NULL"
), "./R/z_data.R")
