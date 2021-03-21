# Save internal csv to package data
# You run this manually to update package data from the csvs

library(data.table)
library(usethis)
library(readxl)
E1 <- fread("./data-raw/data_tables/Edatopic_v11_22.csv")
S1 <- fread("./data-raw/data_tables/Feasibility_v11_22.csv")
R1 <- fread("./data-raw/data_tables/RuleTable.csv")
F1 <- fread("./data-raw/data_tables/FeasibilityLabels.csv", key = "SuitDiff")
T1 <- fread("./data-raw/data_tables/Tree speciesand codes_2.0_2May2019.csv", key = "TreeCode")
V1 <- fread("./data-raw/data_tables/Variables_ClimateBC.csv", key = "Code")
zones_colours_ref <- fread("./data-raw/data_tables/WNAv11_Zone_Colours.csv", key = "classification")
subzones_colours_ref <- fread("./data-raw/data_tables/WNAv11_Subzone_Colours.csv", key = "classification")

# StockingStds
stocking_standards_v11 <- fread("./data-raw/data_tables/StockingStds/StockStands_v11.csv", key = c("Region", "ZoneSubzone", "SS_NoSpace", "Species"), colClasses = c("Standard" = "numeric"))
stocking_info_v10 <- fread("./data-raw/data_tables/StockingStds/StockingInfo_v10.csv", key = "Standard", colClasses = c("Standard" = "numeric"))
stocking_height_v10 <- fread("./data-raw/data_tables/StockingStds/StockingHeight_v10.csv", key = c("Standard", "Species"), colClasses = c("Standard" = "numeric"))
crosswalk <- fread("./data-raw/data_tables/StockingStds/Crosswalk.csv", key = "Modeled")

# Massaging data
# Some Standards end with CC, discarding them
stocking_info_v10[ , Standard := as.numeric(Standard)]
setkey(stocking_info_v10, "Standard")
# Remove standards with no matching Standard in main table
stocking_info_v10 <- stocking_info_v10[!is.na(Standard) & Standard %in% stocking_standards_v11$Standard]

# Duplicated pairs
dupPairs <- function(data) {
  data[duplicated(data[, j = key(data), with=FALSE]) | duplicated(data[, j = key(data), with=FALSE], fromLast = TRUE), j = .SD, by=key(data)]
}

# Checks standards for duplicates
dupPairs(stocking_standards_v11)
dupPairs(stocking_info_v10)
dupPairs(stocking_height_v10)

# Remove duplicates for now, keeping the first of each combination
remDups <- function(d) {
  d[!duplicated(d[, key(d), with = FALSE])]
}
stocking_standards_v11 <- remDups(stocking_standards_v11)
stocking_info_v10 <- remDups(stocking_info_v10)
stocking_height_v10 <- remDups(stocking_height_v10)

# Stocking standards all together
stocking_standards <- merge(
  x = stocking_standards_v11,
  y = stocking_info_v10[,.(SiteSeriesName, Standard, StockingTarget, StockingMINpa, StockingMINp, StockingDelay, AssessmentEarliest, AssessmentLatest, Flag)],
  by = "Standard",
  all.x = TRUE
)

stocking_standards <- merge(
  x = stocking_standards,
  y = stocking_height_v10[,.(Standard, Species, Height)],
  by = c("Standard", "Species"),
  all.x = TRUE
)

stocking_standards <- merge(
  x = stocking_standards,
  y = stocking_height_v10[Species == "Others",.(Standard, Height)],
  by = "Standard",
  all.x = TRUE
)

stocking_standards[, Height := {x <- Height.x; x[is.na(x)] <- Height.y[is.na(x)]; x}]
stocking_standards$Height.x <- NULL
stocking_standards$Height.y <- NULL
stocking_standards[, Footnotes := list(list({x <- do.call(c, .SD); x[!x %chin% c(NA, "")]})), by=1:NROW(stocking_standards), .SDcols = FN1:FN5]
stocking_standards[, c("FN1","FN2","FN3","FN4","FN5") := NULL]

# add-in crosswalk rows to complete standards dataset
# Gettings standards that would be substitute according to crosswalk
a <- stocking_standards[ZoneSubzone %chin% crosswalk$Tables]
# Generate all possible BGC that would use a substitute
a <- a[crosswalk, on = c(ZoneSubzone = "Tables"), allow.cartesian = TRUE, nomatch = NULL]
# Checking if any of those already have a match in the standards table
nrow(stocking_standards[a, on = c(Region = "Region", ZoneSubzone = "Modeled", SS_NoSpace = "SS_NoSpace", Species = "Species"), nomatch = NULL])
# Does not seems like it, so it is safe to add all of them
a[, `:=`(ZoneSubzone = Modeled, Modeled = NULL)]
stocking_standards <- rbindlist(list(stocking_standards, a))
setkey(stocking_standards, Region, ZoneSubzone, SS_NoSpace, Species)
# Recheck for dups
dupPairs(stocking_standards)
# All good

# Notes
footnotes <- read_xlsx("./data-raw/data_tables/StockingStds/Revised Reference Guide Footnotes.xlsx", "Sheet1")
setDT(footnotes)
footnotes <- footnotes[Remove %chin% c(NA, "")]
footnotes[, `Restrictive Footnote` := !`Restrictive Footnote` %chin% c(NA, "")]
footnotes[, `Advisory Footnote` := !`Advisory Footnote` %chin% c(NA, "")]
footnotes[, `Geographic Restriction` := !`Geographic Restriction` %chin% c(NA, "")]
footnotes[, `Site Condition` := !`Site Condition` %chin% c(NA, "")]
footnotes[, `Pest/Disease` := !`Pest/Disease` %chin% c(NA, "")]
footnotes[, Administrative := !Administrative %chin% c(NA, "")]

# Silvical characteristics
silvics_tol <- setDT(read_xlsx("./data-raw/data_tables/StockingStds/EcoSilvic Tables_Klinka.xlsx", range = "Tolerance!A2:I35"))
names(silvics_tol)[4:9] <- paste(names(silvics_tol)[4:9], "tolerance")
silvics_regen <- setDT(read_xlsx("./data-raw/data_tables/StockingStds/EcoSilvic Tables_Klinka.xlsx", range = "Silvics!A2:H35"))
silvics_mature <- setDT(read_xlsx("./data-raw/data_tables/StockingStds/EcoSilvic Tables_Klinka.xlsx", range = "Silvics2!A2:I35"))
silvics_resist <- setDT(read_xlsx("./data-raw/data_tables/StockingStds/EcoSilvic Tables_Klinka.xlsx", range = "Resistance!A3:I36"))
names(silvics_resist) <- c("Life form", "Tree code", "Tree species", "Snow Resistance Class", "Wind Resistance Class",
                           "Fire Risk Class", "Insect Risk Class", "Fungi Risk Class", "Other Risk Class")

silvics <- merge(
  x = silvics_tol,
  y = silvics_regen[, .(`tree species`, `reproduction capacity`, `seed dissemination capacity`, `potential for regeneration`, `potential for regeneration in the open`, `potential for initial growth rate (over 5 years)`)],
  by.x = "Tree species",
  by.y = "tree species",
  all.x = TRUE
)

silvics <- merge(
  x = silvics,
  y = silvics_mature[, .(`Tree species`, `response of advance regeneration to release`, `self- pruning capacity in dense stands`, `crown spatial requirements`, `light conditions beneath closed-canopy, mature stands`, `potential productivity`, longevity)],
  by = "Tree species",
  all.x = TRUE
)

silvics <- merge(
  x = silvics,
  y = silvics_resist[, .(`Tree species`, `Snow Resistance Class`, `Wind Resistance Class`, `Fire Risk Class`,`Insect Risk Class`,`Fungi Risk Class`,`Other Risk Class`)],
  by = "Tree species",
  all.x = TRUE
)

names(silvics) <- tools::toTitleCase(names(silvics))
names(footnotes) <- tools::toTitleCase(names(footnotes))
names(stocking_standards) <- tools::toTitleCase(names(stocking_standards))

use_data(E1, S1, R1, F1, T1, V1, zones_colours_ref, subzones_colours_ref, stocking_standards, footnotes, silvics, overwrite = TRUE)
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
