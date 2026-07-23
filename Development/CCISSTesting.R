library(sf)
library(terra)
library(data.table)
library(RColorBrewer)

sc <- fread("app/Test_sc.csv")
ss <- fread("app/Test_ss.csv")
std <- "85129"

# Function to format Species with footnotes
sppnotes <- function(spp, notes, textstyle) {
  ret <- vector("list", length(spp))
  for (i in seq_len(length(spp))) {
    fn <- paste0(sort(as.integer(unique(unlist(notes[i])))), collapse = ",")
    ret[[i]] <- tags$span(tags$span(style = textstyle[i], spp[i]),
                          tags$sup(fn, .noWS = htmltools:::noWSOptions),
                          if (i < length(spp)) {", "} else {""}, .noWS = htmltools:::noWSOptions)
  }
  ret[["style"]] <- "white-space:normal;"
  do.call(tags$td, ret)
}

sppnotes_cciss <- function(spp, textstyle) {
  ret <- vector("list", length(spp))
  for (i in seq_len(length(spp))) {
    ret[[i]] <- tags$span(tags$span(style = textstyle[i], spp[i]),
                          if (i < length(spp)) {", "} else {""}, .noWS = htmltools:::noWSOptions)
  }
  ret[["style"]] <- "white-space:normal; border-left: 1px solid;"
  do.call(tags$td, ret)
}


sc[,TxtCciss := ""]
if(!is.null(ss)){
  ss <- ss[Standard %in% std]
  ss[, TextStyle := ""]
  
  ss[sc, on = "Species==Spp", `:=`(ProjFeas = suppressWarnings(as.integer(i.ccissFeas)),Curr = suppressWarnings(as.integer(i.Curr)))]
  setnafill(ss, fill = 4L, cols = c("ProjFeas","Curr"))
  ss[Curr > ProjFeas, TextStyle := "color:green"]
  ss[Curr < ProjFeas, TextStyle := "color:red"]
  ss[!ProjFeas %in% c(1,2,3), TextStyle := "color:red;text-decoration:line-through"]

  # cciss colouring
  sc[ss, on = "Spp == Species", CFRGSuit := i.Suitability]
  sc[ccissFeas < Curr, TxtCciss := "color:green"]
  sc[ccissFeas > Curr, TxtCciss := "color:red"]
  sc[!Curr %in% c(1,2,3), TxtCciss := "color:purple"]
  sc[is.na(CFRGSuit), Spp := paste0("(",Spp,")")]

  si <- stocking_info[Standard == std]
  sh <- stocking_height[Standard == std]
  sblock <- list(
    
    #tags$h6("CFRG: ", tags$b(si$Region, .noWS = c("before", "after")), "Standards_ID: ", paste(ss[!is.na(Standard), unique(Standard)], collapse = ", "), .noWS = "inside"),
    tags$h5("CFRG Standards ID: ", paste(ss[!is.na(Standard), unique(Standard)], collapse = ", "), tags$p(si$Region, .noWS = c("before", "after")), .noWS = "inside"),
    tags$table(style = "max-width: 100%; white-space: nowrap;",
               # Report formatting gray out the first row, so faking a row
               tags$tr(
                 tags$td(width = "50%", style = "vertical-align: top; padding:0; background-color:white; border:1px solid black",
                         
                         tags$table(
                           #width = "500px",
                           
                           tags$th(
                             tags$td(tags$b("CFRG"), style = "border-left: 1px solid; width: 33%;"),
                             tags$td("",style = "border-left: 1px solid; width: 5%;"),
                             tags$td(tags$b("CCISS"), style = "border-left: 1px solid; width: 33%;")
                           ),
                           
                           
                           tags$tr(
                             tags$td("Primary", style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & Suitability %in% 1L, sppnotes(Species, Footnotes, TextStyle)],
                             tags$td("E1", style = "border-left: 1px solid;"),
                             sc[!is.na(Spp) & ccissFeas %in% "1", sppnotes_cciss(Spp,TxtCciss)],
                             style = "border-bottom:1px solid black;"
                           ),
                           
                           #tags$hr(style = "padding: 0px; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
                           
                           tags$tr(
                             tags$td("Secondary", style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & Suitability %in% 2L, sppnotes(Species, Footnotes, TextStyle)],
                             tags$td("E2", style = "border-left: 1px solid;"),
                             sc[!is.na(Spp) & ccissFeas %in% "2", sppnotes_cciss(Spp,TxtCciss)],
                             style = "border-bottom:1px solid black;"
                           ),
                           
                           tags$tr(
                             tags$td("Tertiary", style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & Suitability %in% 3L, sppnotes(Species, Footnotes, TextStyle)],
                             tags$td("E3", style = "border-left: 1px solid;"),
                             sc[!is.na(Spp) & ccissFeas %in% "3", sppnotes_cciss(Spp,TxtCciss)],
                             style = "border-bottom:1px solid black;"
                           ),
                           
                           # tags$tr(
                           #   tags$td("Trial",style = "border-right: 1px solid;" ),
                           #   tags$td(""),
                           #   sc[!is.na(Spp) & EstabFeas == "Trial", sppnotes_cciss(Spp,TxtCciss)],
                           #   style = "border-bottom:1px solid black;"
                           # ),
                           tags$tr(
                             tags$td("Broadleaf", style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & Suitability %in% 0L, sppnotes(Species, Footnotes, TextStyle)],
                             tags$td("", style = "border-left: 1px solid;"),
                             style = "border-bottom:1px solid black;"
                           ),
                           
                           tags$tr(
                             tags$td("Preferred (p)",style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & PreferredAcceptable %in% "P", sppnotes(Species, Footnotes, TextStyle)],
                             "",
                           ),
                           tags$tr(
                             tags$td("Acceptable (a)",style = "border-right: 1px solid;"),
                             ss[!is.na(Species) & PreferredAcceptable %in% "A", sppnotes(Species, Footnotes, TextStyle)],
                             "",
                             style = "border-bottom:1px solid black;"
                           ),
                           tags$tr(
                             tags$td(colspan = "2", style = "white-space:normal; vertical-align: top; padding:0; background-color:white; border:none",
                                     tags$small(tags$b("Footnotes")),
                                     tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
                                     {
                                       fn <- ss[PreferredAcceptable %in% c("A", "P") | Suitability %in% 1:3, sort(as.integer(unique(unlist(Footnotes))))]
                                       fnt <- footnotes[match(fn, `Revised Footnote`), `Revised Footnote Text`]
                                       fnshiny <- mapply(function(footnote, text) {list(tags$sup(footnote), tags$small(text), tags$br())}, fn, fnt, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                       do.call(span, fnshiny)
                                     }
                             )
                           )
                         )
                 ),
                 tags$td(width = "50%", style = "vertical-align: top; padding:0px 0px 0px 8px; background-color:white; border:1px solid black",
                         tags$small(tags$b("Stocking (i) - well spaced/ha")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
                         tags$table(
                           #width = "20%",
                           tags$tr(
                             tags$td(tags$b("Target")),
                             tags$td(tags$b("Min pa")),
                             tags$td(tags$b("Min p")),
                             tags$td(tags$b("Regen Delay (max yrs)"))
                           ),
                           tags$tr(
                             tags$td(si$StockingTarget),
                             tags$td(si$StockingMINpa),
                             tags$td(si$StockingMINp),
                             tags$td(si$StockingDelay)
                           )
                         ),
                         tags$br(),
                         tags$small(tags$b("Free Growing Guide")),
                         tags$hr(style = "padding: 0; margin: 0 0 3px 0; height: 2px; background-color: #003366; border: 0px"),
                         tags$table(
                           #width = "100%",
                           tags$tr(
                             tags$td(tags$b("Earliest (yrs)")),
                             tags$td(tags$b("Latest(yrs)")),
                             tags$td(tags$b("Min Height (m)")),
                             tags$td(tags$b("Min Height (m)"))
                           ),
                           tags$tr(
                             tags$td(si$AssessmentEarliest),
                             tags$td(si$AssessmentLatest),
                             tags$td(style = "white-space: normal;", sh[!Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")]),
                             tags$td(style = "white-space: normal;", sh[Flag %in% TRUE, paste(Species, Height, sep = ": ", collapse = ", ")])
                           )
                         )
                 )
               )
    ),
    h6("Legend"),
    HTML(
      paste0(
        '<svg viewBox="0 0 1 1" height="14px" width="14px"><rect height=1 width=1 style="fill : ',
        c("green", "red", "purple"),
        '" /><span style="vertical-align:middle">&nbsp;',
        c("Improving", "Decreasing", "Adding"),
        '</span>',
        collapse = "<br />"
      )
    )
  )
}


test <-       HTML(
  paste0(
    '<svg viewBox="0 0 1 1" height="18px" width="18px">',
    # The first three filled boxes
    '<rect height=1 width=1 style="fill:', 
    c("blue", "red", "purple"), 
    '"/>',
    '</svg>',
    '<span style="vertical-align:middle">&nbsp;',
    c("Increasing", "Decreasing", "Becoming Suitable"),
    '</span>',
    collapse = "<br />"
  ),
  
  # Add the open red box with strikethrough
  '<br />',
  '<svg viewBox="0 0 1 1" height="18px" width="18px" style="overflow:visible">',
  '<rect height="1" width="1" style="fill:none;stroke:red;stroke-width:0.1"/>',
  '<line x1="-0.2" y1="0.5" x2="1.2" y2="0.5" style="stroke:red;stroke-width:0.15"/>',
  '</svg>',
  '<span style="vertical-align:middle">&nbsp;Becoming Unsuitable</span>'
)

legend_html <- paste0(
  # First three boxes (filled)
  '<svg viewBox="0 0 1 1" height="18px" width="18px" style="vertical-align:middle">',
  '<rect height="1" width="1" style="fill:', 
  c("blue", "red", "purple"), 
  '"/>',
  '</svg>',
  '<span style="vertical-align:middle; position:relative; top:-2px">&nbsp;',
  c("Increasing", "Decreasing", "Becoming Suitable"),
  '</span>',
  collapse = "<br />"
)

# Add the “Becoming Unsuitable” entry
legend_html <- HTML(paste0(
  legend_html,
  '<br />',
  '<svg viewBox="0 0 1 1" height="18px" width="18px" style="vertical-align:middle; overflow:visible">',
  '<rect height="1" width="1" style="fill:none;stroke:red;stroke-width:0.1"/>',
  '<line x1="-0.2" y1="0.5" x2="1.2" y2="0.5" style="stroke:red;stroke-width:0.15"/>',
  '</svg>',
  '<span style="vertical-align:middle; position:relative; top:-2px">&nbsp;Becoming Unsuitable</span>'
))

html_print(legend_html)

rproj <- rast("kalum_pem/Kalum_Cw_2081_2100.tif")
rcurr <- rast("kalum_pem/Kalum_Cw_Curr.tif")

rdif <- rcurr - rproj

rpem <- rast("kalum_pem/Kalum_PEM_Raster2.tif")
plot(rpem)
crwlk <- fread("kalum_pem_crosswalk.csv")
data(S1)
cwsuit <- S1[spp == "Cw",]
crwlk[cwsuit, Curr := i.newfeas, on = "SS_NoSpace==ss_nospace"]
crwlk[is.na(Curr), Curr := 5]

pem <- as.data.table(rpem, cells = TRUE) |> as.data.table()
pem[crwlk, Curr := i.Curr, on = "rast_id"]
proj <- as.data.table(rproj, cells = TRUE) |> as.data.table()
pem[proj, Newsuit := i.rast_id, on = "cell"]
pem[is.na(Newsuit), Newsuit := 5]
pem[is.na(Curr), Curr := 5]

dat_spp <- pem[Curr < 3.5 | Newsuit < 3.5,]
dat_spp[,FeasChange := Curr - Newsuit]
dat_spp[Newsuit > 3.5 & Curr <= 3, FeasChange := -10]
dat_spp[Curr > 3.5, FeasChange := round(FeasChange) * 10]
dat_spp[,FeasChange := round(FeasChange/0.5)*0.5]
dat_spp[,FeasRound := round(Newsuit)]
dat_spp[,CurrRound := round(Curr)]
dat_spp[CurrRound > 3, CurrRound := 999]
dat_spp[FeasRound > 3, FeasRound := 999]

breakpoints.change <- c(c(seq(-2.5,2.5,0.5),-10,10,20,30) + 15, 999)
palette.change <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,5,6)], brewer.pal(11,"RdBu")[c(7,8,9,10,11)],"#000000", brewer.pal(9,"YlOrRd")[1:3],"#FFFFFF") # nolint
##mean change colours
change_cols <- data.table(value = breakpoints.change, Colour = palette.change)
change_cols[value == 15, Colour := "#DFDFDF"]

final_dem <- copy(rpem)
final_dem[!is.na(final_dem)] <- 999
final_dem[dat_spp$cell] <- dat_spp$FeasChange + 15
final_rgb <- subst(final_dem, change_cols$value, t(col2rgb(change_cols$Colour,alpha = TRUE)),names = c("red","green", "blue","alpha"))
writeRaster(final_rgb, "kalum_pem/MeanChange_2081_2100.tif", overwrite = TRUE)




pem_bnd <- vect("../Common_Files/Dist_Pkg_PEM_Skeena.gdb/", layer = "TEI_Project_Boundaries")
pem <- vect("../Common_Files/Dist_Pkg_PEM_Skeena.gdb/", query = "select * FROM TEI_Short_Tbl where PROJ_NAME = 'Kalum PEM'")

pem <- st_read("../Common_Files/Dist_Pkg_PEM_Skeena.gdb/",
               query = "select * FROM TEI_Short_Tbl where PROJ_NAME = 'Kalum PEM'")
test <- st_drop_geometry(pem[c("BGC_LBL","SITES_LBL1","SITEMCLBL1")])
test2 <- as.data.table(test)
test2[, SITES_LBL1 := gsub("\\|.*","",SITES_LBL1)]
test2[,SS_NoSpace := paste0(BGC_LBL,"/",SITES_LBL1)]
lbs <- unique(test2$SS_NoSpace)
cr_tab <- data.table(SS_NoSpace = lbs, rast_id = seq_along(lbs))

pem <- pem[c("BGC_LBL","SITES_LBL1")]
pem$SS_NoSpace <- test2$SS_NoSpace
pem <- as.data.table(pem)
pem[cr_tab, rast_id := i.rast_id, on = "SS_NoSpace"]

pem2 <- st_as_sf(pem)
pem2 <- vect(pem2)
pem2 <- project(pem2, crs(dem))
dem <- rast("../CCISS_Spatial/BC_DEM_200m.tif")


pemdem <- rasterize(pem, dem, field = "rast_id")
writeRaster(pemdem, "Kalemm_PEM_Full.tif")
cells_aoi <- as.data.frame(pemdem, )
pemdem <- trim(pemdem)

dem2 <- disagg(pemdem, fact = 4)
pemdem <- rasterize(pem2, dem2, field = "rast_id")

writeRaster(pemdem, "Kalum_PEM_Raster2.tif")
writeVector(pem2, "Kalum_PEM.gpkg")

pemdem <- rast("Kalum_PEM_Raster2.tif")
demhr <- rast("../Common_Files/WNA_DEM_SRT_30m_cropped.tif")
temp <- crop(demhr, pemdem)
pemdem2 <- resample(temp, pemdem)
pemdem2[is.na(pemdem)] <- NA
plot(pemdem2)
writeRaster(pemdem2, "Kalum_DEM_50m.tif")
pem <- st_read("Kalum_PEM.gpkg")

cr_tab <- as.data.table(st_drop_geometry(pem[c("SS_NoSpace","rast_id")]))
cr_tab <- unique(cr_tab)

bec13_cr <- fread("Kalum Cw suitability crosswalk.csv")
bec13_cr[,ss_pem := paste0(bgc,"/",`Site Series Number`)]
bec13_cr <- unique(bec13_cr[,.(ss_nospace,ss_pem)])
setnames(cr_tab, old = "SS_NoSpace", new = "ss_pem")
cr_tab[bec13_cr, SS_NoSpace := i.ss_nospace, on = "ss_pem"]
fwrite(cr_tab, "bec13_crosswalk.csv")

sc <- fread("app/cciss_testing.csv")

hr <- rast("../../Masters/fine_example.nc")
dem_hr <- resample(dem, hr)
dem_hr <- dem_hr - mean(values(dem_hr), na.rm = TRUE)
dem_hr[is.na(dem_hr)] <- 0
dem_hr <- dem_hr/sd(values(dem_hr))
plot(dem_hr)
writeCDF(dem_hr, "BuMo_dem.nc")

ss[sc, on = "Species==Spp", ProjFeas := suppressWarnings(as.integer(i.ccissFeas))]
setnafill(ss, fill = 4L, cols = "ProjFeas")
ss[Suitability > ProjFeas, TextStyle := "color:green"]
ss[Suitability < ProjFeas, TextStyle := "color:red"]
ss[!ProjFeas %in% c(1,2,3), TextStyle := "color:red;text-decoration:line-through"]
ss[Suitability == 0, TextStyle := NA]
#browser()
# cciss colouring
sc[ss, on = "Spp == Species", CFRGSuit := i.Suitability]
sc[ccissFeas < CFRGSuit, TxtCciss := "color:green"]
sc[ccissFeas > CFRGSuit, TxtCciss := "color:red"]
sc[!CFRGSuit %in% c(1,2,3), TxtCciss := "color:purple"]
sc[CFRGSuit == 0, TxtCciss := NA]

si <- stocking_info[Standard == std]
sh <- stocking_height[Standard == std]
temp <- list(
  tags$h5("CFRG Standards_ID: Does Not Exist", .noWS = "inside"),
  tags$table(style = "max-width: 100%; white-space: nowrap;",
             # Report formatting gray out the first row, so faking a row
             tags$tr(
               tags$td(width = "50%", style = "vertical-align: top; padding:0; background-color:white; border:1px solid black",
                       
                       tags$table(
                         width = "500px",
                         
                         tags$th(
                           tags$td(tags$b("CFRG"), style = "border-left: 1px solid;"),
                           tags$td(tags$b("CCISS"), style = "border-left: 1px solid;")
                         ),
                         
                         
                         tags$tr(
                           tags$td("Primary/E1", style = "border-right: 1px solid;"),
                           tags$td(""),
                           sc[!is.na(Spp) & ccissFeas %in% "1", sppnotes_cciss(Spp,TxtCciss)]
                         ),
                         
                         #tags$hr(style = "padding: 0px; margin: 0 0 3px 0; height: 2px; background-color: darkgreen; border: 0px"),
                         
                         tags$tr(
                           tags$td("Secondary/E2", style = "border-right: 1px solid;"),
                           tags$td(""),
                           sc[!is.na(Spp) & ccissFeas %in% "2", sppnotes_cciss(Spp,TxtCciss)]
                         ),
                         
                         tags$tr(
                           tags$td("Tertiary/E3", style = "border-right: 1px solid;"),
                           tags$td(""),
                           sc[!is.na(Spp) & ccissFeas %in% "3", sppnotes_cciss(Spp,TxtCciss)]
                         ),
                         
                         tags$tr(
                           tags$td("Trial",style = "border-right: 1px solid;" ),
                           tags$td(""),
                           sc[!is.na(Spp) & EstabFeas == "Trial", sppnotes_cciss(Spp,TxtCciss)]
                         ),
                         
                         tags$tr(
                           tags$td("Preferred (p)",style = "border-right: 1px solid;"),
                           tags$td(""),
                           sc[!is.na(Spp) & PrefAcc %in% "P", sppnotes_cciss(Spp,TxtCciss)],
                         ),
                         tags$tr(
                           tags$td("Acceptable (a)",style = "border-right: 1px solid;"),
                           tags$td(""),
                           sc[!is.na(Spp) & PrefAcc %in% "A", sppnotes_cciss(Spp,TxtCciss)],
                           style = "border-bottom:1px solid black;"
                         ),
                       )
               )
             )
  )
)

in_viewer <- function(x){
  tab <- paste(capture.output(x), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}
in_viewer(temp[[2]])


library(sf)
dat <- st_read("spatial_app/bdy/bdy.BuMo.shp")
plot(dat)
st_write(dat, "test_file.gpkg")
