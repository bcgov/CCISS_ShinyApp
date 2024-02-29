## A draft app for visualization of climate change in defined areas
## author: Colin Mahony colin.mahony@gov.bc.ca

# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# library(shiny)
library(RColorBrewer)
library(stinepack) # for interpolation splines
library(scales)
library(plotrix)
library(car)
library(DT)
# library(mapview)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(sf)
library(raster)
library(markdown)

# Increase the maximum upload size to 60 MB 
options(shiny.maxRequestSize = 60*1024^2)

# setwd("C:/Users/CMAHONY/OneDrive - Government of BC/Shiny_Apps/CCISS_ShinyApp/spatial_app") # for local testing

studyarea <- "BC"
indir <- paste("data", studyarea, "", sep="/")

edatopes <- c("B2", "C4", "D6")
edatope.names <- c("Poor-subxeric", "Medium-mesic", "Rich-hygric")

scenarios <- c("ssp126", "ssp245")
scenario.names=c("SSP1-2.6", "SSP2-4.5")
scenario <- scenarios[2] # only one scenario currently supported. would need to revise the script to support more than one scenario. 

period.names=c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

bdy <- st_read(dsn = paste("bdy/bdy", studyarea, "shp", sep="."))
P4S.epsg <- st_crs("+init=epsg:4326") # web mercator
bdy <- st_transform(bdy, P4S.epsg)

bbox <- as.vector(st_bbox(bdy))

options(scipen=999)
## Load the input data

modelMetadata <- read.csv("data/ModelList.csv")
# str(ecoprov.climate)

## CLIMATE DATA

clim.meanChange <- read.csv(paste(indir,"clim.meanChange.csv", sep=""))[,-c(1:4)]
clim.refMean <- read.csv(paste(indir,"clim.refMean.csv", sep=""))

variables <- names(clim.meanChange)
variable.names <- read.csv("data/Variables_ClimateBC.csv")
variables.select <- variables[-grep("01|02|03|04|05|06|07|08|09|10|11|12|RH|Rad|MAR", variables)]

variable.types <- rep(NA, length(variables))
variable.types[grep("PPT|DD|PAS|NFFD|Eref|FFP|CMD|MAP|MSP|AHM|SHM|Rad|MAR", variables)] <- "ratio"
variable.types[grep("Tmax|Tmin|Tave|MAT|MWMT|MCMT|TD|EMT|EXT|bFFP|eFFP|CMI", variables)] <- "interval"
variable.types[grep("RH", variables)] <- "pct"

# Convert absolute change to relative change for zero-limited variables
clim.meanChange.ratio <- clim.meanChange
for(i in which(variable.types=="ratio")){clim.meanChange.ratio[,i] <- clim.meanChange.ratio[,i]/clim.refMean[,i]}  #set zero values to one, to facilitate log-transformation

identity <- read.csv(paste(indir,"clim.meanChange.csv", sep=""), stringsAsFactors = F)[,c(1:4)]

scenarios <- sort(unique(identity$SSP))
periods <- unique(identity$PERIOD)
gcms <- unique(identity$GCM)[-1]
mods <- substr(gcms,1,2)

ColScheme.gcms <- c(brewer.pal(n=length(gcms), "Paired"))

## BGC PROJECTIONS

bgc.names <- read.csv(paste("data/All_BGCs_v11_21.csv", sep="."), stringsAsFactors = F) #ISSUE: likely need to update this. 

bgcs.native <- as.vector(unlist(read.csv(paste(indir,"bgcs.native.csv", sep=""))))
zones.native <- as.vector(unlist(read.csv(paste(indir,"zones.native.csv", sep=""))))

## projected area of biogeoclimatic units
bgc.area <- read.csv(paste(indir,"PredSum.bgc.csv", sep=""))
temp <- bgc.area[-which(bgc.area$GCM=="ensembleMean"),] #remove ensemble vote
temp.ens <- bgc.area[which(bgc.area$GCM=="ensembleMean"),] #ensemble vote
bgc.area <- bgc.area[,-c(1:5)] # matches the "identity" table
bgc.area[is.na(bgc.area)] <- 0
bgcs.all <- names(bgc.area)
r <- raster(paste(indir,"BGC.pred.ref.tif", sep=""))
cellarea <- (res(r)[2]*111)*(res(r)[1]*111*cos(mean(extent(r)[3:4]) * pi / 180))
bgc.area <- bgc.area*cellarea

#BGC subzone color scheme
bgccolors <- read.csv("data/WNAv11_Subzone_Colours.csv", stringsAsFactors = F)
zonecolors <- read.csv("data/WNAv11_Zone_Colours.csv", stringsAsFactors = F)
zonecolors.BC <- read.csv("data/BGCzone_Colorscheme.csv", stringsAsFactors = F)
zonecolors$colour[match(zonecolors.BC$zone, zonecolors$classification)] <- as.character(zonecolors.BC$HEX)

## projected area of biogeoclimatic zones
bgc.zones <- bgcs.all
for(i in zonecolors$classification){ bgc.zones[grep(i,bgcs.all)] <- i }
zones <- unique(bgc.zones)
zone.area <- data.frame(matrix(NA, length(bgc.area[,1]), length(zones)))
names(zone.area) <- zones
for(zone in zones){
  s <- which(bgc.zones==zone)
  zone.area[, which(zones==zone)] <- if(length(s)>1) apply(bgc.area[,s], 1, sum, na.rm=T) else bgc.area[,s]
}
zones.all <- names(zone.area)

## add colors for units not in color scheme.
needcolor <- bgcs.all[-which(bgcs.all%in%bgccolors$classification)]
needcolor.zone <- needcolor
for(i in zonecolors$classification){ needcolor.zone[grep(i,needcolor)] <- i }
bgccolors <- rbind(bgccolors, data.frame(classification=needcolor, colour=zonecolors$colour[match(needcolor.zone, zonecolors$classification)]))

## simplify and structure the biogeoclimatic area tables
bgc.area <- bgc.area[,rev(order(apply(bgc.area, 2, sum)))] #sort by total projection area
totalarea <- sum(bgc.area[1,]) #historical distribution
bgc.area <- bgc.area[,-which(apply(bgc.area, 2, sum)/totalarea < 0.25)] #remove small units
bgc.area <- bgc.area[,order(names(bgc.area))] #sort by total projection area
bgcs <- names(bgc.area)

zone.area <- zone.area[,rev(order(apply(zone.area, 2, sum)))] #sort by total projection area
totalarea <- sum(zone.area[1,]) #historical distribution
s <- which(apply(zone.area, 2, sum)/totalarea < 0.5)
if(length(s)>0) zone.area <- zone.area[,-s] #remove small units
zones <- names(zone.area)

# biogeoclimatic spatial data
bgc.pred.ref <- raster(paste(indir,"BGC.pred.ref.tif", sep=""))
bgc.pred.2001 <- raster(paste(indir,"BGC.pred.hist.2001_2020.tif", sep=""))
id.kkz <- read.csv(paste(indir,"id.kkz.csv", sep=""))
sims <- paste(id.kkz$GCM, substr(id.kkz$RUN,1,2), sep="_")
sims[which(id.kkz$GCM=="ensembleMean")] <- "Ensemble mean"
levels.bgc <- read.csv("data/levels.bgc.csv")[,1]

## persistence and expansion of bgc units

    #bgc total table
    bgc.count <- read.csv(paste(indir,"PredSum.bgc.csv", sep=""))
    bgc.count[is.na(bgc.count)] <- 0
    bgc.count <- bgc.count[,-c(1:5)] # matches the "identity" table

    #zone total table
    zone.count <- read.csv(paste(indir,"PredSum.zone.csv", sep=""))
    zone.count[is.na(zone.count)] <- 0
    zone.count <- zone.count[,-c(1:5)] # matches the "identity" table

    #bgc table for home range
    bgc.count.home <- read.csv(paste(indir,"PredSum.bgc.home.csv", sep=""))
    bgc.count.home[is.na(bgc.count.home)] <- 0
    bgc.count.home <- bgc.count.home[,-c(1:5)] # matches the "identity" table

    #zone table for home range
    zone.count.home <- read.csv(paste(indir,"PredSum.zone.home.csv", sep=""))
    zone.count.home[is.na(zone.count.home)] <- 0
    zone.count.home <- zone.count.home[,-c(1:5)] # matches the "identity" table

    ## calculate persistence and expansion tables
    bgc.persistence <- sweep(bgc.count.home, MARGIN=2,unlist(bgc.count[1,match(names(bgc.count.home), names(bgc.count))]), '/' )
    zone.persistence <- sweep(zone.count.home, MARGIN=2,unlist(zone.count[1,match(names(zone.count.home), names(zone.count))]), '/' )
    bgc.expansion <- sweep(bgc.count[,match(names(bgc.count.home), names(bgc.count))]-bgc.count.home, MARGIN=2,unlist(bgc.count[1,match(names(bgc.count.home), names(bgc.count))]), '/' )
    zone.expansion <- sweep(zone.count[,match(names(zone.count.home), names(zone.count))]-zone.count.home, MARGIN=2,unlist(zone.count[1,match(names(zone.count.home), names(zone.count))]), '/' )

    ## simplify and structure the persistence and expansion tables
    totalarea <- sum(bgc.count[1,], na.rm = T) #historical distribution
    small <- which(bgc.count[1,]/totalarea < 0.01) # establish insignificant species for removal
    small <- c(small, which(is.na(bgc.count[1,]))) # establish insignificant species for removal
    bgc.persistence <- bgc.persistence[,-small] #remove small units and assign to permanent table
    bgc.expansion <- bgc.expansion[,-small] #remove small units and assign to permanent table


## SPECIES FEASIBILITIES
SiteLookup <- read.csv("data/SiteLookup.csv", stringsAsFactors = F)
SuitLookup <- read.csv("data/SuitLookup.csv", stringsAsFactors = F)
spps.lookup <- read.csv("data/Tree speciesand codes_2.0_25Aug2021.csv")

spps.all <- vector()
spps.native <- vector()
edatope="C4"
for(edatope in edatopes){

  #fractional feasibility table
  suit.area <- read.csv(paste(indir, paste("PredSum.suit", edatope, "csv", sep="."), sep=""))
  suit.area <- suit.area[,-c(1:5)] # matches the "identity" table

  #species table
  spp.area <- read.csv(paste(indir, paste("PredSum.spp", edatope, "csv", sep="."), sep=""))
  spp.area <- spp.area[,-c(1:5)] # matches the "identity" table

  #fractional feasibility table for home range
  suit.area.home <- read.csv(paste(indir, paste("PredSum.suit.home", edatope, "csv", sep="."), sep=""))
  suit.area.home <- suit.area.home[,-c(1:5)] # matches the "identity" table

  #species table for home range
  spp.area.home <- read.csv(paste(indir, paste("PredSum.spp.home", edatope, "csv", sep="."), sep=""))
  spp.area.home <- spp.area.home[,-c(1:5)] # matches the "identity" table

  ## spp color scheme
  colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
  colors = colors[-grep("yellow", colors)]
  set.seed(5)
  sppcolors <- c(brewer.pal(n=12, "Paired")[-11],sample(colors,dim(spp.area)[1]-11)) # removal of "11" is light yellow, doesn't show up well.

  ## calculate persistence and expansion tables
  suit.persistence <- sweep(suit.area.home, MARGIN=2,unlist(suit.area[1,]), '/' )
  spp.persistence <- sweep(spp.area.home, MARGIN=2,unlist(spp.area[1,]), '/' )
  suit.expansion <- sweep(suit.area-suit.area.home, MARGIN=2,unlist(suit.area[1,]), '/' )
  spp.expansion <- sweep(spp.area-spp.area.home, MARGIN=2,unlist(spp.area[1,]), '/' )

  ## simplify and structure the area tables
  totalarea <- sum(suit.area[1,]) #historical distribution
  small <- which(apply(suit.area, 2, sum, na.rm=T)/totalarea < 0.5) # establish insignificant species for removal
  assign(paste("suit.area", edatope, sep="."), suit.area[,-small]) #remove small units and assign to permanent table
  assign(paste("spp.area", edatope, sep="."), spp.area[,-small]) #remove small units and assign to permanent table
  assign(paste("spps.all", edatope, sep="."), names(spp.area)[-small])

  ## simplify and structure the persistence and expansion tables
  totalarea <- sum(spp.area[1,]) #historical distribution
  exotic <- which(spp.area[1,]/totalarea < 0.01) # establish insignificant species for removal
  assign(paste("suit.persistence", edatope, sep="."), suit.persistence[,-exotic]) #remove exotic units and assign to permanent table
  assign(paste("spp.persistence", edatope, sep="."), spp.persistence[,-exotic]) #remove exotic units and assign to permanent table
  assign(paste("suit.expansion", edatope, sep="."), suit.expansion[,-exotic]) #remove exotic units and assign to permanent table
  assign(paste("spp.expansion", edatope, sep="."), spp.expansion[,-exotic]) #remove exotic units and assign to permanent table
  assign(paste("spps.native", edatope, sep="."), names(spp.area)[-exotic])

  spps.all <- c(spps.all, names(spp.area)[-small])
  spps.native <- c(spps.native, names(spp.area)[-exotic])

}

spps.all <- unique(spps.all)[order(unique(spps.all))]
spps.native <- unique(spps.native)[order(unique(spps.native))]

## Color Schemes for species change maps
breakpoints.suit <-   breakseq <- c(0.5,1.5,2.5,3.5)
palette.suit <-   c("#006400", "#1E90FF", "#EEC900")
ColScheme.suit <- colorBin(palette.suit, bins=breakpoints.suit, na.color = NA)
breakpoints.change <- seq(-3,3,0.5)
palette.change <- c("black", brewer.pal(11,"RdBu")[c(1,2,3,4)], "grey90", "grey90", brewer.pal(11,"RdBu")[c(7,8,9,10,11)]);
palette.change2 <- c(brewer.pal(11,"RdBu")[c(1,2,3,4,4)], "grey90", colorRampPalette(c("white", "khaki1", "gold"))(6));
palette.change3 <- "black"
ColScheme.change <- colorBin(palette.change, bins=breakpoints.change, na.color = NA)
ColScheme.change2 <- colorBin(palette.change3, bins=breakpoints.change, na.color = NA)
ColScheme.change3 <- colorBin(palette.change3, bins=breakpoints.change, na.color = NA)
labels.change <- breakpoints.change[-median(1:length(breakpoints.change))]
breakpoints.binary <- seq(-1,1,0.2)
palette.binary <- c(brewer.pal(11,"RdBu")[c(1:4,6)], brewer.pal(11,"RdBu")[c(6,8:11)])
ColScheme.binary <- colorBin(palette.binary, bins=breakpoints.binary, na.color = NA)
labels.binary <- paste(abs(seq(-.9,.9,0.2))*100, "%", sep="")

## SPATIAL DATA
bgc.simple <- st_read("data/bgc.simple.shp")
bgc.maprecord <- as.character(bgc.simple$BGC)
zone.maprecord <- bgc.maprecord
for(i in zonecolors$classification){ zone.maprecord[grep(i,bgc.maprecord)] <- i }
bgc.list <- sort(unique(bgc.maprecord))
zone.list <- sort(unique(zone.maprecord))

# Define UI ----
ui <- fluidPage(
  navbarPage(title = "Climate change summary", theme = "bcgov.css",
             tabPanel("App",
                      fluidRow(
                        column(2,
                               helpText("Use this app to explore projected changes in climate climate conditions, expressed as shifts in biogeoclimatic units and climatic suitabilities for tree species"),

                               tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),

                               radioButtons("type", inline = FALSE,
                                            label = "Choose the type of map",
                                            # choices = list("Climate variables" = 1, "Biogeoclimatic units" = 2),
                                            choices = list("Climate variables" = 1, "Biogeoclimatic units" = 2, "Species feasibility" = 3),
                                            selected = 2),

                               sliderInput("transparency", label = "Layer transparency", min = 0,
                                           max = 1, value = 0.7),

                               radioButtons("periodtype",
                                            label = "Choose a time period",
                                            choices = list("Reference (1961-1990)" = 1, "Observed (2001-2020)" = 2, "Future (GCMs)" = 3),
                                            selected = 1),

                               conditionalPanel(
                                 condition = "input.zonelevel == true",
                                 h4("Reference biogeoclimatic zone map"),
                                 img(src = paste("refmap",studyarea, "zones.png", sep="."), height = 669*0.45, width = 661*0.45)
                               ),

                               conditionalPanel(
                                 condition = "input.zonelevel == false",
                                 h4("Reference biogeoclimatic variant map"),
                                 img(src = paste("refmap",studyarea, "variants.png", sep="."), height = 669*0.45, width = 661*0.45)
                               )
                        ),

                        column(5,

                               leafletOutput(outputId = "map", height="86vh"),
                               conditionalPanel(
                                 condition = "input.type != 0", # keeping the option of setting a condition, but with 0 there is no condition applied.
                                 downloadButton(outputId = "downloadMap", label = "Download map"),
                                 downloadButton("downloadRaster", "Download raster"),
                                 downloadButton("downloadLevels", "Download raster levels")
                               )

                        ),

                        column(5,
                               column(6,

                                      selectInput("var1",
                                                  label = "Choose the x-axis variable",
                                                  choices = as.list(variables.select),
                                                  selected = "MAT"),

                                      selectInput("sim.focal",
                                                  label = "Choose a GCM simulation",
                                                  choices = as.list(sims),
                                                  selected = "Ensemble mean"),

                                      radioButtons("period", inline = TRUE,
                                                   label = "Choose a time period for GCM results",
                                                   choices = list("2001-2020" = 1, "2021-2040" = 2, "2041-2060" = 3, "2061-2080" = 4, "2081-2100" = 5),
                                                   # choices = list(period.names[1] = 1, period.names[2] = 2, period.names[3] = 3, period.names[4] = 4, period.names[5] = 5),
                                                   selected = 3),

                                      checkboxInput("recent", label = "Show recent observed climate (2001-2020)", value = T),

                               ),
                               column(6,

                                      conditionalPanel(
                                        condition = "input.type == 1",

                                        selectInput("var2",
                                                    label = "Choose the y-axis variable",
                                                    choices = as.list(variables.select),
                                                    selected = "MAP"),

                                        checkboxInput("ratioscale", label = "Relative (%) scale for ratio variables", value = T),

                                      ),

                                      conditionalPanel(
                                        condition = "input.type == 2",

                                        checkboxInput("zonelevel", label = "Generalize to BGC zone level", value = T),

                                        radioButtons("plotbgc", inline = TRUE,
                                                     label = "Choose a plot type",
                                                     choices = list("Area" = 1, "Persistence" = 2),
                                                     selected = 1),

                                        conditionalPanel(
                                          condition = "input.plotbgc == 1",

                                          conditionalPanel(
                                            condition = "input.zonelevel == true",

                                            radioButtons("zone.area.focal", inline = TRUE,
                                                         label = "Select BGC zone for ensemble detail",
                                                         choices = as.list(c("none", zones)),
                                                         selected = "none"),

                                          ),

                                          conditionalPanel(
                                            condition = "input.zonelevel == false",

                                            selectInput("bgc.area.focal",
                                                        label = "Select BGC subzone-variant for ensemble detail",
                                                        choices = as.list(c("none", bgcs)),
                                                        selected = "none"),

                                          ),

                                        ),

                                        conditionalPanel(
                                          condition = "input.plotbgc == 2",

                                          conditionalPanel(
                                            condition = "input.zonelevel == true",

                                            radioButtons("zone.persistence.focal", inline = TRUE,
                                                         label = "Select BGC zone for ensemble detail",
                                                         choices = as.list(c("none", zones.native)),
                                                         selected = "none"),

                                          ),

                                          conditionalPanel(
                                            condition = "input.zonelevel == false",

                                            selectInput("bgc.persistence.focal",
                                                        label = "Select BGC subzone-variant for ensemble detail",
                                                        choices = as.list(c("none", bgcs.native)),
                                                        selected = "none"),

                                          ),

                                        ),

                                      ),

                                      conditionalPanel(
                                        condition = "input.type == 3",

                                        radioButtons("mapspp", inline = TRUE,
                                                     label = "Choose a map type",
                                                     choices = list("Feasibility" = 1, "Change" = 2, "Loss/gain" = 3),
                                                     selected = 1),

                                        radioButtons("plotspp", inline = TRUE,
                                                     label = "Choose a plot type",
                                                     choices = list("Area" = 1, "Persistence" = 2),
                                                     selected = 1),

                                        checkboxInput("fractional", label = "Use fractional (partial) feasibilities", value = F),

                                        radioButtons("edatope", inline = TRUE,
                                                     label = "Select an edatope (site type)",
                                                     choices = as.list(edatopes),
                                                     selected = edatopes[2]),

                                        conditionalPanel(
                                          condition = "input.plotspp == 1",

                                          conditionalPanel(
                                            condition = "input.edatope == 'B2'",

                                            radioButtons("spp.focal.1.B2", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.B2)),
                                                         selected = "none")
                                          ),

                                          conditionalPanel(
                                            condition = "input.edatope == 'C4'",

                                            radioButtons("spp.focal.1.C4", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.C4)),
                                                         selected = "none")
                                          ),

                                          conditionalPanel(
                                            condition = "input.edatope == 'D6'",

                                            radioButtons("spp.focal.1.D6", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.D6)),
                                                         selected = "none")
                                          ),

                                        ),

                                        conditionalPanel(
                                          condition = "input.plotspp == 2",

                                          conditionalPanel(
                                            condition = "input.edatope == 'B2'",

                                            radioButtons("spp.focal.2.B2", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.B2)),
                                                         selected = "none")
                                          ),

                                          conditionalPanel(
                                            condition = "input.edatope == 'C4'",

                                            radioButtons("spp.focal.2.C4", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.C4)),
                                                         selected = "none")
                                          ),

                                          conditionalPanel(
                                            condition = "input.edatope == 'D6'",

                                            radioButtons("spp.focal.2.D6", inline = TRUE,
                                                         label = "Select a species for ensemble detail",
                                                         choices = as.list(c("none", spps.all.D6)),
                                                         selected = "none")
                                          ),
                                        ),

                                      ),



                               ),

                               column(12,
                                      plotOutput(outputId = "scatterPlot", height="auto"),
                                      downloadButton(outputId = "downloadPlot", label = "Download plot")
                               ),
                        )
                      ),


                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;position: absolute; bottom:0%; ",

                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("About",

                      includeMarkdown("about.Rmd"),

                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",

                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),

             tabPanel("Find-a-BEC",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Choose a BGC zone or subzone-variant to display on the map"),

                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),

                          selectInput("showbgc",
                                      label = "Choose a BGC subzone-variant",
                                      choices = as.list(c("none", bgc.list)),
                                      selected = "none"),

                          selectInput("showzone",
                                      label = "Choose a BGC zone",
                                      choices = as.list(c("none", zone.list)),
                                      selected = "none"),

                        ),

                        mainPanel(

                          leafletOutput(outputId = "becmap", height="86vh")

                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",

                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),

             tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",

                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             )
  )
)

# Define server logic ----
server <- function(input, output, session) {

    output$map <- renderLeaflet({


    leaflet() %>%
      addTiles() %>%
      # addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      # addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4]) %>%
      # addLayersControl(
      #   baseGroups = c("Base map", "Terrain only", "Satellite view"),
      #   options = layersControlOptions(collapsed = FALSE),
      # ) %>%
      addPolygons(data=bdy, fillColor = NA, color="black", smoothFactor = 0.2, fillOpacity = 0, weight=2)

  }
  )

  observe({

    # period <- periods[length(periods)]
    # zonelevel=F
    # transparency <- 0.7
    # sim.focal <- sims[1]

    zonelevel <- if(input$zonelevel==T) T else F
    sim.focal <- input$sim.focal
    scenario <- scenarios[2]
    period <-  periods[as.numeric(input$period)+1]
    transparency <- input$transparency

    if(input$periodtype==1) X <- bgc.pred.ref
    if(input$periodtype==2) X <- bgc.pred.2001
    if(input$periodtype==3) X <- raster(paste(indir,paste("BGC.pred", id.kkz$GCM[which(sims==sim.focal)], id.kkz$RUN[which(sims==sim.focal)], scenario, period,"tif", sep="."), sep=""))
    BGC.pred <- levels.bgc[values(X)]

    zone.pred <- rep(NA, length(BGC.pred))
    for(i in zones.all){ zone.pred[grep(i,BGC.pred)] <- i }

    ColScheme <- if(zonelevel==T) zonecolors$colour[match(zones.all, zonecolors$classification)] else bgccolors$colour[match(bgcs.all, bgccolors$classification)]
    units <- if(zonelevel==T) zones.all else bgcs.all
    pred <- if(zonelevel==T) zone.pred else BGC.pred

    values(X) <- factor(pred, levels=units)
    values(X)[1:length(units)] <- 1:length(units) # this is a patch that is necessary to get the color scheme right.

    if(input$type==2){

      leafletProxy("map") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
        addRasterImage(X, colors = ColScheme, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
        addPolygons(data=bdy, fillColor = NA, color="black", smoothFactor = 0.2, fillOpacity = 0, weight=2)

    }
    if(input$type==3){

      X <- bgc.pred.ref
      values(X) <- NA

      edatope <- input$edatope
      if(input$plotspp==1){
        if(edatope=="B2") spp.focal <- input$spp.focal.1.B2
        if(edatope=="C4") spp.focal <- input$spp.focal.1.C4
        if(edatope=="D6") spp.focal <- input$spp.focal.1.D6
      } else if(input$plotspp==2){
        if(edatope=="B2") spp.focal <- input$spp.focal.2.B2
        if(edatope=="C4") spp.focal <- input$spp.focal.2.C4
        if(edatope=="D6") spp.focal <- input$spp.focal.2.D6
      }

      # spp.focal <- get(paste("input$spp.focal", input$plotspp, edatope, sep="."))

      if(input$mapspp==1){
        values(X) <- NA
        if(spp.focal!="none") {
          spp.simple <- substring(spp.focal, 1, 2)
          suit <- SuitLookup$ESuit[which(SuitLookup$Spp==spp.simple)][match(as.vector(unlist(SiteLookup[which(names(SiteLookup)==edatope)])), SuitLookup$SS_NoSpace[which(SuitLookup$Spp==spp.simple)])]
          if(input$periodtype==3){
            if(sim.focal=="Ensemble mean"){
              suit.ref <- suit[match(levels.bgc[values(bgc.pred.ref)], SiteLookup$BGC)]
              suit.ref[is.na(suit.ref)] <- 4 #set non-suitable to 4
              change.proj <- values(raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep="")))
              temp <- suit.ref-change.proj
              temp[which(temp>3.5)] <- NA #set non-suitable to NA
              temp[which(temp<1)] <- 1 #set non-suitable to NA
            } else {
              temp <- suit[match(BGC.pred, SiteLookup$BGC)]
              temp[which(temp>3.5)] <- NA #set non-suitable to NA
            }
          } else {
            temp <- suit[match(BGC.pred, SiteLookup$BGC)]
            temp[which(temp>3.5)] <- NA #set non-suitable to NA
          }
          values(X) <- temp
        }
        leafletProxy("map") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
          addRasterImage(X, colors =  ColScheme.suit, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
          addPolygons(data=bdy, fillColor = NA, color="black", smoothFactor = 0.2, fillOpacity = 0, weight=2)
      }
      if(input$mapspp==2){
        values(X) <- NA
        if(input$periodtype==3){
          if(spp.focal!="none"){
            suit.ref <- suit[match(levels.bgc[values(bgc.pred.ref)], SiteLookup$BGC)]
            suit.ref[is.na(suit.ref)] <- 4 #set non-suitable to 4
            X <- raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
            suit.proj <- suit.ref-values(X)
            # X2 <- raster::setValues(X,NA)
            # X2[suit.ref==4] <- X[suit.ref==4]
            # X2[X2<0] <- NA
            # X3 <- raster::setValues(X,NA)
            # X3[which(suit.ref<4 & suit.proj>3.5)] <- 1
          }
          leafletProxy("map") %>%
            addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
            addRasterImage(X, colors =  ColScheme.change, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
            # addRasterImage(X2, colors =  ColScheme.change2, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
            # addRasterImage(X3, colors =  ColScheme.change3, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
            addPolygons(data=bdy, fillColor = NA, color="black", smoothFactor = 0.2, fillOpacity = 0, weight=2)
        }
              }
      if(input$mapspp==3){
        values(X) <- NA
        if(input$periodtype==3){
          if(spp.focal!="none") X <- raster(paste(indir,paste("Spp.binary", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
          leafletProxy("map") %>%
            addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
            addRasterImage(X, colors =  ColScheme.binary, method="ngb", opacity = transparency, maxBytes = 6 * 1024 * 1024)%>%
            addPolygons(data=bdy, fillColor = NA, color="black", smoothFactor = 0.2, fillOpacity = 0, weight=2)
        } # end if(input$periodtype==3)
      }
    }

  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map")

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$type==3) {
      if(input$mapspp==1) proxy %>% addLegend(colors =  palette.suit, labels=c("1 (primary)", "2 (secondary)", "3 (tertiary)"), title = "Climatic feasibility")
      if(input$mapspp==2) proxy %>% addLegend(colors = palette.change, labels = labels.change, title = "Ensemble-mean change</br>in climatic feasibility")
      if(input$mapspp==3) proxy %>% addLegend(colors = palette.binary, labels = labels.binary, title = "% of ensemble predicting</br>loss (red) or gain (blue)</br>of climatic feasibility")
    }
  })

  #Show popup on click
  observeEvent(input$map_click, {

    # for testing
    # edatope="C4"
    # spp.focal="Fdc"

    sim.focal <- input$sim.focal
    scenario <- scenarios[2]
    period <-  periods[as.numeric(input$period)+1]

    if(input$periodtype==1) X <- bgc.pred.ref
    if(input$periodtype==2) X <- bgc.pred.2001
    if(input$periodtype==3) X <- raster(paste(indir,paste("BGC.pred", id.kkz$GCM[which(sims==sim.focal)], id.kkz$RUN[which(sims==sim.focal)], scenario, period,"tif", sep="."), sep=""))
    BGC.pred <- levels.bgc[values(X)]

    click <- input$map_click
    bgc.popup <- BGC.pred[cellFromXY(X, matrix(c(click$lng, click$lat), 1))]
    text<-paste0("<strong>", bgc.popup, "</strong>", "<br/>Zone: ", bgc.names$ZoneName[which(bgc.names$Map_Label==bgc.popup)], "<br/>Subzone/Variant: ",  bgc.names$SubzoneName[which(bgc.names$Map_Label==bgc.popup)])
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })

  # Map download

  spp.focal <- reactive({
    if(input$plotspp==1){
      if(input$edatope=="B2") spp.focal <- input$spp.focal.1.B2
      if(input$edatope=="C4") spp.focal <- input$spp.focal.1.C4
      if(input$edatope=="D6") spp.focal <- input$spp.focal.1.D6
    } else if(input$plotspp==2){
      if(input$edatope=="B2") spp.focal <- input$spp.focal.2.B2
      if(input$edatope=="C4") spp.focal <- input$spp.focal.2.C4
      if(input$edatope=="D6") spp.focal <- input$spp.focal.2.D6
    }
    return(spp.focal)
  })

  output$downloadMap <- downloadHandler(
    filename =  function(){
      if(input$type!=3){
        paste(studyarea,
              "map",
              if(input$zonelevel==T) "zones" else "bgcs",
              if(input$periodtype==1) "ref" else if(input$periodtype==2) "obs" else paste("proj",input$sim.focal,periods[as.numeric(input$period)+1],sep="."),
              "png", sep=".")
      } else {
        paste(studyarea,
              "map",
              if(input$periodtype==1) "ref" else if(input$periodtype==2) "obs" else paste("proj",input$sim.focal,periods[as.numeric(input$period)+1],sep="."),
              if(input$mapspp==1) "Feasibility" else if(input$mapspp==2) "ChangeFeas" else "LossGain",
              spp.focal(),
              input$edatope,
              "png", sep=".")
      }
    },

    content = function(file) {

      zonelevel <- if(input$zonelevel==T) T else F
      sim.focal <- input$sim.focal
      scenario <- scenarios[2]
      period <-  periods[as.numeric(input$period)+1]
      transparency <- input$transparency

      if(input$type!=3){

        if(input$periodtype==1) X <- bgc.pred.ref
        if(input$periodtype==2) X <- bgc.pred.2001
        if(input$periodtype==3) X <- raster(paste(indir,paste("BGC.pred", id.kkz$GCM[which(sims==sim.focal)], id.kkz$RUN[which(sims==sim.focal)], scenario, period,"tif", sep="."), sep=""))
        BGC.pred <- levels.bgc[values(X)]

        zone.pred <- rep(NA, length(BGC.pred))
        for(i in zones.all){ zone.pred[grep(i,BGC.pred)] <- i }

        ColScheme <- if(zonelevel==T) zonecolors$colour[match(zones.all, zonecolors$classification)] else bgccolors$colour[match(bgcs.all, bgccolors$classification)]
        units <- if(zonelevel==T) zones.all else bgcs.all
        pred <- if(zonelevel==T) zone.pred else BGC.pred

        values(X) <- NA
        values(X) <- factor(pred, levels=units)
        values(X)[1:length(units)] <- 1:length(units) # this is a patch that is necessary to get the color scheme right.

        png(file, width = 5*300, height=if(studyarea=="CDFCP") 3.5*300 else 4.5*300, res = 300)
        par(mar=c(0,0,0,0))

        plot(X, xaxt="n", yaxt="n", col=alpha(ColScheme, 1), legend=FALSE, legend.mar=0, maxpixels=ncell(X), bty="n", box=FALSE)
        values(X)[-(1:length(units))] <- NA # cover up the color bar
        image(X, add=T, col="white") # cover up the color bar
        plot(bdy, add=T, lwd=1)

        totalarea <- sum(bgc.count[1,])
        temp <- table(pred)/totalarea
        temp <- rev(sort(temp))
        temp <- temp[which(temp > 0.005)]
        legendunits <- names(temp)

        if(zonelevel==T) legend("topright", legend=paste(legendunits, " (", round(temp*100, 0), "%)", sep=""), fill=zonecolors$colour[match(legendunits, zonecolors$classification)], ncol= if(length(legendunits)<12) 1 else if(length(legendunits)<23) 2 else 3, bty="n", cex=0.7)
        if(zonelevel==F) legend("topright", legend=paste(legendunits, " (", round(temp*100, 0), "%)", sep=""), fill=bgccolors$colour[match(legendunits, bgccolors$classification)], ncol= if(length(legendunits)<12) 1 else if(length(legendunits)<23) 2 else 3, bty="n", cex=0.5)

        # box()
        dev.off()

      } else {

        X <- bgc.pred.ref
        values(X) <- NA

        edatope <- input$edatope
        if(input$plotspp==1){
          if(edatope=="B2") spp.focal <- input$spp.focal.1.B2
          if(edatope=="C4") spp.focal <- input$spp.focal.1.C4
          if(edatope=="D6") spp.focal <- input$spp.focal.1.D6
        } else if(input$plotspp==2){
          if(edatope=="B2") spp.focal <- input$spp.focal.2.B2
          if(edatope=="C4") spp.focal <- input$spp.focal.2.C4
          if(edatope=="D6") spp.focal <- input$spp.focal.2.D6
        }
        if(input$periodtype==1) X <- bgc.pred.ref
        if(input$periodtype==2) X <- bgc.pred.2001
        if(input$periodtype==3) X <- raster(paste(indir,paste("BGC.pred", id.kkz$GCM[which(sims==sim.focal)], id.kkz$RUN[which(sims==sim.focal)], scenario, period,"tif", sep="."), sep=""))
        BGC.pred <- levels.bgc[values(X)]

        ## calculate projected suitability
        values(X) <- NA
        if(spp.focal!="none") {
          spp.simple <- substring(spp.focal, 1, 2)
          suit <- SuitLookup$ESuit[which(SuitLookup$Spp==spp.simple)][match(as.vector(unlist(SiteLookup[which(names(SiteLookup)==edatope)])), SuitLookup$SS_NoSpace[which(SuitLookup$Spp==spp.simple)])]
          if(input$periodtype==3){
            if(sim.focal=="Ensemble mean"){
              suit.ref <- suit[match(levels.bgc[values(bgc.pred.ref)], SiteLookup$BGC)]
              suit.ref[is.na(suit.ref)] <- 4 #set non-suitable to 4
              change.proj <- values(raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep="")))
              temp <- suit.ref-change.proj
              temp[which(temp>3.5)] <- NA #set non-suitable to NA
              temp[which(temp<1)] <- 1 #set non-suitable to NA
            } else {
              temp <- suit[match(BGC.pred, SiteLookup$BGC)]
              temp[which(temp>3.5)] <- NA #set non-suitable to NA
            }
          } else {
            temp <- suit[match(BGC.pred, SiteLookup$BGC)]
            temp[which(temp>3.5)] <- NA #set non-suitable to NA
          }
          suit.proj <- temp
        }

        png(file=file, type="cairo", units="in", width=5, height=if(studyarea=="CDFCP") 3.5 else 4.5, pointsize=12, res=300)

        par(plt=c(0,1,0,1), bg="white")
        plot(0, col="white", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
        Common <- as.character(spps.lookup$EnglishName[which(spps.lookup$TreeCode==spp.focal)])
        Latin <- as.character(spps.lookup$ScientificName[which(spps.lookup$TreeCode==spp.focal)])
        mtext(if(spp.focal%in%spps.lookup$TreeCode) bquote(bold(.(spp.focal))~"-"~.(Common)) else bquote(bold(.(spp.focal))),
              side=3, line=-1.5, adj=0.01, cex=0.9, font=2)
        mtext(paste("Site type: ", edatope, " (", edatope.names[which(edatopes==edatope)], ")", sep=""), side=3, line=-2.5, adj=0.01, cex=0.8, font=1)
        mtext(paste("Time period: ", period.names[which(periods==period)-1], sep=""), side=3, line=-3.5, adj=0.01, cex=0.8, font=1)

        ##=================================
        ###historic suitability

        values(X) <- suit.ref
        breakseq <- c(0.5,1.5,2.5,3.5,5)
        ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")

        par(plt = c(0.01, 0.4, 0.005, 0.5),new = TRUE, xpd = TRUE)

        image(X,xlab = NA,ylab = NA,bty = "n",  xaxt="n", yaxt="n",
              col=ColScheme, breaks=breakseq, maxpixels= ncell(X))
        plot(bdy, add=T, border="black",col = NA, lwd=0.4)
        legend("topleft", legend=c("1 (primary)", "2 (secondary)", "3 (tertiary)"),
               fill=ColScheme, bty="n", cex=0.8, title="Historical feasibility", inset=c(0,-0.3))

        ##=================================
        ##mean feasibility change
        if(input$mapspp==1){
          if(spp.focal!="none") values(X) <- suit.proj
          breakpoints <- c(0.5,1.5,2.5,3.5,5)
          labels <- c("1 (primary)", "2 (secondary)", "3 (tertiary)")
          ColScheme <- c("darkgreen", "dodgerblue1", "gold2", "white")
          legendTitle <- "Projected feasibility"
        } else if(input$mapspp==2){
          if(spp.focal!="none") X <- raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
          breakpoints <- seq(-3,3,0.5); length(breakpoints)
          labels <- c("-3","-2", "-1", "no change", "+1","+2","+3")
          ColScheme <- c("black", brewer.pal(11,"RdBu")[c(1,2,3,4)], "grey90", "grey90", brewer.pal(11,"RdBu")[c(7,8,9,10,11)]);
          legendTitle <- "Mean change\nin feasibility"
        } else if(input$mapspp==3){
          if(spp.focal!="none") X <- raster(paste(indir,paste("Spp.binary", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
          breakpoints <- c(seq(-1, -.50,.10), seq(.60, 1.00,.10));length(breakpoints)
          labels <- c("Loss", "Gain")
          ColScheme <- c(brewer.pal(11,"RdBu")[c(1:4)], "grey90", "grey90", brewer.pal(11,"RdBu")[c(8:11)]); length(ColScheme)
          legendTitle <- "% of GCMs projecting\nloss or gain of\nclimatic feasibility"
        }

        #Map
        par(plt = c(0.3, 0.99, 0.01, 0.9), xpd = TRUE, new = TRUE)
        image(X,xlab = NA,ylab = NA,bty = "n", xaxt="n", yaxt="n", col=ColScheme, breaks=breakpoints, maxpixels= ncell(X))
        plot(bdy, add=T, border="black",col = NA, lwd=0.4)

        # Legend
        par(plt = c(0.75, 0.999, 0.6, 1.0), xpd = TRUE, new = TRUE)
        plot(0, col="white", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), bty="n")
        xl <- 0.2; yb <- 0.1; xr <- 0.4; yt <- 0.9; xadj <- 0.01
        y.int <- (yt-yb)/length(ColScheme)
        if(input$mapspp==1){
          legend("left", legend=c("1 (primary)", "2 (secondary)", "3 (tertiary)"),
               fill=ColScheme, bty="n", cex=0.8, title="Projected feasibility")
        } else if(input$mapspp==2){
          rect(xl+xadj,  head(seq(yb,yt,y.int),-1),  xr,  tail(seq(yb,yt,y.int),-1),  col=ColScheme)
          text(rep(xr-.0050000,length(labels)),seq(yb,yt,(yt-yb)/(length(labels)-1)),labels,pos=4,cex=0.8,font=1)
          text(xl-.0150000, mean(c(yb,yt))-.050000, legendTitle, srt=90, pos=3, cex=0.85, font=2)
        } else if(input$mapspp==3){
          rect(xl+xadj,  head(seq(yb,yt,y.int),-1),  xr,  tail(seq(yb,yt,y.int),-1),  col=ColScheme)
          text(rep(xr+.0250000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(3,10)],labels,pos=4,cex=0.8,font=0.8, srt=90)
          text(rep(xr-.0050000,length(labels)),seq(yb,yt,(yt-yb)/(15-1))[c(1,8,15)],c("100%", "50%", "100%"),pos=4,cex=0.8,font=1)
          text(xl-.0250000, mean(c(yb,yt))-.050000, legendTitle, srt=90, pos=3, cex=0.85, font=2)
        }
        par(xpd=F)

        dev.off()
      }
    }
  )

  # Raster download

  output$downloadRaster <- downloadHandler(
    filename =  function(){
      if(input$type!=3){
        paste(studyarea,
              "raster",
              if(input$periodtype==1) "ref" else if(input$periodtype==2) "obs" else paste("proj",input$sim.focal,periods[as.numeric(input$period)+1],sep="."),
              "tif", sep=".")
      } else {
        paste(studyarea,
              "raster",
              if(input$periodtype==1) "ref" else if(input$periodtype==2) "obs" else paste("proj",input$sim.focal,periods[as.numeric(input$period)+1],sep="."),
              if(input$mapspp==1) "Feasibility" else if(input$mapspp==2) "ChangeFeas" else "LossGain",
              spp.focal(),
              input$edatope,
              "tif", sep=".")
      }
    },

    content = function(file) {

      if(input$type!=3){
        sim.focal <- input$sim.focal
        scenario <- scenarios[2]
        period <-  periods[as.numeric(input$period)+1]

        if(input$periodtype==1) X <- bgc.pred.ref
        if(input$periodtype==2) X <- bgc.pred.2001
        if(input$periodtype==3) X <- raster(paste(indir,paste("BGC.pred", id.kkz$GCM[which(sims==sim.focal)], id.kkz$RUN[which(sims==sim.focal)], scenario, period,"tif", sep="."), sep=""))
        BGC.pred <- levels.bgc[values(X)]

        values(X) <- NA
        values(X) <- factor(BGC.pred, levels=bgcs.all)

        writeRaster(X, filename=file)

      } else {

        sim.focal <- input$sim.focal
        scenario <- scenarios[2]
        period <-  periods[as.numeric(input$period)+1]

        X <- bgc.pred.ref
        values(X) <- NA

        edatope <- input$edatope
        if(input$plotspp==1){
          if(edatope=="B2") spp.focal <- input$spp.focal.1.B2
          if(edatope=="C4") spp.focal <- input$spp.focal.1.C4
          if(edatope=="D6") spp.focal <- input$spp.focal.1.D6
        } else if(input$plotspp==2){
          if(edatope=="B2") spp.focal <- input$spp.focal.2.B2
          if(edatope=="C4") spp.focal <- input$spp.focal.2.C4
          if(edatope=="D6") spp.focal <- input$spp.focal.2.D6
        }

        if(input$mapspp==1){
          values(X) <- NA
          if(spp.focal!="none") {
            suit <- SuitLookup$ESuit[which(SuitLookup$Spp==spp.focal)][match(as.vector(unlist(SiteLookup[which(names(SiteLookup)==edatope)])), SuitLookup$SS_NoSpace[which(SuitLookup$Spp==spp.focal)])]
            if(input$periodtype==3){
              if(sim.focal=="Ensemble mean"){
                suit.ref <- suit[match(levels.bgc[values(bgc.pred.ref)], SiteLookup$BGC)]
                suit.ref[is.na(suit.ref)] <- 4 #set non-suitable to 4
                change.proj <- values(raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep="")))
                temp <- suit.ref-change.proj
                temp[which(temp>3.5)] <- NA #set non-suitable to NA
                temp[which(temp<1)] <- 1 #set non-suitable to NA
              } else {
                temp <- suit[match(BGC.pred, SiteLookup$BGC)]
                temp[which(temp>3.5)] <- NA #set non-suitable to NA
              }
            } else {
              temp <- suit[match(BGC.pred, SiteLookup$BGC)]
              temp[which(temp>3.5)] <- NA #set non-suitable to NA
            }
            values(X) <- temp
          }
        }
        if(input$mapspp==2){
          values(X) <- NA
          if(input$periodtype==3){
            if(spp.focal!="none") X <- raster(paste(indir,paste("Spp.Changesuit", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
          }
        }
        if(input$mapspp==3){
          values(X) <- NA
          if(input$periodtype==3){
            if(spp.focal!="none") X <- raster(paste(indir,paste("Spp.binary", spp.focal, edatope, scenario, period, "tif", sep="."), sep=""))
          } # end if(input$periodtype==3)
        }
        writeRaster(X, filename=file)
      }
    }
  )

  # Raster levels download
  output$downloadLevels <- downloadHandler(
    filename =  "levels.bgc.csv",

    content = function(file) {
      write.csv(levels.bgc, file, row.names=FALSE)
    }
  )

  ## Plot window (done as a function so that the user can export)
  scatterPlot <- function() {

    # period <- "2081_2100"
    # scenario <- scenarios[2]
    # var1 <- "MAT"
    # var2 <- "MAP"
    # ratioscale <- T
    # zonelevel=T
    # unit.area.focal <- if(zonelevel==T) zones[2] else bgcs[1]
    # unit.persistence.focal <- if(zonelevel==T) zones.native[2] else bgcs.native[1]
    # units.area <- if(zonelevel==T) zones else bgcs
    # fractional=F
    # edatope <- edatopes[1]
    # spp.focal <- "Fd"
    # recent <- T
    # sim.focal <- sims[1]

    period <-  periods[as.numeric(input$period)+1]
    scenario <- scenarios[as.numeric(input$scenario)+1]
    var1 <- input$var1
    var2 <- input$var2
    zonelevel <- if(input$zonelevel==T) T else F
    unit.area.focal <- if(zonelevel==T) input$zone.area.focal else input$bgc.area.focal
    unit.persistence.focal <- if(zonelevel==T) input$zone.persistence.focal else input$bgc.persistence.focal
    units.area <- if(zonelevel==T) zones else bgcs
    fractional <- if(input$fractional==T) T else F
    edatope <- input$edatope
    recent <- input$recent
    sim.focal <- input$sim.focal

    variable.type1 <- variable.types[which(variables==var1)]
    variable.type2 <- variable.types[which(variables==var2)]

    if(input$type==1) {

      #-------------------------
      # climate scatterplot
      #-------------------------

      ratioscale <- if(input$ratioscale==T) T else F

      data <- if(ratioscale==T & variable.type1=="ratio") clim.meanChange.ratio else if(ratioscale==T & variable.type2=="ratio") clim.meanChange.ratio else clim.meanChange

      x <- data[, which(variables==var1)]
      y <- data[, which(variables==var2)]

      xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
      ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)

      par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
      plot(x,y,col="white", tck=0, xaxt="n", yaxt="n", xlim=xlim, ylim=ylim, ylab="",
           xlab=paste("Change in", if(var1%in%variable.names$Code) variable.names$Variable[which(variable.names$Code==var1)] else var1))
      par(mgp=c(2.5,0.25, 0))
      title(ylab=paste("Change in", if(var2%in%variable.names$Code) variable.names$Variable[which(variable.names$Code==var2)] else var2))
      lines(c(0,0), c(-99,99), lty=2, col="gray")
      lines(c(-99,99), c(0,0), lty=2, col="gray")

      if(recent==T){
        x1 <- data[2, which(variables==var1)]
        y1 <- data[2, which(variables==var2)]
        points(x1,y1, pch=16, col="gray", cex=2.5)
        text(x1,y1, "2001-2020 (observed)", cex=1, font=2, pos=4, col="gray", offset=0.9)
      }

      # gcm=gcms[3]
      for(gcm in gcms){
        i=which(gcms==gcm)
        runs <- unique(identity$RUN[which(identity$GCM==gcm)])
        # run=runs[4]
        for(run in runs){
          is.focalSim <- paste(gcm, substr(run,1,2), sep="_")==sim.focal
          x2 <- data[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(variables==var1)]
          y2 <- data[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(variables==var2)]
          if(length(unique(sign(diff(x2))))==1){
            x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
            y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
            s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
            if(run=="ensembleMean") lines(s, col=ColScheme.gcms[i], lwd=2)
          } else if(run=="ensembleMean") lines(x2, y2, col=ColScheme.gcms[i], lwd=2)
          j=which(periods==period)
          if(length(runs)>2) points(x2[j],y2[j], pch=21, bg=if(is.focalSim==T) "black" else ColScheme.gcms[i], cex=1)
          if(run=="ensembleMean") points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=if(gcm=="ensembleMean") 3.5 else 3)
          if(run=="ensembleMean") text(x2[j],y2[j], mods[i], cex=if(gcm=="ensembleMean") 0.7 else 0.5, font=2)
        }
      }
      axis(1, at=pretty(x), labels=if(ratioscale==T & variable.type1=="ratio") paste(pretty(x)*100, "%", sep="") else pretty(x), tck=0)
      axis(2, at=pretty(y), labels=if(ratioscale==T & variable.type2=="ratio") paste(pretty(y)*100, "%", sep="") else pretty(y), las=2, tck=0)

    } else if(input$type==2){

      if(input$plotbgc==1){
        
        #-------------------------
        # BGC scatterplot
        #-------------------------

        data <- if(zonelevel==T) zone.area else bgc.area
        clim.data <- clim.meanChange
        ColScheme <- if(zonelevel==T) zonecolors else bgccolors
        units <- if(zonelevel==T) zones else bgcs

        x <- clim.data[, which(variables==var1)]
        variable.type1 <- variable.types[which(variables==var1)]

        xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)-c(diff(range(x))/2.9, 0)
        ylim=c(0, max(data, na.rm=T)*1.05)

        par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
        plot(0,col="white", tck=0, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, ylab="",
             xlab=paste("Change in", variable.names$Variable[which(variable.names$Code==var1)]),
        )
        par(mgp=c(2.5,0.25, 0))
        title(ylab=paste("Area of biogeoclimatic unit ('000 sq.km)"))

        axis(1, at=pretty(x), labels=pretty(x), tck=0)
        axis(2, at=pretty(ylim), labels=pretty(ylim)/1000, tck=0, las=2)

        lines(rep(x[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==period)], 2), ylim, col="gray90", lwd=2)

        # establish whether to plot the BGC label on the right or left, based on whether the trajectory is increasing or decreasing. 
        increasing <- data[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==periods[length(periods)]),]>data[1,]
        order.increasing <- rev(order(unlist(as.vector(data[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==periods[length(periods)]),increasing==T]))))
        order.decreasing <- rev(order(unlist(as.vector(data[1,increasing==F]))))
        units.increasing <- units.area[increasing==T][order.increasing]
        units.decreasing <- units.area[increasing==F][order.decreasing]
        data.increasing <-  if(length(which(increasing==T))>1) data[,increasing==T][,order.increasing] else data[,increasing==T]
        data.decreasing <- if(length(which(increasing==F))>1) data[,increasing==F][,order.decreasing] else data[,increasing==F]
        units.sort <- c(units.increasing, units.decreasing)
        data.sort <- cbind(data.increasing, data.decreasing)
        increasing.sort <- increasing[match(units.sort, units)]
        
        # plot the ensemble mean lines
        for(unit in units.sort){
          i <- which(units.sort==unit)
          col.focal <- if(unit.area.focal=="none") ColScheme$colour[which(ColScheme$classification==unit)] else "lightgray"
          col.focal2 <- if(unit.area.focal=="none") "black" else "darkgray"
          x1 <- x[c(1, which(identity$GCM=="ensembleMean" & identity$SSP==scenario))]
          y1 <- data.sort[c(1, which(identity$GCM=="ensembleMean" & identity$SSP==scenario)), which(units.sort==unit)]
          y1[is.na(y1)] <- 0
          if(length(unique(sign(diff(x1))))==1){
            x3 <- if(unique(sign(diff(x1)))==-1) rev(x1) else x1
            y3 <- if(unique(sign(diff(x1)))==-1) rev(y1) else y1
            s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
            lines(s, col=col.focal, lwd=2)
          } else lines(x1, y1, col=col.focal, lwd=3)

          # labels
          position <- rep(0:2, times=100)
          side <- if(increasing.sort[i]==T) 4 else 2
          space <- 12
          lines(if(increasing.sort[i]==T) c(x1[length(x1)], x1[length(x1)]+position[i]*diff(range(x))/space) else c(x1[1], x1[1]-position[i]*diff(range(x))/space),
                if(increasing.sort[i]==T) rep(y1[length(y1)],2) else rep(y1[1],2), col=col.focal, lty=2)
          text(if(increasing.sort[i]==T) x1[length(x1)]+position[i]*diff(range(x))/space else x1[1]-position[i]*diff(range(x))/space,
               if(increasing.sort[i]==T) y1[length(y1)] else y1[1],
               unit, col=col.focal, pos=side, font=2, cex=0.7, offset=0.1)

          if(recent==T){
            x1 <- x[1:2]
            y1 <- data.sort[1:2, which(units.sort==unit)]
            lines(x1, y1, col=col.focal, lwd=1.25, lty=1)
            points(x1[2],y1[2], pch=21, bg=col.focal, col=col.focal2, cex=1.2)
          }

        }

        # plot the ensemble
        if(unit.area.focal!="none"){
          for(gcm in gcms){
            i=which(gcms==gcm)
            runs <- unique(identity$RUN[which(identity$GCM==gcm)])
            for(run in runs){
              is.focalSim <- paste(gcm, substr(run,1,2), sep="_")==sim.focal
              x2 <- x[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run))]
              y2 <- data[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(units==unit.area.focal)]
              if(length(unique(sign(diff(x2))))==1){
                x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
                y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
                s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
                if(run=="ensembleMean") lines(s, col=ColScheme.gcms[i], lwd=if(gcm=="ensembleMean") 4 else 2)
              } else if(run=="ensembleMean") lines(x2, y2, col=ColScheme.gcms[i], lwd=if(gcm=="ensembleMean") 4 else 2)
              j=which(periods==period)
              if(length(runs)>2) points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=1)
              if(run=="ensembleMean") points(x2[j],y2[j], pch=21, bg=if(is.focalSim==T) "black" else ColScheme.gcms[i], cex=if(gcm=="Ensemble mean") 3.5 else 3)
              if(run=="ensembleMean") text(x2[j],y2[j], mods[i], cex=if(gcm=="Ensemble mean") 0.7 else 0.5, font=2)
            }
          }
          
          if(recent==T){
            x1 <- x[1:2]
            y1 <- data.sort[1:2, which(units.sort==unit.area.focal)]
            lines(x1, y1, col="gray", lwd=1.25, lty=1)
            points(x1[2],y1[2], pch=21, bg="gray", col=1, cex=2)
            text(x1[2],y1[2], "2001-2020 (observed)", cex=1, font=2, pos=4, col="gray30", offset=0.7)
          }

        }
        box()

      } else if(input$plotbgc==2){

        #-------------------------
        # bgc bubbleplot
        #-------------------------

        persistence <- if(zonelevel==T) zone.persistence else bgc.persistence
        expansion <- if(zonelevel==T) zone.expansion else bgc.expansion
        ColScheme <- if(zonelevel==T) zonecolors else bgccolors
        # units <- names(persistence)
        units.native <- if(zonelevel==T) zones.native else bgcs.native
        units <- units.native

        par(mar=c(3,4,0.1,0.1), mgp=c(1.25, 0.25, 0), cex=1.5)

        xlim <- c(0, 1.1)
        ylim <- c(-5,3)
        plot(0, xlim=xlim, ylim=ylim, col="white", xaxt="n", yaxt="n", xlab="Persistence within historical range", ylab="")
        axis(1,at=seq(xlim[1], xlim[2], 0.2), labels=paste(seq(xlim[1], xlim[2], 0.2)*100,"%", sep=""), tck=0)
        axis(2,at=seq(ylim[1], ylim[2]), labels=paste(round(2^(seq(ylim[1], ylim[2]))*100),"%", sep=""), las=2, tck=0)
        par(mgp=c(2.75, 0.25, 0))
        title(ylab="Expansion beyond historical range", cex.lab=1)
        iso <- seq(0,1.2, 0.001)
        lines(1-iso, log2(iso), lty=2, lwd=2, col="darkgray")
        # arctext(x = "Growing feasible range", center = c(-1, -28.7), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
        # arctext(x = "Shrinking feasible range", center = c(-1, -29.3), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
        # mtext(paste(edatope.name[which(edatopes==edatope)], " sites", " (", edatope, ")", sep=""), side=3, line=-1.25, adj= if(edatope=="C4") 0.025 else 0.075, cex=0.7, font=1)

        unit=units[2]
        for(unit in units){
          col.focal <- if(unit.persistence.focal=="none") ColScheme$colour[which(ColScheme$classification==unit)] else "lightgray"
          col.focal2 <- if(unit.persistence.focal=="none") "black" else "darkgray"
          x <- persistence[which(identity$SSP==scenario & identity$PERIOD==period), which(names(persistence)==unit)]
          y <- expansion[which(identity$SSP==scenario & identity$PERIOD==period), which(names(persistence)==unit)]
          y[y<2^(ylim[1])] <- 2^(ylim[1])
          y <- log2(y)

          # points(x,y)
          if(length(x)>1){
            if(var(x)>0) if(var(y)==0) lines(range(x, na.rm=T), range(y), col=col.focal) else dataEllipse(x, y, levels=0.5, center.pch=21, add=T, col=col.focal, fill=T, lwd=0.5, plot.points=F)
          }
          points(mean(x),mean(y), pch=21, bg=col.focal, cex=if(unit==unit.persistence.focal) 4.5 else 3, col=col.focal2)
          text(mean(x),mean(y), unit, cex=if(unit==unit.persistence.focal) 1 else 0.7, font=2, col=col.focal2)
        }

        if(unit.persistence.focal!="none"){

          for(gcm in gcms){
            i=which(gcms==gcm)
            runs <- unique(identity$RUN[which(identity$GCM==gcm)])
            for(run in runs){
              is.focalSim <- paste(gcm, substr(run,1,2), sep="_")==sim.focal
              x2 <- persistence[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(names(persistence)==unit.persistence.focal)]
              y2 <- expansion[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(names(expansion)==unit.persistence.focal)]
              y2[y2<2^(ylim[1])] <- 2^(ylim[1])
              y2 <- log2(y2)
              # if(all(diff(x2) > 0)){
              if(length(unique(sign(diff(x2))))==1 & sum(diff(x2))!=0){
                x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
                y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
                s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
                if(run=="ensembleMean") lines(s, col=ColScheme.gcms[i], lwd=2)
              } else if(run=="ensembleMean") lines(x2, y2, col=ColScheme.gcms[i], lwd=2)
              j=which(periods==period)
              if(length(runs)>2) points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=1)
              if(run=="ensembleMean") points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=if(gcm=="Ensemble mean") 3.5 else 3)
              if(run=="ensembleMean") text(x2[j],y2[j], mods[i], cex=if(gcm=="Ensemble mean") 0.7 else 0.5, font=2)
            }
          }
          
          if(recent==T){
            x1 <- persistence[2, which(names(persistence)==unit.persistence.focal)]
            y1 <- expansion[2, which(names(expansion)==unit.persistence.focal)]
            y1[y1<2^(ylim[1])] <- 2^(ylim[1])
            y1 <- log2(y1)
            points(x1,y1, pch=21, bg="gray", col=1, cex=2)
            text(x1,y1, "2001-2020 (observed)", cex=1, font=2, pos=4, col="gray30", offset=0.7)
          }

        }
        box()

      }
    } else if(input$type==3){

      if(input$plotspp==1){

        if(edatope=="B2") spp.focal <- input$spp.focal.1.B2
        if(edatope=="C4") spp.focal <- input$spp.focal.1.C4
        if(edatope=="D6") spp.focal <- input$spp.focal.1.D6

        #-------------------------
        # species scatterplot
        #-------------------------

        data <- if(fractional==T) get(paste("suit.area", edatope, sep=".")) else get(paste("spp.area", edatope, sep="."))
        clim.data <- clim.meanChange
        ColScheme <- sppcolors
        spps <- names(data)

        x <- clim.data[, which(variables==var1)]
        variable.type1 <- variable.types[which(variables==var1)]

        xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)-c(diff(range(x))/4, 0)
        ylim=c(0, max(data, na.rm=T)*1.05)

        par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
        plot(0,col="white", tck=0, xaxt="n", yaxt="n", xaxs="i", yaxs="i", xlim=xlim, ylim=ylim, ylab="",
             xlab=paste("Change in", variable.names$Variable[which(variable.names$Code==var1)]),
        )
        par(mgp=c(2.5,0.25, 0))
        title(ylab=paste("Tree species' feasible area ('000 sq.km)"))

        axis(1, at=pretty(x), labels=pretty(x), tck=0)
        axis(2, at=pretty(ylim), labels=pretty(ylim)/1000, tck=0, las=2)

        lines(rep(x[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==period)], 2), ylim, col="gray90", lwd=2)
        
        # establish whether to plot the spp label on the right or left, based on whether the trajectory is increasing or decreasing. 
        increasing <- data[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==periods[length(periods)]),]>data[1,]
        order.increasing <- rev(order(unlist(as.vector(data[which(identity$GCM=="ensembleMean" & identity$SSP==scenario & identity$PERIOD==periods[length(periods)]),increasing==T]))))
        order.decreasing <- rev(order(unlist(as.vector(data[1,increasing==F]))))
        spps.increasing <- spps[increasing==T][order.increasing]
        spps.decreasing <- spps[increasing==F][order.decreasing]
        data.increasing <- data[,increasing==T][,order.increasing]
        data.decreasing <- data[,increasing==F][,order.decreasing]
        spps.sort <- c(spps.increasing, spps.decreasing)
        data.sort <- cbind(data.increasing, data.decreasing)
        increasing.sort <- increasing[match(spps.sort, spps)]
        for(spp in spps.sort){
          i <- which(spps.sort==spp)
          col.focal <- if(spp.focal=="none") sppcolors[i] else "lightgray"
          col.focal2 <- if(spp.focal=="none") "black" else "darkgray"
          x1 <- x[c(1, which(identity$GCM=="ensembleMean" & identity$SSP==scenario))]
          y1 <- data.sort[c(1, which(identity$GCM=="ensembleMean" & identity$SSP==scenario)), which(spps.sort==spp)]
          y1[is.na(y1)] <- 0
          if(length(unique(sign(diff(x1))))==1){
            x3 <- if(unique(sign(diff(x1)))==-1) rev(x1) else x1
            y3 <- if(unique(sign(diff(x1)))==-1) rev(y1) else y1
            s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
            lines(s, col=col.focal, lwd=3)
          } else lines(x1, y1, col=col.focal, lwd=3)

          # labels
          position <- rep(0:2, times=100)
          side <- if(increasing.sort[i]==T) 4 else 2
          space <- 12
          lines(if(increasing.sort[i]==T) c(x1[length(x1)], x1[length(x1)]+position[i]*diff(range(x))/space) else c(x1[1], x1[1]-position[i]*diff(range(x))/space),
                if(increasing.sort[i]==T) rep(y1[length(y1)],2) else rep(y1[1],2), col=col.focal, lty=2)
          text(if(increasing.sort[i]==T) x1[length(x1)]+position[i]*diff(range(x))/space else x1[1]-position[i]*diff(range(x))/space,
               if(increasing.sort[i]==T) y1[length(y1)] else y1[1],
               spp, col=col.focal, pos=side, font=2, cex=0.7, offset=0.1)

          if(recent==T){
            x1 <- x[1:2]
            y1 <- data.sort[1:2, which(spps.sort==spp)]
            lines(x1, y1, col=col.focal, lwd=1.25, lty=1)
            points(x1[2],y1[2], pch=21, bg=col.focal, col=col.focal2, cex=1.2)
          }

        }

        if(spp.focal!="none"){
          for(gcm in gcms){
            i=which(gcms==gcm)
            runs <- unique(identity$RUN[which(identity$GCM==gcm)])
            for(run in runs){
              is.focalSim <- paste(gcm, substr(run,1,2), sep="_")==sim.focal
              x2 <- x[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run))]
              y2 <- data[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(spps==spp.focal)]
              if(length(unique(sign(diff(x2))))==1){
                x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
                y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
                s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
                if(run=="ensembleMean") lines(s, col=ColScheme.gcms[i], lwd=if(gcm=="ensembleMean") 4 else 2)
              } else if(run=="ensembleMean") lines(x2, y2, col=ColScheme.gcms[i], lwd=if(gcm=="ensembleMean") 4 else 2)
              j=which(periods==period)
              if(length(runs)>2) points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=1)
              if(run=="ensembleMean") points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=if(gcm=="ensembleMean") 3.5 else 3)
              if(run=="ensembleMean") text(x2[j],y2[j], mods[i], cex=if(gcm=="ensembleMean") 0.7 else 0.5, font=2)
            }
          }
        }
        

      } else if(input$plotspp==2){

        #-------------------------
        # species bubbleplot
        #-------------------------

        if(edatope=="B2") spp.focal <- input$spp.focal.2.B2
        if(edatope=="C4") spp.focal <- input$spp.focal.2.C4
        if(edatope=="D6") spp.focal <- input$spp.focal.2.D6

        persistence <- if(fractional==T) get(paste("suit.persistence", edatope, sep=".")) else get(paste("spp.persistence", edatope, sep="."))
        expansion <- if(fractional==T) get(paste("suit.expansion", edatope, sep=".")) else get(paste("spp.expansion", edatope, sep="."))
        spps <- names(persistence)

        par(mar=c(3,4,0.1,0.1), mgp=c(1.25, 0.25, 0), cex=1.5)

        xlim <- c(0, 1.5)
        ylim <- c(-5,3)
        plot(0, xlim=xlim, ylim=ylim, col="white", xaxt="n", yaxt="n", xlab="Persistence within historically feasible range", ylab="")
        axis(1,at=seq(xlim[1], xlim[2], 0.2), labels=paste(seq(xlim[1], xlim[2], 0.2)*100,"%", sep=""), tck=0)
        axis(2,at=seq(ylim[1], ylim[2]), labels=paste(round(2^(seq(ylim[1], ylim[2]))*100),"%", sep=""), las=2, tck=0)
        par(mgp=c(2.75, 0.25, 0))
        title(ylab="Expansion beyond historically feasible range", cex.lab=1)
        iso <- seq(0,1.2, 0.001)
        lines(1-iso, log2(iso), lty=2, lwd=2, col="darkgray")
        # arctext(x = "Growing feasible range", center = c(-1, -28.7), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
        # arctext(x = "Shrinking feasible range", center = c(-1, -29.3), radius = 4.6, start = 0.431*pi , cex = 0.8, stretch = 1.05, col="darkgray", font=2)
        # mtext(paste(edatope.names[which(edatopes==edatope)], " sites", " (", edatope, ")", sep=""), side=3, line=-1.25, adj= if(edatope=="C4") 0.025 else 0.075, cex=0.7, font=1)

        spp=spps[1]
        for(spp in spps){
          i <- which(spps==spp)
          col.focal <- if(spp.focal=="none") sppcolors[i] else "lightgray"
          col.focal2 <- if(spp.focal=="none") "black" else "darkgray"
          x <- persistence[which(identity$SSP==scenario & identity$PERIOD==period), i]
          y <- expansion[which(identity$SSP==scenario & identity$PERIOD==period), i]
          y[y<2^(ylim[1])] <- 2^(ylim[1])
          y <- log2(y)

          # points(x,y)
          if(length(x)>1){
            if(var(x)>0) if(var(y)==0) lines(range(x), range(y), col=col.focal) else dataEllipse(x, y, levels=0.5, center.pch=21, add=T, col=col.focal, fill=T, lwd=0.5, plot.points=F)
          }
          points(mean(x),mean(y), pch=21, bg=col.focal, cex=if(spp==spp.focal) 4.5 else 3, col=col.focal2)
          text(mean(x),mean(y), spp, cex=if(spp==spp.focal) 1 else 0.7, font=2, col=col.focal2)
        }

        if(spp.focal!="none"){
          for(gcm in gcms){
            i=which(gcms==gcm)
            runs <- unique(identity$RUN[which(identity$GCM==gcm)])
            for(run in runs){
              is.focalSim <- paste(gcm, substr(run,1,2), sep="_")==sim.focal
              x2 <- persistence[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(spps==spp.focal)]
              y2 <- expansion[c(1, which(identity$GCM==gcm & identity$SSP==scenario & identity$RUN==run)), which(spps==spp.focal)]
              y2[y2<2^(ylim[1])] <- 2^(ylim[1])
              y2 <- log2(y2)
              # if(all(diff(x2) > 0)){
              if(length(unique(sign(diff(x2))))==1 & sum(diff(x2))!=0){
                x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
                y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
                s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
                if(run=="ensembleMean") lines(s, col=ColScheme.gcms[i], lwd=2)
              } else if(run=="ensembleMean") lines(x2, y2, col=ColScheme.gcms[i], lwd=2)
              j=which(periods==period)
              if(length(runs)>2) points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=1)
              if(run=="ensembleMean") points(x2[j],y2[j], pch=21, bg=ColScheme.gcms[i], cex=if(gcm=="ensembleMean") 3.5 else 3)
              if(run=="ensembleMean") text(x2[j],y2[j], mods[i], cex=if(gcm=="ensembleMean") 0.7 else 0.5, font=2)
            }
          }
        }
      }
    }
  }
  output$scatterPlot <- renderPlot({ scatterPlot() },
                                   height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.25,0))
  )

  # Plot download
  output$downloadPlot <- downloadHandler(
    filename =  function(){
      paste(studyarea,
            if(input$type==1){
              paste(
                "plot.climate",
                input$var1,
                input$var2,
                sep="."
              )
            } else if(input$type==2){
              paste(
                "plot.bgc",
                if(input$plotbgc==1){
                  paste("area", if(input$zonelevel==T) input$zone.area.focal else input$bgc.area.focal, sep=".")
                } else {
                  paste("persistence", if(input$zonelevel==T) input$zone.persistence.focal else input$bgc.persistence.focal, sep=".")
                },
                sep="."
              )
            } else if(input$type==3){
              paste(
                "plot.spp",
                if(input$plotbgc==1){
                  paste("area", if(edatope=="B2") input$spp.focal.1.B2 else if(edatope=="C4") input$spp.focal.1.C4 else input$spp.focal.1.D6, sep=".")
                } else {
                  paste("persistence", if(edatope=="B2") input$spp.focal.2.B2 else if(edatope=="C4") input$spp.focal.2.C4 else input$spp.focal.2.D6, sep=".")
                },
                sep="."
              )
            },
            period,
            "png", sep=".")
    },

    content = function(file) {

      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_scatterPlot_width
      height <- session$clientData$output_scatterPlot_height

      png(file, width = width*pixelratio*1.5, height = height*pixelratio*2, res = 120*pixelratio)
      scatterPlot()
      dev.off()
    }
  )

  #-------------------------
  # Find-a-BEC
  #-------------------------

  # showbgc <- "BGxh1"
  # showzone <- "BG"

  output$becmap <- renderLeaflet({

    showbgc <- input$showbgc
    showzone <- input$showzone

    leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
      fitBounds(lng1 = extent(bgc.simple)[1], lat1 = extent(bgc.simple)[2], lng2 = extent(bgc.simple)[3], lat2 = extent(bgc.simple)[4]) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>%
      addPolygons(data=bgc.simple[zone.maprecord == showzone,], fillColor = "red", color="red", smoothFactor = 0.2, fillOpacity = 0.4, weight=2)%>%
      addPolygons(data=bgc.simple[bgc.maprecord == showbgc,], fillColor = "black", color="black", smoothFactor = 0.2, fillOpacity = 0.4, weight=2)

  },
  )

  #-------------------------
  # Model Metadata Table
  #-------------------------

  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata,
                  options = list(pageLength = dim(modelMetadata)[1]),
                  rownames= FALSE,
                  caption = HTML("<p><h4>Global climate models featured in this app; see the <a href='https://bcgov-env.shinyapps.io/cmip6-BC/' target='_blank'>cmip-BC app</a> for more info.
                                 ECS is equilibrium climate sensitivity (long-term temperature change in response to an instant doubling of CO2), and values are quoted from <a href='https://advances.sciencemag.org/content/6/26/eaba1981.abstract' target='_blank'>Meehl et al. (2020)</a>.
                                 The last column is the number of model runs for each scenario that are included in ClimateBC</p></h4>")
    )
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)


