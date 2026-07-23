## selection and evaluation of a small ensemble of simulations that represent the 
## extremes of the larger GCM ensemble
## Colin Mahony, 2025

library(data.table)
library(ccissr)
library(terra)
library(climr)
library(bcmaps)
library(MASS)
source("KKZ.R") # this is the KKZ script provided by Alex Cannon



### -------------------------------------------------------
### study area setup
### -------------------------------------------------------

studyarea <- "BC"

# output directory for data created in this script
dir.create(file.path("spatial_app/data", studyarea))
outdir <- paste("spatial_app/data", studyarea, sep="/")
bnd <- project(vect(paste("spatial_app/bdy/bdy", studyarea, "shp", sep=".")), "epsg:4326") #boundary file

### -------------------------------------------------------
### common variables
#lookup tables

gcms <- c("ACCESS-ESM1-5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0")

# ssps <- c("ssp126", "ssp245")
# ssp.names=c("SSP1-2.6", "SSP2-4.5")
ssps <- c("ssp245")
ssp.names=c("SSP2-4.5")

periods <- c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100") 
period.names=c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

##climr variables need for this model
vars_needed <- c("DD5","DD_0_at","DD_0_wt","PPT05","PPT06","PPT07","PPT08","PPT09","CMD","PPT_at","PPT_wt","CMD07","SHM", "AHM", "NFFD", "PAS", "CMI")

  dem <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/PRISM_dem.asc")
  dem <- aggregate(dem, fact=15)
  bnd <- vect(paste("spatial_app/bdy/bdy", studyarea, "shp", sep=".")) #boundary file
  bnd <- project(bnd,"epsg:4326") # project to albers to be able to specify resolution in meters. 
  dem <- mask(dem,bnd)
  dem <- trim(dem)

sum(!is.na(values(dem)))
plot(dem)
plot(bnd, add=T)
# plot(land, add=T, col="blue")

X <- dem # base raster
values(X) <- NA

## make the climr input file
points <- as.data.frame(dem, cells=T, xy=T)
colnames(points) <- c("id", "lon", "lat", "elev")
points <- points[,c(2,3,4,1)] #restructure for climr input
# values(X)[points$id] <- points$el ; plot(X)


# reference period climate
clim.ref <- downscale(points,
                  which_refmap  = "refmap_prism",
                  gcms  = NULL,
                  return_refperiod  = TRUE, 
                  vars = list_vars())
addVars(clim.ref)
clim.ref <- logVars(clim.ref, base=2)

## calculate mean climate of study area for use in calculating change
clim.refmean <- apply(as.data.frame(clim.ref)[,-c(1:2)], 2, FUN=mean, na.rm=T)

# future period climate
ssp <- list_ssps()[2]
period <- list_gcm_periods()[5]
clim <- downscale(points,
                  which_refmap  = "refmap_prism",
                  gcms = gcms,
                  ssps = ssp,
                  gcm_periods = period,
                  max_run = 3L,
                  return_refperiod = FALSE,
                  vars = list_vars())
addVars(clim)
clim <- logVars(clim, base=2)
unique(clim$GCM)

## calculate ensemble mean and append to clim
clim.ensembleMean <- clim[RUN == "ensembleMean", lapply(.SD, mean), by = id, .SDcols = !(id:PERIOD)]
identity <- data.table(
  id = clim.ensembleMean$id,
  GCM = rep("ensembleMean", dim(clim.ensembleMean)[1]),
  SSP = ssp, 
  RUN = rep("ensembleMean", dim(clim.ensembleMean)[1]), 
  PERIOD = period
)
clim.ensembleMean <- cbind(identity, clim.ensembleMean[,!"id"])
clim <- rbind(clim, clim.ensembleMean)

## calculate mean climate change across study area [ISSUE: REFACTOR TO DATA.TABLE]
clim.mean <- as.data.frame(clim[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = .(GCM, SSP, RUN, PERIOD), .SDcols = !(id:PERIOD)]) #mean value for each run across the study area. 
change.temp <- sweep(clim.mean[,-c(1:4)], 2, clim.refmean, FUN='-') # subtract the reference period mean vector from each row. 
change <- cbind(clim.mean[,c(1:4)], change.temp)

load("C:/Users/CMAHONY/Government of BC/Future Forest Ecosystems Centre - CCISS - CCISS/ccissv13_workingfiles/BGC_modelling/Trained_Models/BGCModel_Extratrees_Balanced.Rdata") ##load RF model
pred_vars <- BGCmodel[["forest"]][["independent.variable.names"]] ##required predictors

# use KKZ to select a subset of simulations that represent the centroid and extremes of the ensemble
select <- which(change$RUN != "ensembleMean" & change$SSP=="ssp245" & change$PERIOD == "2081_2100")
x <- change[select, which(names(change)%in%pred_vars)] 
id <- change[select, c(1,3)] 
x <- as.matrix(x[,]) # necessary for the subset.kkz function to work
x <- scale(x) #z-standardize the data
attr(x,"scaled:center")<-NULL
attr(x,"scaled:scale")<-NULL
x.kkz <- subset.kkz(x,n.cases=6) # this is the KKZ algorithm sourced from the KKZ.R script
id.kkz <- id[as.numeric(row.names(x.kkz$cases)),]
id.kkz <- rbind(id.kkz, data.frame(GCM="ensembleMean", RUN="ensembleMean")) # force the subset to include the ensemble mean
# write.csv(id.kkz, paste(outdir, "/id.kkz.csv", sep="."), row.names = F)

# plot the subset in PCA space
x.pca <- predict(prcomp(x), x)
par(mar=c(3,3,0.1,0.1))
eqscplot(x.pca[,1:2], col="white")
text(x.pca[,1:2], rownames(x), cex=0.7)
text(x.pca[,1:2], paste(id$GCM, id$RUN), cex=0.7, pos=4)
points(x.pca[which(row.names(x.pca)%in%row.names(id.kkz)),1:2], col=2, cex=3)

# ------------------------------------------
# ------------------------------------------
# PLOTS
# ------------------------------------------
# ------------------------------------------

small <- fread(paste(outdir, "/id.kkz.csv", sep="."))[1:5]

# ------------------------------------------
# scatterplots

select <- which(change$RUN != "ensembleMean" & change$SSP=="ssp245" & change$PERIOD == "2081_2100")
id <- change[select, c(1,3)] 
xvars <- c("MAT", "Tmax_sm", "Tmin_wt")
yvars <- c("PPT", "PPT_sm", "PPT_wt")
yeartimes <- c("Annual", "Summer", "Winter")
colors <- c("black", "#A13D63", "#3A7F4D", "#8257A5", "#746F57")

png(filename=paste0("instructions-raw/Figure_smallEnsemble_scatterplots.png"), type="cairo", units="in", width=6.5, height=7.5, pointsize=12, res=300)
mat <- matrix(c(1,2,1,3), 2)
layout(mat, widths=c(1,1), heights=c(2, 1))
par(mar=c(3,4,2,0.1), mgp=c(2, 0.2, 0), tck=-0.005)
for(i in 1:3){
  xvar <- xvars[i]
  yvar <- yvars[i]
  x <- change[select, xvar]
  y <- 2^change[select, yvar]*100-100
  plot(x, y, main=yeartimes[i], xlim=range(x)*c(0.95, c(1.15, 1.35, 1.35)[i]), xlab=paste0("Change in temperature (\u00B0C)"), ylab=paste0("Change in precipitation"), yaxt="n", col="gray", pch=16, cex=1.5)
  axis(2, at=pretty(y), labels=paste0(pretty(y), "%"), las=2) 
  if(i==1) text(x,y, paste(id$GCM, id$RUN), cex=0.7, pos=4, col="gray")
  s <- which(paste(id$GCM, id$RUN)%in% paste(small$GCM, small$RUN))
  points(x[s],y[s], cex=1.5, pch=16, col=colors)
  text(x[s],y[s], paste(id$GCM, id$RUN)[s], cex=0.7, pos=4, col=colors)
  mtext(paste0("(", letters[i], ")"), side=3, line=-1.5, adj = 0.015, font=2, cex=1.1)
}
dev.off()

# ------------------------------------------
# change maps

vars <- c("PPT", "PPT_sm", "PPT_wt", "Tmax_sm", "Tmin_wt")
ref <- proj <- change <- X
var=vars[2]
for(var in vars){
  ratiovar <- grepl("AHM|DD|Eref|FFP|NFFD|PAS|PPT|SHM|CMD", var)
  revcolor <- grepl("PAS|PPT", var)
  
  png(filename=paste0("instructions-raw/Figure_smallEnsemble_changeMaps_", var, ".png"), type="cairo", units="in", width=6.5, height=4.5, pointsize=10, res=300)
  
  mat <- matrix(c(7,1,2,7,3,4,7,5,6), 3)
  layout(mat, widths=c(1,1,1), heights=c(0.4, 1 ,1))
  par(mar=c(0,0,0,0))
  
  lim <- if(ratiovar) 0.585 else 6
  lim.upper <- lim
  lim.lower <- 0-lim
  breaks <- seq(lim.lower, lim.upper, lim/100)
  colscheme <- colorRampPalette(hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
  if(revcolor) colscheme <- rev(colscheme)
  
  # CRU
  elements <- c("Tmin", "Tmax", "Pr")
  e=1
  for(i in 1:6){
    gcm <- if(i==6) "ensembleMean" else selected$GCM[i]
    run <- if(i==6) "ensembleMean" else selected$RUN[i]
    title <- if(i==6) "Ensemble mean" else paste0(gcm, " (", run, ")")
    ref[clim.ref$id] <- clim.ref[, get(var)]
    proj[clim.ref$id] <- clim[GCM==gcm & RUN==run, get(var)]
    change <- proj-ref
    change <- mask(change, bnd)
    change[change>lim.upper] <- lim.upper
    change[change < lim.lower] <- lim.lower
    plot(change, col=colscheme, breaks=breaks, axes=F, type="continuous", legend=F, main=title)
    plot(bnd, add=T)
  }
  
  ## legend
  pct <- if(ratiovar) 100 else 1
  plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
  xl <- 0.2; yb <- 0.3; xr <- 0.8; yt <- 0.5
  rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
  rect(xl,  yb,  xr,  yt)
  labels <- if(ratiovar) paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct-pct, "%", sep="") else round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct
  text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
  text(mean(c(xl,xr)), yt+0.01, paste("Change in", var), pos=3, cex=1.5, font=2)
  
  dev.off()
  
  print(var)
}



