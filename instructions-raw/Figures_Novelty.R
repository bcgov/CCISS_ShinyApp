# remotes::install_github("bcgov/ccissr@development")


#---------------------------
#---------------------------
# Figures for the vignette on the biogeoclimatic projections novelty metric 
#---------------------------
#---------------------------

library(climr)
library(terra)
library(data.table)
library(bcmaps)
library(ccissr)
library(ranger)
library(scales)
library(EnvStats)
library(plotly)

#---------------------------
# Data
#---------------------------

#BGC model and variable list
# BGCmodel <- readRDS("//objectstore2.nrs.bcgov/ffec/BGC_models/BGCmodel_WNA_V2.1.rds") # permanent storage but too slow
BGCmodel <- readRDS("C:/Users/CMAHONY/Government of BC/Future Forest Ecosystems Centre - CCISS - CCISS/ccissv13_workingfiles/BGC_modelling/Trained_Models//BGCmodel_WNA_V2.1.rds")
pred_vars <- BGCmodel[["forest"]][["independent.variable.names"]] ##required predictors

# bc boundary
bc <- vect(bc_bound())
bc <- project(bc, "EPSG:4326")

# DEM
# dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/", sep="") # permanent storage but too slow
dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/PRISM_dem/", sep="")
dem <- rast(paste(dir, "PRISM_dem.asc", sep=""))
dem <- aggregate(dem, fact=3)
dem <- mask(dem, bc)
dem <- trim(dem)
plot(dem)
# climate data for the biogeoclimatic projections
grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
clim.grid <- downscale(xyz = grid,
                       gcms = list_gcms()[5],
                       ssps = list_ssps()[2],
                       gcm_periods = list_gcm_periods(),
                       run_nm = list_runs_ssp(list_gcms()[5], list_ssps()[2])[4],
                       vars = list_vars()
)
addVars(clim.grid)
clim.grid <- clim.grid[is.finite(CMD.total)] #remove NA rows to have complete cases for RF model

#historical climate for training points
pts <- fread("//objectstore2.nrs.bcgov/ffec/BGC_models/points_WNA_simple200.csv")
# pts <- fread("C:/Users/CMAHONY/Government of BC/Future Forest Ecosystems Centre - CCISS - CCISS/ccissv13_workingfiles/BGC_modelling/TrainingSamples/points_WNA_v2.1.csv")
clim.pts <- downscale(xyz = pts,
                      vars = list_vars())
addVars(clim.pts)
clim.pts <- pts[clim.pts, on = "id"]

# Calculate the centroid climate for the training points
clim.pts.mean <- clim.pts[, lapply(.SD, mean), by = BGC, .SDcols = -c("id", "PERIOD")]

# historical interannual climatic variability at the geographic centroids of the training points
pts.mean <- pts[, lapply(.SD, mean), by = BGC]
pts.mean$id <- 1:dim(pts.mean)[1]
clim.icv.pts <- downscale(xyz = pts.mean,
                          obs_years = 1951:1990,
                          obs_ts_dataset = "cru.gpcc",
                          return_refperiod = FALSE,
                          vars = list_vars())
addVars(clim.icv.pts)
clim.icv.pts <- pts.mean[clim.icv.pts, on = "id"]

#---------------------------
# EDA with Scree and 3D plots
#---------------------------

# climate data and BGC projections
clim.targets <- clim.grid[PERIOD == list_gcm_periods()[3], ]
bgc.pred <- predict(BGCmodel, data = clim.targets)[['predictions']]

# plots of focal analogs
bgc.focal = "CWHxm_WA" # moderate to high novelty
bgc.focal <- "CRFdh_CA"
bgc.focal <- "ESSFwm3" # good example of true analog plus novel fringe
bgc.focal <- "SBAPcp" # example where pooled pca is necessary.
bgc.focal <- "MGPmg"
bgc.focal <- "BWBScmE" # good example of novelty along a low-variance dimension.
bgc.focal <- "IDFmw2" # significant separation in the 4th PC.
bgc.focal <- "CWHvh1" # how non-normality in the analog spatial distribution can distort novelty metric
bgc.focal <- "IDFdxx_WY" #
bgc.focal <- "IDFdk5" # low novelty in pred_vars but high novelty in basic variables
analog_novelty(clim.targets = clim.targets,
               clim.analogs = clim.pts,
               label.targets = bgc.pred,
               label.analogs = clim.pts$BGC,
               # vars = pred_vars[-which(pred_vars=="CMI")], # remove CMI as it is NA along the coast (climr bug)
               vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
               # pcs = 3,
               analog.focal = bgc.focal,
               # logVars = FALSE,
               plotScree = TRUE,
               clim.icvs <- clim.icv.pts,
               label.icvs <- clim.icv.pts$BGC,
               plot2d = TRUE,
               plot3d = TRUE,
               plot3d.pcs=c(1,2,3),
               plot3d.candidates = TRUE
)


#---------------------------
# Novelty maps
#---------------------------

# Color Scheme for sigma novelty
breakseq <- c(0,4,8)
breakpoints <- c(seq(breakseq[1], breakseq[3], 0.01),199); length(breakpoints)
ColScheme <- c(colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "black"))(length(breakpoints)))

# template Raster
X <- dem
values(X) <- NA

# climate data and BGC projections
clim.targets <- clim.grid[PERIOD == list_gcm_periods()[3], ]
bgc.pred <- predict(BGCmodel, data = clim.targets)[['predictions']]

par(mar=c(1,1,1,1), mfrow=c(1,1))
novelty <- analog_novelty(clim.targets = clim.targets,
                          clim.analogs = clim.pts,
                          label.targets = bgc.pred,
                          label.analogs = clim.pts$BGC,
                          # vars = pred_vars[-which(pred_vars=="CMI")], # remove CMI as it is NA along the coast (climr bug)
                          vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                          clim.icvs <- clim.icv.pts,
                          label.icvs <- clim.icv.pts$BGC,
                          weight.icv = 0.5,
                          threshold = 0.95,
                          pcs = NULL, 
                          logVars = TRUE
                          
)
X[clim.targets[, id]] <- novelty
plot(X, col=ColScheme, axes=F)
mtext("Sigma novelty", side=4, line=-4.5, adj = 0.5, font=2)

# Maps at different ICV weights
png(filename=paste("instructions-raw/Figure_Novelty_maps_ICVweight_RFvar.png", sep=""), type="cairo", units="in", width=8, height=5.5, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(2,2))
weight.icvs=c(0, 0.33, 0.66, 1)
for(weight.icv in weight.icvs){
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets <- bgc.pred,
                            label.analogs <- clim.pts$BGC,
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC,
                            weight.icv = weight.icv,
                            vars = pred_vars[-which(pred_vars=="CMI")]
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(paste0("ICV weight = ", weight.icv), side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[which(weight.icvs==weight.icv)], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(weight.icv)
  }
dev.off()

# Maps at different ICV weights
png(filename=paste("instructions-raw/Figure_Novelty_maps_ICVweight_BasicVar.png", sep=""), type="cairo", units="in", width=8, height=5.5, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(2,2))
weight.icvs=c(0, 0.33, 0.66, 1)
for(weight.icv in weight.icvs){
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets <- bgc.pred,
                            label.analogs <- clim.pts$BGC,
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC,
                            weight.icv = weight.icv,
                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_"))
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(paste0("ICV weight = ", weight.icv), side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[which(weight.icvs==weight.icv)], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(weight.icv)
}
dev.off()

# Map at increasing PCs
png(filename=paste("instructions-raw/Figure_Novelty_maps_PCs_BasicVar.png", sep=""), type="cairo", units="in", width=8, height=5.5, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(2,2))
pcnums = c(2,3,4,5)
for(pcnum in pcnums){
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets = bgc.pred,
                            label.analogs = clim.pts$BGC,
                            # vars = pred_vars[-which(pred_vars=="CMI")],
                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC,
                            weight.icv = 0.5,
                            threshold = 0.95,
                            pcs = pcnum
                            
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(paste0(pcnum, "PCs"), side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[which(pcnums==pcnum)], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(pcnum)
}
dev.off()

# Maps with and without log-transformation
clim.targets <- clim.grid[PERIOD == list_gcm_periods()[3], ]
par(mar=c(1,1,1,1), mfrow=c(1,2))
for(log in c(TRUE, FALSE)){
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets <- bgc.pred,
                            label.analogs <- clim.pts$BGC,
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC,
                            logVars = log,
                            # vars = pred_vars[-which(pred_vars=="CMI")],
                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                            threshold = 0.95,
                            pcs = NULL
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-4.5, adj = 0.5, font=2)
  mtext(paste0("Basic variables", "\n", "logVars = ", log), line=-3.5, adj = 0.975, )
}

# Maps of variable sets
png(filename=paste("instructions-raw/Figure_Novelty_maps_varsets.png", sep=""), type="cairo", units="in", width=8, height=2.75, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(1,2))
varsets <- c("Basic", "BGC model")
for(varset in varsets){
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets <- bgc.pred,
                            label.analogs <- clim.pts$BGC,
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC,
                            vars = if(varset=="BGC model") pred_vars[-which(pred_vars=="CMI")] else as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                            threshold = 0.95,
                            pcs = NULL
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(paste0(varset, " variables"), side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[which(varsets==varset)], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(varset)
}
dev.off()


#---------------------------
# novelty over time
#---------------------------

png(filename=paste("instructions-raw/Figure_Novelty_maps_TimePeriod.png", sep=""), type="cairo", units="in", width=8, height=5.5, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(2,2))
for(i in c(0,1,3,5)){ # i is the iteration through gcm_periods
  it <- if(i==0) 1 else it+1
  # subset climate data to selected time period
  if(i==0){
    clim.targets <- clim.grid[PERIOD == "1961_1990", ]
  } else clim.targets <- clim.grid[PERIOD == list_gcm_periods()[i], ]
  
  # BGC projections
  bgc.pred <- predict(BGCmodel, data = clim.targets)[['predictions']]
  
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets = bgc.pred,
                            label.analogs = clim.pts$BGC,
                            # vars = pred_vars[-which(pred_vars=="CMI")],
                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(c("1961_1990", list_gcm_periods())[i+1], side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[it], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(c("1961_1990", list_gcm_periods())[i+1])
}
dev.off()


#---------------------------
# novelty over gcms
#---------------------------

gcms <- c("ACCESS-ESM1-5","EC-Earth3","GISS-E2-1-G","MPI-ESM1-2-HR")
runs <- c("r1i1p1f1","r4i1p1f1","r2i1p3f1","r1i1p1f1")

png(filename=paste("instructions-raw/Figure_Novelty_maps_GCMs.png", sep=""), type="cairo", units="in", width=8, height=5.5, pointsize=10, res=300)
par(mar=c(0,0,0,0), mfrow=c(2,2))
for(i in 1:4){ # i is the iteration through gcm_periods
  gcm = gcms[i]
  run = runs[i]
  
  clim.grid <- downscale(xyz = grid,
                         gcms = gcm,
                         ssps = list_ssps()[2],
                         gcm_periods = list_gcm_periods()[3],
                         run_nm = run,
                         vars = list_vars(),
                         return_refperiod = F
  )
  addVars(clim.grid)
  clim.grid <- clim.grid[is.finite(CMD.total)] #remove NA rows to have complete cases for RF model
  
  # subset climate data to selected time period
  clim.targets <- clim.grid[PERIOD == list_gcm_periods()[3], ]
  
  # BGC projections
  bgc.pred <- predict(BGCmodel, data = clim.targets)[['predictions']]
  
  novelty <- analog_novelty(clim.targets = clim.targets,
                            clim.analogs = clim.pts,
                            label.targets = bgc.pred,
                            label.analogs = clim.pts$BGC,
                            # vars = pred_vars[-which(pred_vars=="CMI")],
                            vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                            clim.icvs <- clim.icv.pts,
                            label.icvs <- clim.icv.pts$BGC
  )
  X[clim.targets[, id]] <- novelty
  plot(X, col=ColScheme, axes=F)
  mtext("Sigma novelty", side=4, line=-6, adj = 0.5, font=2)
  mtext(gcm, side=1, line=-3.5, adj = 0.025)
  mtext(paste0("(", letters[i], ")"), side=3, line=-4.5, adj = 0.025, font=2)
  print(gcm)
}
dev.off()


#---------------------------
# plot of novelty vs MAT change
#---------------------------

dem.small <- aggregate(dem, fact=5)
plot(dem.small)
grid.small <- as.data.frame(dem.small, cells = TRUE, xy = TRUE)
colnames(grid.small) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
clim.grid.small <- downscale(xyz = grid.small,
                       gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                       ssps = list_ssps()[1:3],
                       gcm_periods = list_gcm_periods(),
                       max_run = 3,
                       vars = list_vars()
)
addVars(clim.grid.small)
clim.grid.small <- clim.grid.small[is.finite(CMD.total)] #remove NA rows to have complete cases for RF model

# BGC projections
bgc.pred <- predict(BGCmodel, data = clim.grid.small)[['predictions']]

novelty <- analog_novelty(clim.targets = clim.grid.small,
                          clim.analogs = clim.pts,
                          label.targets = bgc.pred,
                          label.analogs = clim.pts$BGC,
                          # vars = pred_vars[-which(pred_vars=="CMI")],
                          vars = as.vector(outer(c("Tmin", "Tmax", "PPT"), c("wt", "sp", "sm", "at"), paste, sep = "_")),
                          clim.icvs <- clim.icv.pts,
                          label.icvs <- clim.icv.pts$BGC
)

clim.grid.small[,bgc.pred := bgc.pred]
clim.grid.small[,novelty := novelty]

clim.mean <- clim.grid.small[, lapply(.SD, mean), by = c("GCM", "SSP", "RUN", "PERIOD"), .SDcols = -c("id", "bgc.pred")]
clim.mean[is.na(GCM), GCM := "baseline"]
clim.mean[is.na(RUN), RUN := "baseline"]
clim.mean <- clim.mean[-which(RUN=="ensembleMean"),]

x <- clim.mean$MAT-clim.mean$MAT[1]
y <- clim.mean$novelty
z <- clim.mean$GCM
gcm_colors <- setNames(c("black", rainbow(length(unique(z))-2), "grey"), unique(z))
gcm_shapes <- setNames(rep(c(21, 22, 24), times=5)[1:length(unique(z))], unique(z))

png(filename=paste("instructions-raw/Figure_Novelty_scatterplot_ensemble.png", sep=""), type="cairo", units="in", width=8, height=5, pointsize=12, res=300)
par(mar=c(3,3,.1,.1), mfrow=c(1,1), mgp=c(1.75, 0.25, 0), tck=-0.005)
plot(x, y, pch = gcm_shapes[z], bg = gcm_colors[z], col = "black", cex = 1.5, yaxt="n", 
     ylim = c(0, max(y)*1.05), yaxs="i",
     xlab = "Change in mean annual temperature (\u00B0C)", 
     ylab = "Mean novelty")
axis(2, at=pretty(y), labels = paste0(pretty(y), "\u03C3"), las=2)
legend("topleft", legend = names(gcm_colors), pt.cex = 1.5, pch=gcm_shapes, pt.bg = gcm_colors, title = "GCM", bty = "n")
dev.off()

#---------------------------
# Illustration of Mahalanobis distance
#---------------------------
library(MASS)   #provides eqscplot(x,y), which plots axes of equal scale
library(plotrix)  #provides draw.ellipse()

png(filename=paste("instructions-raw/Figure_Novelty_MahalDemo.png", sep=""), type="cairo", units="in", width=8, height=6, pointsize=12, res=300)

bgc = "ICHwk4"
x.var = "Tmax_sm"
y.var = "PPT_sm"
y.scale = 60 #log base to get a decent equal scale plot
x <- clim.icv.pts[BGC==bgc,get(x.var)]
y <- clim.icv.pts[BGC==bgc,get(y.var)]/y.scale
z <- clim.icv.pts[BGC==bgc,PERIOD]
data <- data.frame(x,y)

par(mfrow=c(1,1), mar=c(3,3,0.1,0.1), mgp=c(2, 0.25, 0), tck=-0.001)
eqscplot(x, y, pch = 19, col = alpha("grey",1), cex=1.3, yaxt="n",
         xlim=c(min(data[,1]-2), max(data[,1])+2),
         xlab = "Mean maximum daily temperature in Summer (Celsius)", ylab = "Summer Precipitation (mm)")
axis(2, at=seq(100, 500, 100)/y.scale, labels = seq(100, 500, 100), las=2)

pca <- prcomp(data, retx=T)  #PCA on the interannual variability (unscaled. scaling it reduces the effectiveness of the illustration)
slope <- pca$rotation[2, ]/pca$rotation[1, ]; 
mn <- apply(data, 2, mean)
k=sqrt(qchisq(0.95, df = 2)); draw.ellipse(mean(data[,1]),mean(data[,2]), a=pca$sdev[1]*k, b=pca$sdev[2]*k, angle=atan(slope[1])*360/2/pi, lty=2, border="black")
k=sqrt(qchisq(0.997, df = 2)); draw.ellipse(mean(data[,1]),mean(data[,2]), a=pca$sdev[1]*k, b=pca$sdev[2]*k, angle=atan(slope[1])*360/2/pi, lty=2, border="black")
# k=sqrt(qchisq(0.99994, df = 2)); draw.ellipse(mean(data[,1]),mean(data[,2]), a=pca$sdev[1]*k, b=pca$sdev[2]*k, angle=atan(slope[1])*360/2/pi, lty=2, border="black")
# k=sqrt(qchisq(0.9999994, df = 2)); draw.ellipse(mean(data[,1]),mean(data[,2]), a=pca$sdev[1]*k, b=pca$sdev[2]*k, angle=atan(slope[1])*360/2/pi, lty=2, border="black")
#establish coordinates of the PC z-scores
sigma <- c(-3,-2,-1,1,2,3) #which z-scores to provide tick marks for 
pc1.x <- mean(data[,1])+sigma*sqrt(pca$sdev[1]^2/(1+slope[1]^2))
pc1.y <- rev(mean(data[,2])+sigma*sqrt(pca$sdev[1]^2/(1+slope[2]^2)))
pc2.x <- mean(data[,1])+sigma*sqrt(pca$sdev[2]^2/(1+slope[2]^2))
pc2.y <- mean(data[,2])+sigma*sqrt(pca$sdev[2]^2/(1+slope[1]^2))
#draw the PC axes
segments(min(pc1.x),max(pc1.y),max(pc1.x),min(pc1.y))
segments(min(pc2.x),min(pc2.y),max(pc2.x),max(pc2.y))
tk <- 0.04; exp <- 8
segments(pc1.x-sqrt(tk^2/(1+(1/slope[1])^2)),pc1.y-sqrt(tk^2/(1+slope[1]^2)),pc1.x+sqrt(tk^2/(1+(1/slope[1])^2)),pc1.y+sqrt(tk^2/(1+slope[1]^2)))
segments(pc2.x-sqrt(tk^2/(1+(1/slope[2])^2)),pc2.y+sqrt(tk^2/(1+slope[2]^2)),pc2.x+sqrt(tk^2/(1+(1/slope[2])^2)),pc2.y-sqrt(tk^2/(1+slope[2]^2)))
par(srt=-35) 
text(pc1.x-sqrt((tk*exp)^2/(1+(1/slope[1])^2)),pc1.y-+sqrt((tk*exp)^2/(1+slope[1]^2)),sigma)
text(pc2.x-sqrt((tk*exp)^2/(1+(1/slope[2])^2))+0.5,pc2.y-sqrt((tk*exp)^2/(1+slope[2]^2)),sigma)
text(max(pc1.x)-0., min(pc1.y)-0.07, "PC1", pos=4, font=2)
par(srt=55); text(max(pc2.x)+0.28, max(pc2.y)+.1, "PC2", pos=3, font=2); par(srt=0)

data.new <- data.frame(x=22.5, y=300/y.scale)
points(data.new, pch=16, cex=1.5)

dev.off()