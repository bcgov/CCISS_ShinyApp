## spatial climates
library(data.table)
library(sf)
library(RPostgreSQL)
library(dplyr)
library(foreach)
library(rmapshaper)
library(tictoc)
library(rasterVis)
library(raster)
library(ccissdev)

##some setup
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", 
                 host = "138.197.168.220",
                 password = "PowerOfBEC", port = 5432, 
                 dbname = "cciss")
X <- raster("BC_Raster.tif")
X <- raster::setValues(X,NA)
outline <- st_read(con,query = "select * from bc_outline")


##make projected bgc maps - can skip this part
scn <- "ssp245";fp <- "2021-2040";gcm <- "EC-Earth3" ##select options
dat <- dbGetQuery(con,paste0("select rast_id, bgc_pred from pts2km_future where gcm = '",gcm,"' and scenario = '",
                             scn,"' and futureperiod = '",fp,"'"))
setDT(dat)
bgcs <- unique(dat$bgc_pred)
bgcID <- data.table(bgc = bgcs, id = 1:length(bgcs))
cols <- subzones_colours_ref
dat[cols,Col := i.colour, on = c(bgc_pred = "classification")]
dat[bgcID,bgcID := i.id, on = c(bgc_pred = "bgc")]

X[dat$rast_id] <- dat$bgcID
X2 <- ratify(X)
rat <- as.data.table(levels(X2)[[1]])
rat[dat,`:=`(bgc = i.bgc_pred, col = i.Col), on = c(ID = "bgcID")]
pdf(file=paste0("./BGCFuturesMaps/BGC_Projections",gcm,fp,scn,"test.pdf"), width=6.5, height=7, pointsize=10)
plot(X2,col = rat$col,legend = FALSE,axes = FALSE, box = FALSE, main = paste0(gcm," (",fp,", ",scn,")"))
plot(outline, col = NA, add = T)
dev.off()


