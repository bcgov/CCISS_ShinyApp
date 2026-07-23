library(ccissr)
library(data.table)
library(pool)
library(RPostgres)

breakseq <- c(0,4,8)
bs2 <- breakseq * 100
breakpoints <- seq.int(bs2[1], bs2[3], 1)
ColScheme <- colorRampPalette(c("gray90", "gray50", "#FFF200", "#CD0000", "#000000"))(length(breakpoints))
coltab <- data.table(
  values = as.integer(breakpoints),
  color = ColScheme               # Corresponding colors
)


sspred <- fread("sspred_test.csv")
suit <- fread("suitability_v13_14.csv")
suit[,V1 := NULL]
setnames(suit, c("BGC","SS_NoSpace","Sppsplit","FeasOrig","Spp","Feasible","Mod","OR"))

data("E1_Phase")
phase_cw <- E1_Phase[,.(SS_NoSpace,MainUnit)]

cciss_full <- function(SSPred,suit,spp_select){
  suit <- suit[Spp %in% spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[!grepl("[0-9]a$|[0-9]b$|[0-9]c$",SS_NoSpace),]
  SSPred <- SSPred[!grepl("\\.1$|\\.2$|\\.3$",SS_NoSpace),]

  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
  # Fill with 0 if columns does not exist, encountered the error at SiteRef 3104856 
  colNms <- c("1","2","3","X")
  set(suitVotes, j = as.character(1:5)[!as.character(1:5) %in% names(suitVotes)], value = 0)
  
  suitVotes[,VoteSum := `1`+`2`+`3`+`4`+`5`]
  suitVotes[,X := 1 - VoteSum]
  suitVotes[,VoteSum := NULL]
  suitVotes[,X := X + `5` + `4`]
  suitVotes[,`:=`(`5` = NULL, `4` = NULL)]
  setkey(suitVotes, SS_NoSpace, Spp)
  setkey(suit, SS_NoSpace, Spp)
  suitVotes[suit, Curr := i.Feasible]
  suitVotes[is.na(Curr), Curr := 5]
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[Curr > 3.5, Curr := 4]
  
  suitVotes[,Improve := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Improve"),.SDcols = colNms]
  suitVotes[,Decline := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Decline"),.SDcols = colNms]
  datRot <- suitVotes[,lapply(.SD, mean),.SDcols = c("Improve","Decline"), by = list(SiteRef,SS_NoSpace,FuturePeriod,Spp,Curr)]
  datRot[,`:=`(Improve = round(Improve*100),Decline = round(Decline*100))]
  datRot[,Curr := NULL]
  
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,Newsuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitVotes <- merge(suitVotes, datRot, by = c('SiteRef','FuturePeriod','SS_NoSpace','Spp'),all = T)
  suitRes <- suitVotes[,.(Curr = mean(Curr),Newsuit = mean(Newsuit), Improve = mean(Improve), Decline = mean(Decline), Prop1 = mean(`1`), Prop2 = mean(`2`), Prop3 = mean(`3`)), by = .(SiteRef,FuturePeriod,Spp)]
  return(suitRes)
}

cppFunction('NumericVector ModelDir(NumericMatrix x, NumericVector Curr, std::string dir){
  int n = x.nrow();
  NumericVector res(n);
  NumericVector temp(5);
  NumericVector temp2;
  double curr_suit;
  if(dir == "Improve"){
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_front(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(0,curr_suit)]);
    }
  }else{
    for(int i = 0; i < n; i++){
      temp = x(i,_);
      temp.push_back(0);
      curr_suit = Curr[i];
      if(curr_suit == 4){
        curr_suit = 3;
      }
      res[i] = sum(temp[Range(curr_suit,4)]);
    }
  }
  
  return(res);
}
')

test <- cciss_full(sspred, suit, "Fd")

pool <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##spatial testing
final_dem <- rast("./app/cciss_spatial/Raster_Templated.tif")
temp <- setDT(dbGetQuery(pool, "select cellid, newsuit from cciss_feas where fp_code = 2021 and spp_id = 3 and edatope = 2 and newsuit < 400 limit 20"))

rs <- dbSendQuery(pool, "select cellid, newsuit from cciss_feas where fp_code = 2061 and spp_id = 2 and edatope = 2 and newsuit < 400")

for(i in 1:30){
  temp <- dbFetch(rs, n = 5e5)
  cat(nrow(temp), "\n")
  if(nrow(temp) < 1) break
  setDT(temp)
  fwrite(temp, "tmp_dbfeas.csv", append = T)
  rm(temp)
  gc()
}
dbClearResult(rs)

install.packages("duckdb", repos = c("https://duckdb.r-universe.dev", getOption("repos")))
library(duckdb)
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "INSTALL postgres;")
dbExecute(con, "LOAD postgres;")

dbPointInfo <- function(con, points) {
  
  # It is more efficient for the query optimizer to do
  # each join in a separate sql, better use of index and caching
  # geometry have been optimized (split polygons) for the
  # whole function to return under 50ms
  
  Long <- .subset2(points, "Long")
  Lat <- .subset2(points, "Lat")
  ID <- .subset2(points, "ID")

  elev_info_sql <- paste0("
    WITH pts4269 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 4269) geom,
      ",ID," id ", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT pts.id, 
    MAX(ROUND(CAST(ST_Value(dem.rast, pts.geom) as NUMERIC), 2)) elevation_m
    FROM bc_elevation dem
    CROSS JOIN pts4269 pts
    WHERE ST_Intersects(dem.rast, pts.geom)
    GROUP BY pts.id
  ")

    bec_info_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom,
      ",ID," id ", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT pts.id,
           bec.zone,
           bec.subzone,
           bec.variant,
           bec.phase,
           bec.natural_disturbance,
           bec.map_label,
           bec.bgc_label,
           bec.zone_name,
           bec.subzone_name,
           bec.variant_name,
           bec.phase_name,
           bec.natural_disturbance_name,
           bec.feature_area_sqm,
           bec.feature_length_m
    FROM pts3005 pts
    LEFT JOIN bec_info bec
    ON ST_Intersects(pts.geom, bec.geometry)
  ")
  
  site_ref_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom,
      ",ID," id ", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT pts.id, 
    hgrid.siteno site_no
    FROM pts3005 pts 
    LEFT JOIN hex_grid hgrid
    ON ST_Intersects(pts.geom, hgrid.geom)
  ")
  
  bc_land_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom,
      ",ID," id ", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT pts.id,
    bcb_hres.objectid is NOT NULL onbcland
    FROM pts3005 pts
    LEFT JOIN bcb_hres
    ON ST_Within(pts.geom, bcb_hres.geometry)
  ")
  
  bc_forest_region <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom,
      ",ID," id ", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT pts.id,
    region_ref forest_region
    FROM pts3005 pts
    LEFT JOIN bc_forest_regions
    ON ST_Within(pts.geom, bc_forest_regions.geom)                       
  ")
  
  res1 <- RPostgres::dbGetQuery(con, bec_info_sql) |> as.data.table()
  setkey(res1,id)
  res2 <- RPostgres::dbGetQuery(con, elev_info_sql)|> as.data.table()
  setkey(res2,id)
  res3 <- RPostgres::dbGetQuery(con, site_ref_sql)|> as.data.table()
  setkey(res3,id)
  res4 <- RPostgres::dbGetQuery(con, bc_land_sql)|> as.data.table()
  setkey(res4,id)
  res5 <- RPostgres::dbGetQuery(con, bc_forest_region)|> as.data.table()
  setkey(res5,id)
  res <- res1[res2,][res3,][res4,][res5,]
  return(res)  
}

dat <- fread("../../../Downloads/cciss_points_final.csv")
dat <- dat[,.(ID,Lat = lat, Long = long)]
res2 <- dbPointInfo(pool, dat)

Long <- .subset2(dat, "Long")
Lat <- .subset2(dat, "Lat")

site_ref_sql <- paste0("
    WITH pts3005 AS (
      ", paste0("SELECT st_transform(st_pointfromtext('POINT(", Long, " ", Lat, ")', 4326), 3005) geom", collapse = "\n UNION ALL \n") ,"
    )
    
    SELECT cast(hgrid.siteno as text) site_no
    FROM pts3005 pts 
    LEFT JOIN hex_grid hgrid
    ON ST_Intersects(pts.geom, hgrid.geom)
  ")

res <- dbGetQuery(pool, site_ref_sql)

bgc <- dbGetQuery(pool, "select * from bgc_preds where cellid = 19906357")

suit <- dbGetQuery(pool, paste0("select fp_code, newsuit, prop1, prop2, prop3 from cciss_feas where cellid = ",
                                "19906357"," and edatope = 2 and spp_id = 3")) |> as.data.table()


gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,0,0,1,1,1,1,0,0,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]


ccissOutput <- function(SSPred,suit,rules,feasFlag,histWeights,futureWeights){
  
  # Declare binding for checks
  if (FALSE) {
    BGC <-
      SS_NoSpace <-
      Spp <-
      Feasible <-
      SiteRef <-
      FuturePeriod <-
      SS.pred <-
      SSprob <-
      VoteSum <-
      `1` <-
      `2` <-
      `3` <-
      `4` <-
      `5` <-
      X <-
      Curr <-
      i.Feasible <-
      ModAgree <-
      NewSuit <-
      SuitDiff <-
      Xadj <-
      X2 <-
      NewSuitFrac <-
      Flag <-
      i.Flag <-
      Improve <-
      Decline <-
      Weight <-
      i.Weight <-
      ccissSuit <-
      ccissSuitFrac <-
      OrderCol <-
      IncludeFlag <-
      NULL
  }
  
  # library(data.table)
  # SSPred <- fread("../CCISS_ShinyApp/app/temp_sspred.csv")
  # bgc <- fread("../CCISS_ShinyApp/app/temp_bgc.csv")
  # futureWeights <- rep(0.25,4)
  # S1 <- ccissr::S1
  # S1 <- S1[,.(bgc,ss_nospace, spp, newfeas, outrange)]
  # setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible","OHR"))
  # suit <- S1
  # SSPred[bgc, Novelty := i.Novelty, on = c("SiteRef","FuturePeriod","BGC","BGC.pred")]
  
  ### generate raw feasibility ratios
  ccissWt <- data.table(FuturePeriod = c(2021,2041,2061,2081),
                        Weight = futureWeights)
  
  suit <- suit[,list(BGC,SS_NoSpace,Spp,Feasible)]
  ## replace the coast/interior divisions of species
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,list(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob,Novelty)]
  Site_BGC <- unique(SSPred[,list(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
  novelVotes <- dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible,
                      value.var = "Novelty", fun.aggregate = mean)
  setnafill(novelVotes,fill = 0, cols = as.character(1:5)) ###should we return this separately, or merge?
  # Fill with 0 if columns does not exist, encountered the error at SiteRef 3104856 
  set(suitVotes, j = as.character(1:5)[!as.character(1:5) %in% names(suitVotes)], value = 0)
  suitVotes[,VoteSum := `1`+`2`+`3`+`4`+`5`]
  suitVotes[,X := 1 - VoteSum]
  suitVotes[,VoteSum := NULL]
  suitVotes[,X := X + `5` + `4`]
  suitVotes[,`:=`(`5` = NULL, `4` = NULL)]
  setkey(suitVotes, SS_NoSpace, Spp)
  setkey(suit, SS_NoSpace, Spp)
  suitVotes[suit, Curr := i.Feasible]
  suitVotes[is.na(Curr), Curr := 5]
  suitVotes[,ModAgree := matrixStats::rowMaxs(as.matrix(.SD)), .SDcols = c("1","2","3","X")]
  suitVotes[,NewSuit := NewSuitNoCurr(as.matrix(.SD),c(1,2,3,5)), .SDcols = c("1","2","3","X")]
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[,FuturePeriod := as.integer(FuturePeriod)]
  suitVotes[Curr > 3.5, Curr := 4]
  suitVotes[NewSuit > 3.5, NewSuit := 4]
  suitVotes[,SuitDiff := stepDiff(FuturePeriod,NewSuit,Curr), by = list(SiteRef,SS_NoSpace,Spp)] ## final raw output table
  
  ##Generate summary feasibility from raw proportions
  histWt <- histWeights[1]
  currWt <- histWeights[2]    ## weight in summary given to the modern climate change period
  earlyWt <- histWeights[3]   ## weight in summary given to the 2010-2040 climate change period
  
  colNms <- c("1","2","3","X")
  datFeas <- suitVotes[FuturePeriod %in% c(1961 ,1991, 2021),]
  
  datFeas[FuturePeriod == 1961, (colNms) := lapply(.SD,"*",histWt), .SDcols = colNms]
  datFeas[FuturePeriod == 1991, (colNms) := lapply(.SD,"*",currWt), .SDcols = colNms]
  datFeas[FuturePeriod == 2021, (colNms) := lapply(.SD,"*",earlyWt), .SDcols = colNms]
  
  datFeas <- datFeas[,lapply(.SD, sum),.SDcols = colNms, by = list(SiteRef,SS_NoSpace,Spp,Curr)]
  ## ADD VARIABLE THAT IS 1-SUM OF 1-4 COLUMNS THAN ADD VALUE TO x COLUMN TO ACCOUNT FOR nULL FUTURES AND MAKE ROW SUM = 1
  
  datFeas[,Xadj := rowSums(.SD), .SDcols = colNms]
  datFeas[,X2 := X + (1-Xadj)]
  ## THEN VARIABLE THAT IS SUM OF COL1 + COL2*2, COL3*3, COL 'X'* 4 and then round(0) for establishment feasibility
  datFeas[,NewSuitFrac := `1`+(`2`*2)+(`3`*3)+(X2*5)]
  datFeas[,NewSuit := round(NewSuitFrac,0)]
  datFeas[,`:=`(X = NULL,Xadj = NULL)]
  
  #datFeas[,FeasEstab := `1`+`2`+0.75*`3`] 
  # datFeas[,NewSuit := round(NewSuit, digits = 0)]
  datFeas[NewSuit >= 4,NewSuit := 10]
  datFeas[NewSuit == 0,NewSuit := 1]
  datFeas[Curr == 4,Curr := 10]
  datFeas[,SuitDiff := Curr - NewSuit]
  datFeas[feasFlag, Flag := i.Flag, on = c("SuitDiff")]
  datFeas[Curr == 10 & NewSuit == 10, Flag := "NotIn"]
  added <- c("A1", "A2", "A3")
  datFeas <- datFeas[Curr == 10 & (Flag %in% added), Curr := 4 ]
  datFeas_final <- datFeas %>% dplyr::select(SiteRef,SS_NoSpace,Spp,NewSuit,NewSuitFrac, SuitDiff, Flag) %>% dplyr::mutate(dplyr::across(NewSuit, round, 0))%>%
    dplyr::mutate(NewSuit = sub('10', 'X', NewSuit)) 
  
  ###mid rotation trend using 41-60 and 61-80 - used for bifurcation
  datRot <- suitVotes[FuturePeriod %in% c(2021,2041,2061,2081),]
  
  ##calculate bifurcating
  datRot[,Improve := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Improve"),.SDcols = colNms]
  datRot[,Decline := ModelDir(as.matrix(.SD), Curr = Curr, dir = "Decline"),.SDcols = colNms]
  
  datRot <- datRot[,lapply(.SD, mean),.SDcols = c("Improve","Decline"), by = list(SiteRef,SS_NoSpace,Spp,Curr)]
  datRot[,`:=`(Improve = round(Improve*100),Decline = round(Decline*100))]
  #datRot[,Trend := paste0(Improve,":",Decline)]
  datRot <- datRot[,list(SiteRef,SS_NoSpace,Spp,Improve,Decline)] ##final
  
  ###################################################################
  datFuture <- suitVotes[FuturePeriod %in% c(2021,2041,2061,2081),]
  datFuture <- datFuture[,lapply(.SD, sum),.SDcols = colNms, 
                         by = list(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  datFuture[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  datFuture[ccissWt, Weight := i.Weight, on = "FuturePeriod"]
  datFuture <- datFuture[,list(ccissSuitFrac = stats::weighted.mean(NewSuit,Weight)), 
                         by = list(SiteRef,SS_NoSpace,Spp,Curr)]
  datFuture[,ccissSuit := round(ccissSuitFrac)]
  
  ###merge data to make summary tables############################################################
  summOut <- merge(datFeas_final, datRot, by = c('SiteRef','SS_NoSpace','Spp'),all = T)
  summOut <- merge(summOut, datFuture, by = c('SiteRef','SS_NoSpace','Spp'), all = T)
  summOut[,OrderCol := (Curr + NewSuitFrac + ccissSuitFrac)/3]
  summOut[,IncludeFlag := fifelse(ccissSuitFrac > 0.1,TRUE,FALSE)]
  summOut[,c("Flag","SuitDiff","NewSuitFrac","ccissSuitFrac") := NULL]
  #summOut <- summOut[Flag != "NotIn",]
  summOut[,`:=`(Curr = as.character(Curr),
                ccissSuit = as.character(ccissSuit))]
  summOut[,c("Curr","ccissSuit") := lapply(.SD,function(x){fifelse(x >= 4,"X",x)}), 
          .SDcols = c("Curr","ccissSuit")]
  summOut[is.na(NewSuit) | NewSuit == 4,NewSuit := "X"]
  #summOut[NewSuit == "X" & ccissSuit %in% c("1","2","3"), NewSuit := "Trial"]
  summOut <- summOut[!is.na(ccissSuit),]
  print("Done Feas")
  return(list(Summary = summOut, Raw = suitVotes))
}



points <- dbGetQuery(pool, "select siteno from preselected_points where bgc = 'IDFdk3'")
sitenos <- points$siteno[1:150]
bgc <- dbGetCCISS(pool, sitenos, avg = T, modWeights = all_weight)
setDT(bgc)
eda <- edatopicOverlap(bgc, E1, E1_Phase)
t1 <- bgc[,.(BGC.Sum = sum(BGC.prop)), by = .(FuturePeriod)]
BGC <- bgc[FuturePeriod == "2041",]
