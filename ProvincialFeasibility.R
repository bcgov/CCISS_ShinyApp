library(data.table)
library(ccissdev)
library(pool)
library(RPostgres)

##setup dbi connection
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

sppDb <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "spp_feas",
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432,
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

##adapted feasibility function
cciss_feasibility <- function(SSPred,suit,spp_select){
  
  suit <- suit[Spp %chin% spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  SSPred <- SSPred[,.(SiteRef,FuturePeriod,BGC,SS_NoSpace,SS.pred,SSprob)]
  Site_BGC <- unique(SSPred[,.(SiteRef,BGC)])
  SSPred <- na.omit(SSPred)
  setkey(SSPred,SS.pred)
  setkey(suit,SS_NoSpace)
  suitMerge <- suit[SSPred, allow.cartesian = T]
  suitMerge <- na.omit(suitMerge)
  if(nrow(suitMerge) < 3){
    return(NULL)
  }
  setnames(suitMerge, old = c("SS_NoSpace", "i.SS_NoSpace"), new = c("SS.pred", "SS_NoSpace"))
  suitVotes <- data.table::dcast(suitMerge, SiteRef + Spp + FuturePeriod + SS_NoSpace ~ Feasible, 
                                 value.var = "SSprob", fun.aggregate = sum)
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
  setorder(suitVotes,SiteRef,SS_NoSpace,Spp,FuturePeriod)
  suitVotes[Curr > 3.5, Curr := 4]
  colNms <- c("1","2","3","X")
  suitVotes <- suitVotes[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  suitVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  suitRes <- suitVotes[,.(Curr = mean(Curr),NewSuit = mean(NewSuit)), by = .(SiteRef,SS_NoSpace,FuturePeriod,Spp)]
  return(suitRes)
}


S1 <- setDT(dbGetQuery(sppDb,"select bgc,ss_nospace,spp,newfeas from feasorig"))
setnames(S1,c("BGC","SS_NoSpace","Spp","Feasible"))

gcm_weight <- data.table(gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
                                 "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
                                 "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                         weight = c(1,1,0,1,1,1,1,0,1,1,1,1,0))

rcp_weight <- data.table(rcp = c("ssp126","ssp245","ssp370","ssp585"), 
                         weight = c(0.8,1,0.8,0))

all_weight <- as.data.table(expand.grid(gcm = gcm_weight$gcm,rcp = rcp_weight$rcp))
all_weight[gcm_weight,wgcm := i.weight, on = "gcm"]
all_weight[rcp_weight,wrcp := i.weight, on = "rcp"]
all_weight[,weight := wgcm*wrcp]
all_weight[,comb := paste0("('",gcm,"','",rcp,"',",weight,")")]
weights <- paste(all_weight$comb,collapse = ",")

runCCISS <- function(con, bgcSelect, weights, spp_select){
  cciss_sql <- paste0("
  WITH test_points AS (
    SELECT * 
    FROM preselected_points
    WHERE bgc IN ('",paste(bgcSelect,collapse = "','") ,"')
  ),
  cciss AS (
    SELECT cciss_future12_array.siteno,
         test_points.dist_code,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         test_points.bgc,
         bgc.bgc bgc_pred,
         w.weight
  FROM cciss_future12_array
  JOIN test_points
    ON (cciss_future12_array.siteno = test_points.siteno),
       unnest(bgc_pred_id) WITH ordinality as source(bgc_pred_id, row_idx)
  JOIN (SELECT ROW_NUMBER() OVER(ORDER BY gcm_id, scenario_id, futureperiod_id) row_idx,
               gcm,
               scenario,
               futureperiod
        FROM gcm 
        CROSS JOIN scenario
        CROSS JOIN futureperiod) labels
    ON labels.row_idx = source.row_idx
    JOIN (values ",weights,") 
    AS w(gcm,scenario,weight)
    ON labels.gcm = w.gcm AND labels.scenario = w.scenario
  JOIN bgc
    ON bgc.bgc_id = source.bgc_pred_id
  WHERE futureperiod IN ('2021','2041','2061','2081')
  
  ), cciss_count_den AS (
  
    SELECT bgc, dist_code,
           futureperiod,
           SUM(weight) w
    FROM cciss
    GROUP BY bgc,dist_code, futureperiod
  
  ), cciss_count_num AS (
  
    SELECT bgc, 
           dist_code,
           futureperiod,
           bgc_pred,
           SUM(weight) w
    FROM cciss
    GROUP BY bgc, dist_code, futureperiod, bgc_pred
  
  ), cciss_curr AS (
      SELECT cciss_prob12.siteno,
      period,
      test_points.bgc,
      test_points.dist_code,
      bgc_pred,
      prob
      FROM cciss_prob12
      JOIN test_points
      ON (cciss_prob12.siteno = test_points.siteno)
      
  ), curr_temp AS (
    SELECT bgc, dist_code,
           COUNT(distinct siteno) n
    FROM cciss_curr
    GROUP BY bgc, dist_code
  )
  
  SELECT a.bgc, a.dist_code,
         a.futureperiod,
         a.bgc_pred,
         a.w/cast(b.w as float) bgc_prop
  FROM cciss_count_num a
  JOIN cciss_count_den b
    ON (a.bgc = b.bgc AND a.dist_code = b.dist_code AND a.futureperiod = b.futureperiod)
   WHERE a.w <> 0
  
  UNION ALL

  SELECT a.bgc,a.dist_code,
          period as futureperiod,
          bgc_pred,
          SUM(prob)/b.n bgc_prop
  FROM cciss_curr a
  JOIN curr_temp b
    ON (a.bgc = b.bgc AND a.dist_code = b.dist_code)
  GROUP BY a.bgc, a.dist_code,period,b.n,bgc_pred
  
  UNION ALL

  SELECT DISTINCT 
            test_points.bgc, test_points.dist_code,
            '1961' as futureperiod,
            cciss_curr.bgc as bgc_pred,
            cast(1 as numeric) bgc_prop
    FROM cciss_curr
    JOIN test_points
      ON (cciss_curr.siteno = test_points.siteno);
  ")
  
  dat <- setDT(dbGetQuery(con, cciss_sql))
  if(nrow(dat) < 5){
    return(NULL)
  }else{
    setorder(dat, bgc, dist_code, futureperiod, bgc_pred)
    dat[,SiteRef := paste0(bgc,"_",dist_code)]
    setnames(dat, new = c("FuturePeriod","BGC","BGC.pred","BGC.prop"),
             old = c("futureperiod","bgc","bgc_pred","bgc_prop"))
    
    dat <- dat[,.(SiteRef,FuturePeriod,BGC,BGC.pred,BGC.prop)]
    SSPreds <- edatopicOverlap(dat, E1, E1_Phase, onlyRegular = T)
    
    cciss_feas <- cciss_feasibility(SSPreds, S1, spp_select = spp_select)
    return(cciss_feas)
  }
  
}

bgcOpts <- dbGetQuery(pool, "select distinct bgc from preselected_points")[,1]
library(foreach)
out <- foreach(bgc = bgcOpts, .combine = rbind) %do% {
  cat("Processing",bgc,"\n")
  dat1 <- runCCISS(pool,c(bgc),weights,c("Py","Pl","Sx"))
  dat1
}



##setnames(dat, c("SiteRef","FuturePeriod","BGC","BGC.pred","BGC.prop"))
##test <- dat[,.(Tot = sum(bgc_prop)), by = .(bgc,dist_code,futureperiod)]

