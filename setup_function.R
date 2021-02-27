setup_data_function <- function(){
  
  cloud_dir <- ("./data_tables/")
  # if(dir.exists("E:/Sync/CCISS_data/")){
  #   cloud_dir <- "E:/Sync/CCISS_data/"
  # }else{
  #   cloud_dir <- "C:/Users/kirid/Desktop/Work2021/"
  # }
  E1 <- fread(paste0(cloud_dir, "Edatopic_v11_22.csv")) 
  S1 <- fread(paste0(cloud_dir,"Feasibility_v11_22.csv"))
  
  R1 <- fread(paste0(cloud_dir,"RuleTable.csv"))
  F1 <- fread(paste0(cloud_dir,"FeasibilityLabels.csv"))
  
  ### 
  ##load csv with lat and long columns
  loc_name <- "WilliamsLake" ## name of trial run
  pointDat <- fread("./test_locations/WilliamsLake_Locations.csv")
  test_siteno <- dbGetHexID(pointDat,host = "smithersresearch.ca") ## returns hex number from location for testing
  tic()
  # test_siteno <- dbGetHexID(pointDat, host = "FLNRServer")
  toc()
  ID1_testsite <- cbind(pointDat, test_siteno) %>% dplyr::rename(SiteNo = test_siteno)
  # test_siteno <- c(3084611 ,3088043 ,3088100 ,3093240 ,3096701 ,3103527 ,3103529 ,3103530 ,3105299 ,3115525) #hex numbers
  average <- FALSE
  BGC <- dbGetCCISS(test_siteno, avg = average, host = "smithersresearch.ca") 
  #BGC <- BGC %>% filter (BGC == "IDFxh2")
  
  # browser()
  SSPreds_edatopic <- edatopicOverlap(BGC,Edatope = E1)
  
  
  cciss_res <- ccissOutput(SSPreds = SSPreds_edatopic, suit = S1, rules = R1, feasFlag = F1)
  CCISS_Summary <- as.data.frame (cciss_res$Summary)
  CCISS_Summary <- left_join(CCISS_Summary,ID1_testsite) %>% dplyr::select(ID1, everything())#%>% filter(!ID1 == "test")
  CCISS_Sites <- CCISS_Summary %>% dplyr::select(ID1, Lat, Long)
  
  CCISS_Raw <- as.data.frame (cciss_res$Raw)
  CCISS_Raw  <- left_join(CCISS_Raw ,ID1_testsite) %>% dplyr::select(-ID2, -ModAgree, - SuitDiff, -Lat, -Long, -Elev) %>% 
    dplyr::select(ID1, everything()) %>% 
    filter(!ID1 == "test") %>% mutate(across(NewSuit, round, 0)) %>% mutate(across(c(`1`, `2`, `3`, X), round, 2))

  
  output <- list(
    "BGC" = BGC,
    "CCISS_Summary" = CCISS_Summary,
    "CCISS_Sites" = CCISS_Sites,
    "CCISS_Raw" = CCISS_Raw
  )
  
  return(output)
}