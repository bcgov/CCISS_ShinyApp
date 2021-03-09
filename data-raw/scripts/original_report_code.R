con <- RPostgreSQL::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "smithersresearch.ca",
  dbname = "cciss_data",
  port = 5432, 
  user = "postgres",
  password = "Kiriliny41"
)

for (site in unique(BGC$SiteNo)) {
  for (bgc in unique(BGC[SiteNo == site]$BGC)) {
    plot_ly(data = BGC[BGC == bgc & SiteNo == site], x = ~FuturePeriod,
            y = ~BGC.prop, split = ~BGC.pred, type = 'bar') %>%
      layout(yaxis = list(title = ""), barmode = 'stack') %>%
      print
  }
}

loc_name <- "WilliamsLake" ## name of trial run
pointDat <- fread("./data-raw/test_locations/WilliamsLake_Locations.csv")
test_siteno <- dbGetHexID(con, pointDat[,.(Long, Lat)]) ## returns hex number from location for testing
tic()
# test_siteno <- dbGetHexID(pointDat, host = "FLNRServer")
toc()
ID1_testsite <- cbind(pointDat, test_siteno) %>% dplyr::rename(SiteNo = test_siteno)
# test_siteno <- c(3084611 ,3088043 ,3088100 ,3093240 ,3096701 ,3103527 ,3103529 ,3103530 ,3105299 ,3115525) #hex numbers
average <- FALSE
BGC <- dbGetCCISS(con, test_siteno, avg = average) 
#BGC <- BGC %>% filter (BGC == "IDFxh2")

SSPreds <- edatopicOverlap(BGC,Edatope = E1)

cciss_res <- ccissOutput(SSPred = SSPreds, suit = S1, rules = R1, feasFlag = F1)
CCISS_Summary <- as.data.frame (cciss_res$Summary)
CCISS_Summary <- left_join(CCISS_Summary,ID1_testsite) %>% dplyr::select(ID1, everything())#%>% filter(!ID1 == "test")
CCISS_Sites <- CCISS_Summary %>% dplyr::select(ID1, Lat, Long)
CCISS_Raw <- as.data.frame (cciss_res$Raw)
CCISS_Raw  <- left_join(CCISS_Raw ,ID1_testsite) %>% dplyr::select(-ID2, -ModAgree, - SuitDiff, -Lat, -Long, -Elev) %>% 
  dplyr::select(ID1, everything()) %>% 
  filter(!ID1 == "test") %>% mutate(across(NewSuit, round, 0)) %>% mutate(across(c(`1`, `2`, `3`, X), round, 2))


