### CCISS 2020 Step 3: Join with feasibility table, create summary tables
### Kiri Daust. Heavily edited 28Jan2021 by W.MacKenzie
#SSPred = SSPreds; suit = S1; rules = R1; feasFlag = F1

# Copyright 2021 Province of British Columbia
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

# SSPred <- combAll
# suit <- fread("~/CommonTables/Feasibility_v12_3.csv")
# setnames(suit, old = "SppVar", new = "Spp")
# F1 <- suit
# save(F1,file = "./data/F1.rda")
#' ccissOutput
#' @param SSPred A data.table. Predictions input.
#' @param suit A data.table. Suitability.
#' @param rules A data.table. Set of rules.
#' @param feasFlag A data.table. Flag depending on feasibility differential.
#' @details What the function does
#' @return What the function returns
#' @importFrom matrixStats rowMaxs
#' @importFrom dplyr select everything mutate across full_join filter
#' @export
ccissOutput <- function(SSPred,suit,rules,feasFlag){
 ### generate raw feasibility ratios
  suit <- suit[,.(BGC,SS_NoSpace,Spp,Feasible)]
  ## replace the coast/interior divisions of species
  suit <- unique(suit)
  suit <- na.omit(suit)
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
  suitVotes[,SuitDiff := stepDiff(FuturePeriod,NewSuit,Curr), by = .(SiteRef,SS_NoSpace,Spp)] ## final raw output table
  #suitVotes2 <- suitVotes %>% dplyr::select(SiteRef, Spp, FuturePeriod, SS_NoSpace, Curr, NewSuit, dplyr::everything() )
  
  ##Generate summary feasibility from raw proportions
  histWt <- 0.3
  currWt <- 0.35    ## weight in summary given to the modern climate change period
  earlyWt <- 0.35   ## weight in summary given to the 2010-2040 climate change period

  colNms <- c("1","2","3","X")
  datFeas <- suitVotes[FuturePeriod %in% c(1961 ,1991, 2021),]
  #datFeas[FuturePeriod == 1975, (colNms) := lapply(.SD,"*",histWt), .SDcols = colNms]
  datFeas[FuturePeriod == 1991, (colNms) := lapply(.SD,"*",currWt), .SDcols = colNms]##Write c function
  datFeas[FuturePeriod == 2021, (colNms) := lapply(.SD,"*",earlyWt), .SDcols = colNms]

  datFeas <- datFeas[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datFeas2 <- datFeas
  ## ADD VARIABLE THAT IS 1-SUM OF 1-4 COLUMNS THAN ADD VALUE TO x COLUMN TO ACCOUNT FOR nULL FUTURES AND MAKE ROW SUM = 1
  datFeas2[,Xadj := rowSums(.SD), .SDcols = colNms]
  datFeas2[,X2 := X + (1-Xadj)]
  #datFeas2 <- datFeas2 %>% dplyr::mutate (Xadj2 = 1 - Xadj) %>% dplyr::mutate(X2 = X + Xadj2)# %>% select (-Xadj)
   ## THEN VARIABLE THAT IS SUM OF COL1 + COL2*2, COL3*3, COL 'X'* 4 and then round(0) for establishment feasibility
  datFeas2[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X2*4)]
  datFeas2[,NewSuit := round(NewSuit,0)]
  datFeas2[,`:=`(X = NULL,Xadj = NULL)]
  datFeas <- datFeas2
 
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
  datFeas_final <- datFeas %>% dplyr::select(SiteRef,SS_NoSpace,Spp,Curr,NewSuit, SuitDiff, Flag) %>% dplyr::mutate(dplyr::across(NewSuit, round, 0))%>%
           dplyr::mutate(NewSuit = sub('10', 'X', NewSuit)) 
  
  #### use flag to update next section where species is added in current period
  datEarly <- suitVotes[FuturePeriod == 2021,]
  
  datEarly <- datEarly[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datEarly2 <- datEarly
  datEarly2[,Suit2025 := `1`+(`2`*2)+(`3`*3)+(X*4)]
  datEarly2[,change2025 := Curr - Suit2025]
  datEarly2[,Suit2025 := round(Suit2025,0)]
  
  # datEarly2 <- datEarly2 %>% dplyr::mutate(Suit2025 = `1` +(`2` * 2 ) + (`3` * 3) + (X * 4)) %>% dplyr::mutate (change2025 = Curr- Suit2025) %>% 
  #   dplyr::mutate(dplyr::across(Suit2025, round, 0)) %>% dplyr::mutate(Suit2025 = sub('4', 'X', Suit2025))
  
  datEarly2[,Trajectory2025 := fifelse(change2025 >=1.5, "Strongly Improving", 
                                      fifelse(change2025>=.5 & change2025<1.5, "Improving",
                                             fifelse(change2025 >= -.5 & change2025 <.5, "Stable",
                                                    fifelse(change2025>= -1.5 & change2025 <= -.5, "Declining", "Strongly Declining"))))]

  datEarly2[,FailRisk2025 := fifelse(X>.5, "High", 
                                    fifelse(X>.2 & X<.5, "Increased", "Normal"))]
  
  datEarly3 <- datEarly2[,.(SiteRef,SS_NoSpace,Spp, Curr, Suit2025,
                            Trajectory2025, FailRisk2025)]
  
   ###mid rotation (2055) compare to historic (trending down or up - flag where becoming unsuitable)
  datMid <- suitVotes[FuturePeriod == 2041,]
  
  datMid <- datMid[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datMid2 <- datMid
  datMid2[,Suit2055 := `1`+(`2`*2)+(`3`*3)+(X*4)]
  datMid2[,change2055 := Curr - Suit2055]
  datMid2[,Suit2055 := round(Suit2055,0)]
  datMid2[,Trajectory2055 := fifelse(change2055 >=1.5, "Strongly Improving", 
                                    fifelse(change2055>=.5 & change2055<1.5, "Improving",
                                           fifelse(change2055 >= -.5 & change2055 <.5, "Stable",
                                                  fifelse(change2055>= -1.5 & change2055 <= -.5, "Declining", "Strongly Declining"))))]
  
  datMid2[,FailRisk2055 := fifelse(X>.5, "High", 
                                  fifelse(X>.2 & X<.5, "Increased", "Normal"))]
  datMid3 <- datMid2[,.(SiteRef,SS_NoSpace,Spp, Curr, Suit2055,
                        Trajectory2055, FailRisk2055)]
  
  ###end rotation (2085) compare to historic (trending down or up - flag where becoming unsuitable)
  datLong <- suitVotes[FuturePeriod == 2081,]
  
  datLong <- datLong[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datLong2 <- datLong
  datLong2[,Suit2085 := `1`+(`2`*2)+(`3`*3)+(X*4)]
  datLong2[,change2085 := Curr - Suit2085]
  datLong2[,Suit2085 := round(Suit2085,0)]
  
  datLong2[,Trajectory2085 := fifelse(change2085 >=1.5, "Strongly Improving", 
                                     fifelse(change2085>=.5 & change2085<1.5, "Improving",
                                             fifelse(change2085 >= -.5 & change2085 <.5, "Stable",
                                                     fifelse(change2085>= -1.5 & change2085 <= -.5, "Declining", "Strongly Declining"))))]
  
  datLong2[,FailRisk2085 := fifelse(X>.5, "High", 
                                   fifelse(X>.2 & X<.5, "Increased", "Normal"))]
  datLong3 <- datLong2[,.(SiteRef,SS_NoSpace,Spp,Curr, Suit2085, Trajectory2085, FailRisk2085)]
  
  summOut <- merge(datFeas_final, datEarly3, by = c('SiteRef','SS_NoSpace','Spp', 'Curr'),all = T)
  summOut <- merge(summOut, datMid3, by = c('SiteRef','SS_NoSpace','Spp', 'Curr'), all = T)
  summOut <- merge(summOut, datLong3, by = c('SiteRef','SS_NoSpace','Spp', 'Curr'), all = T)
  summOut <- summOut[Flag != "NotIn",!c("SuitDiff","Trajectory2025","Trajectory2055","Trajectory2085")]
  summOut[,`:=`(Suit2025 = as.character(Suit2025),
                Suit2055 = as.character(Suit2055),
                Suit2085 = as.character(Suit2085))]
  summOut[,c("Suit2025","Suit2055","Suit2085") := lapply(.SD,function(x){fifelse(x == 4,"X",x)}), 
          .SDcols = c("Suit2025","Suit2055","Suit2085") ]
  print("Done Feas")
  return(list(Summary = summOut, Raw = suitVotes))
}

### merge in rulesets
# ruleSub <- rules[Type == "PeriodTraj",]
# suitVotes[ruleSub, PeriodTraj := i.Label, on = .(SuitDiff < mx, SuitDiff >= mn)]
# ruleSub <- rules[Type == "Risk",]
# suitVotes[ruleSub, Risk := i.Label, on = .(X < mx, X >= mn)]
# ruleSub <- rules[Type == "ModAgr",]
# suitVotes[ruleSub, ModAgrClass := i.Label, on = .(ModAgree < mx, ModAgree >= mn)]
# 
# ruleSub <- rules[Type == "OverallTraj",]
# datMid[ruleSub, OverallTraj := i.Label, on = .(SuitDiff < mx, SuitDiff >= mn)]
# datMid[Bifurc == T, OverallTraj := "Bifurcating"]
# 
# ruleSub <- rules[Type == "EstabRisk",]
# datFeas[ruleSub, Estab.Risk := i.Label, on = .(FeasEstab < mx, FeasEstab >= mn)]
# 
# temp1 <- datFeas[,.(SiteRef,Spp,SS_NoSpace,Curr,NewSuit,Flag,Estab.Risk)]
# temp2 <- datMid[,.(SiteRef,Spp,SS_NoSpace,OverallTraj,ModAgree,Improve,Stable,Decline)]
# summOut <- merge(temp1,temp2, by = c("SiteRef","Spp","SS_NoSpace"), all = T)
# summOut <- summOut[complete.cases(summOut),]
# summOut[NewSuit > 3.5,NewSuit := 4]
# summOut[Curr > 3.5,Curr := 4]
# 
# suitVotes[,`:=`(ModAgree = NULL,NewSuit = NULL,SuitDiff = NULL)]