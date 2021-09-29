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
ccissOutput <- function(SSPred,suit,rules,feasFlag,histWeights,futureWeights){
 ### generate raw feasibility ratios
  ccissWt <- data.table(FuturePeriod = c(2021,2041,2061,2081),
                        Weight = futureWeights)
  
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

  ##Generate summary feasibility from raw proportions
  histWt <- histWeights[1]
  currWt <- histWeights[2]    ## weight in summary given to the modern climate change period
  earlyWt <- histWeights[3]   ## weight in summary given to the 2010-2040 climate change period

  colNms <- c("1","2","3","X")
  datFeas <- suitVotes[FuturePeriod %in% c(1961 ,1991, 2021),]

  datFeas[FuturePeriod == 1961, (colNms) := lapply(.SD,"*",histWt), .SDcols = colNms]
  datFeas[FuturePeriod == 1991, (colNms) := lapply(.SD,"*",currWt), .SDcols = colNms]
  datFeas[FuturePeriod == 2021, (colNms) := lapply(.SD,"*",earlyWt), .SDcols = colNms]

  datFeas <- datFeas[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
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
  
  datRot <- datRot[,lapply(.SD, mean),.SDcols = c("Improve","Decline"), by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datRot[,`:=`(Improve = round(Improve*100),Decline = round(Decline*100))]
  #datRot[,Trend := paste0(Improve,":",Decline)]
  datRot <- datRot[,.(SiteRef,SS_NoSpace,Spp,Improve,Decline)] ##final
  
###################################################################
  datFuture <- suitVotes[FuturePeriod %in% c(2021,2041,2061,2081),]
  datFuture <- datFuture[,lapply(.SD, sum),.SDcols = colNms, 
                         by = .(SiteRef,FuturePeriod, SS_NoSpace,Spp,Curr)]
  datFuture[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  datFuture[ccissWt, Weight := i.Weight, on = "FuturePeriod"]
  datFuture <- datFuture[,.(ccissSuitFrac = weighted.mean(NewSuit,Weight)), 
                         by = .(SiteRef,SS_NoSpace,Spp,Curr)]
  datFuture[,ccissSuit := round(ccissSuitFrac)]
  
  ###merge data to make summary tables############################################################
  summOut <- merge(datFeas_final, datRot, by = c('SiteRef','SS_NoSpace','Spp'),all = T)
  summOut <- merge(summOut, datFuture, by = c('SiteRef','SS_NoSpace','Spp'), all = T)
  summOut[,OrderCol := (Curr + NewSuitFrac + ccissSuitFrac)/3]
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

#### use flag to update next section where species is added in current period
# datEarly <- suitVotes[FuturePeriod == 2021,]
# datEarly <- datEarly[,lapply(.SD, sum),.SDcols = colNms, by = .(SiteRef,SS_NoSpace,Spp,Curr)]
# datEarly[,Suit2025 := `1`+(`2`*2)+(`3`*3)+(X*4)]
# datEarly[,change2025 := Curr - Suit2025]
# 
# datEarly[,Trajectory2025 := fifelse(change2025 >=1.5, "Strongly Improving", 
#                                     fifelse(change2025>=.5 & change2025<1.5, "Improving",
#                                             fifelse(change2025 >= -.5 & change2025 <.5, "Stable",
#                                                     fifelse(change2025>= -1.5 & change2025 <= -.5, "Declining", "Strongly Declining"))))]
# 
# datEarly[,FailRisk2025 := fifelse(X>.5, "High", 
#                                   fifelse(X>.2 & X<.5, "Increased", "Normal"))]
# 
# datEarly <- datEarly[,.(SiteRef,SS_NoSpace,Spp, Suit2025,
#                         Trajectory2025, FailRisk2025)]