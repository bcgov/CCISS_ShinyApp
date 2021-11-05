edatopicOverlap <- function(BGC,edaBlobs,E1,E1_Phase,S1,spp_select){
  
  suit <- S1[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  
  SS <- E1[,list(BGC,SS_NoSpace,Edatopic)]
  edaPhase <- E1_Phase
  SS <- unique(SS)
  BGC <- unique(BGC)
  
  ##regular site series edatopes
  allUnits <- unique(BGC$BGC)
  allBlobs <- CJ(BGC = allUnits, edatopic = edaBlobs$edatopic) #cross join
  allBlobs[edaBlobs,Blob := i.SS_NoSpace, on = "edatopic"]
  temp <- rbind(SS,E1_Phase[is.na(SpecialCode),list(BGC,SS_NoSpace,Edatopic)])
  bgcPred <- BGC[,.(SiteRef,BGC.pred,BGC.prop)]
  futBGC <- allBlobs[bgcPred, on = c(BGC = "BGC.pred"), allow.cartesian = T]
  gc()
  futBGC <- temp[futBGC, on = c("BGC", Edatopic = "edatopic"), allow.cartesian = T]
  futBGC <- na.omit(futBGC)
  futBGC <- futBGC[,.(NumInBlob = .N), by = .(SiteRef,BGC,Blob,SS_NoSpace)]
  totalNum <- temp[,.(NumEdas = .N), by = .(BGC,SS_NoSpace)]
  futBGC[totalNum, TotalNum := i.NumEdas, on = "SS_NoSpace"]
  futBGC[,FutOverlap := NumInBlob/TotalNum]
  futBGC[,FutOverlap := FutOverlap/sum(FutOverlap), by = .(SiteRef,BGC,Blob)]
  futBGC[bgcPred,BGC.prop := i.BGC.prop, on = c("SiteRef",BGC = "BGC.pred")]
  futBGC[,FutureProp := FutOverlap*BGC.prop]
  futBGC[,c("NumInBlob","TotalNum","FutOverlap","BGC.prop") := NULL]
  futBGC[suit,Feasible := i.Feasible, on = "SS_NoSpace"]
  futBGC[,AnySuit := if(any(!is.na(Feasible))) T else F, by = .(SiteRef,Blob)]
  futBGC <- futBGC[(AnySuit),]
  futBGC[is.na(Feasible), Feasible := 4]
  futBGC[Feasible > 3.5, Feasible := 4]
  futVotes <- data.table::dcast(futBGC, SiteRef + Blob ~ Feasible, 
                                             value.var = "FutureProp", fun.aggregate = sum)
  futVotes[,VoteSum := `1`+`2`+`3`+`4`]
  futVotes[,X := 1 - VoteSum]
  futVotes[,VoteSum := NULL]
  futVotes[,X := X + `4`]
  futVotes[,`:=`(`4` = NULL)]
  futVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  
}
