edatopicOverlap <- function(BGC,edaBlobs,E1,E1_Phase,S1,spp_select){
  
  suit <- S1[Spp == spp_select,.(BGC,SS_NoSpace,Spp,Feasible)]
  suit <- unique(suit)
  suit <- na.omit(suit)
  
  SS <- E1[,list(BGC,SS_NoSpace,Edatopic)]
  edaPhase <- E1_Phase
  SS <- unique(SS)
  BGC <- unique(BGC)
  bgcLookup <- BGC[,.(SiteRef,BGC)]
  
  ##regular site series edatopes
  allUnits <- unique(BGC$BGC)
  allBlobs <- CJ(BGC = allUnits, edatopic = edaBlobs$edatopic) #cross join
  allBlobs[edaBlobs,Blob := i.SS_NoSpace, on = "edatopic"]
  temp <- rbind(SS,E1_Phase[is.na(SpecialCode),list(BGC,SS_NoSpace,Edatopic)])
  bgcPred <- BGC[,.(SiteRef,BGC.pred,BGC.prop)]
  bgcCurr <- BGC[,.(SiteRef,BGC)]
  
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
  futVotes <- futVotes[,.(SiteRef,Blob,NewSuit)]
  setnames(futVotes,old = "NewSuit", new = "FutureFeas")
  
  ###curr period
  currBGC <- allBlobs[bgcCurr, on = c("BGC"), allow.cartesian = T]
  gc()
  currBGC <- temp[currBGC, on = c("BGC", Edatopic = "edatopic"), allow.cartesian = T]
  currBGC <- na.omit(currBGC)
  currBGC <- unique(currBGC)
  currBGC <- currBGC[,.(NumInBlob = .N), by = .(SiteRef,BGC,Blob,SS_NoSpace)]
  currBGC[totalNum, TotalNum := i.NumEdas, on = "SS_NoSpace"]
  currBGC[,CurrOverlap := NumInBlob/TotalNum]
  currBGC[,CurrProp := CurrOverlap/sum(CurrOverlap), by = .(SiteRef,BGC,Blob)]
  currBGC[,c("NumInBlob","TotalNum","CurrOverlap") := NULL]
  currBGC[suit,Feasible := i.Feasible, on = "SS_NoSpace"]
  currBGC[,AnySuit := if(any(!is.na(Feasible))) T else F, by = .(SiteRef,Blob)]
  currBGC <- currBGC[(AnySuit),]
  currBGC[is.na(Feasible), Feasible := 4]
  currBGC[Feasible > 3.5, Feasible := 4]
  currVotes <- data.table::dcast(currBGC, SiteRef + BGC + Blob ~ Feasible, 
                                value.var = "CurrProp", fun.aggregate = sum)
  
  currVotes[,VoteSum := `1`+`2`+`3`+`4`]
  currVotes[,X := 1 - VoteSum]
  currVotes[,VoteSum := NULL]
  currVotes[,X := X + `4`]
  currVotes[,`:=`(`4` = NULL)]
  currVotes[,NewSuit := `1`+(`2`*2)+(`3`*3)+(X*5)]
  
  currVotes <- currVotes[,.(SiteRef,BGC,Blob,NewSuit)]
  setnames(currVotes,old = "NewSuit", new = "CurrentFeas")
  
  res <- merge(currVotes,futVotes, by = c("SiteRef","Blob"), all = T)
  res[,BGC := NULL]
  res[bgcLookup,BGC := i.BGC, on = "SiteRef"]
  res[is.na(CurrentFeas) | CurrentFeas > 3.5, CurrentFeas := 5]
  res[is.na(FutureFeas) | FutureFeas > 3.5, FutureFeas := 5]
  setorder(res,SiteRef,Blob)
  return(res)
}
