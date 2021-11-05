edatopicOverlap <- function(BGC,edaBlobs,E1,E1_Phase){
  
  SS <- E1[,list(BGC,SS_NoSpace,Edatopic)]
  edaPhase <- E1_Phase
  SS <- unique(SS)
  BGC <- unique(BGC)
  
  ##regular site series edatopes
  allUnits <- unique(BGC$BGC)
  allBlobs <- CJ(allUnits,edaBlobs) #cross join
  setname(allBlobs,old = "SS_NoSpace", new = "Blob")
  temp <- rbind(SS,E1_Phase[is.na(SpecialCode),list(BGC,SS_NoSpace,Edatopic)])
  CurrBGC <- allBlobs[BGC, on = "BGC", allow.cartesian = T]
  CurrBGC <- CurrBGC[!duplicated(CurrBGC),]
  setkey(BGC, BGC.pred)
  setkey(SS, BGC)
  FutBGC <- SS[BGC, allow.cartesian = T]
  FutBGC <- FutBGC[!duplicated(FutBGC),] 
  setnames(FutBGC, old = c("BGC","SS_NoSpace","i.BGC"), 
           new = c("BGC.pred","SS.pred","BGC"))
  FutBGC <- na.omit(FutBGC)
  
  setkey(FutBGC, SiteRef, FuturePeriod, BGC,BGC.pred, Edatopic)
  FutBGC[,BGC.prop := NULL]
  
  setkey(CurrBGC,SiteRef,FuturePeriod, BGC,BGC.pred, Edatopic)
  new <- FutBGC[CurrBGC, allow.cartesian = T]
  new <- new[!is.na(SS.pred),]
  setkey(new, SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  ##new <- new[complete.cases(new),]
  
  numEda <- E1[,list(NumEdas = .N), by = list(BGC,SS_NoSpace)]
  
  ###forwards overlap
  SS.out <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)]
  SS.out[numEda,SS.Curr := i.NumEdas, on = c(SS_NoSpace = "SS_NoSpace"), allow.cartesian = T]
  SS.out[,SSProb := SS.prob/SS.Curr]
  
  ###reverse overlap
  SS.out.rev <- new[,list(SS.prob = .N,BGC.prop = BGC.prop[1]), 
                    keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS.pred,SS_NoSpace)]
  SS.out.rev[numEda,SS.Curr := i.NumEdas, on = c(SS.pred = "SS_NoSpace")]
  SS.out.rev[,SSProbRev := SS.prob/SS.Curr]
  
  ##combine them
  combAll <- merge(SS.out,SS.out.rev,by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"))
  combAll[,allOverlap := SSProb*SSProbRev]
  setnames(combAll, old = "BGC.prop.x",new = "BGC.prop")
  combAll <- combAll[,list(SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace, 
                           allOverlap, SS.pred, BGC.prop)]
  combAll <- na.omit(combAll)
  ###########################################################################
  ##now redo for phases
  
  numEdaPh <- E1_Phase[,list(NumEdas = .N), by = list(SS_NoSpace)]
  phaseSmall <- unique(edaPhase[,list(BGC,MainUnit,Phase = SS_NoSpace)])
  combPhase <- phaseSmall[combAll, on = c(MainUnit = "SS.pred"), allow.cartesian = T]
  justPhase <- combPhase[!is.na(Phase),]
  curr <- unique(justPhase[,list(SiteRef, FuturePeriod, BGC = i.BGC, BGC.pred, SS_NoSpace)])
  fut <- unique(justPhase[,list(SiteRef, FuturePeriod, BGC = i.BGC, BGC.pred, MainUnit, Phase)])
  phaseTemp <- E1_Phase[,list(SS_NoSpace,Edatopic)]
  curr <- E1[curr, on = "SS_NoSpace", allow.cartesian = T] 
  fut <- phaseTemp[fut, on = c(SS_NoSpace = "Phase"),allow.cartesian = T]
  setnames(fut,old = "SS_NoSpace", new = "PhasePred")
  setkey(curr, SiteRef, FuturePeriod, BGC, BGC.pred,Edatopic)
  setkey(fut,SiteRef,FuturePeriod,BGC,BGC.pred,Edatopic)
  new <- fut[curr, allow.cartesian = T]
  new <- new[!is.na(PhasePred),]
  
  ###forwards overlap
  SS.out <- new[,list(SS.prob = .N,MainUnit = MainUnit[1]), 
                keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,PhasePred)]
  SS.out[numEda,SS.Curr := i.NumEdas, on = c(SS_NoSpace = "SS_NoSpace"), allow.cartesian = T]
  SS.out[,SSProb := SS.prob/SS.Curr]
  
  ###reverse overlap
  SS.out.rev <- new[,list(SS.prob = .N,MainUnit = MainUnit[1]), 
                    keyby = list(SiteRef,FuturePeriod,BGC,BGC.pred,PhasePred,SS_NoSpace)]
  SS.out.rev[numEdaPh,SS.Curr := i.NumEdas, on = c(PhasePred = "SS_NoSpace"), allow.cartesian = T]
  SS.out.rev[,SSProbRev := SS.prob/SS.Curr]
  
  ##combine them
  combPhaseAll <- merge(SS.out,SS.out.rev,by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","PhasePred"))
  combPhaseAll[,allOverlap := SSProb*SSProbRev]
  combPhaseAll <- combPhaseAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,
                                     SS.pred = MainUnit.y, PhasePred,phaseOverlap = allOverlap)]
  combPhaseAll <- na.omit(combPhaseAll)
  setkey(combPhaseAll,SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  setkey(combAll,SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)
  combAll <- combPhaseAll[combAll]
  ####add phase to non-phase###
  combAll[,PhaseSum := sum(phaseOverlap), by = list(SiteRef,FuturePeriod,BGC,BGC.pred,SS_NoSpace,SS.pred)]
  combAll[,phaseOverlap := phaseOverlap/PhaseSum]
  combAll[!is.na(phaseOverlap),allOverlap := allOverlap * phaseOverlap]
  combAll[!is.na(phaseOverlap),SS.pred := PhasePred]
  combAll[,c("phaseOverlap","PhaseSum","PhasePred") := NULL]
  ###done phases
  
  combAll <- merge(combAll,SSsp.out,
                   by = c("SiteRef","FuturePeriod","BGC","BGC.pred","SS_NoSpace","SS.pred"), all = T)
  temp <- combAll[!is.na(allOverlap.y),]
  temp[,c("allOverlap.x","BGC.prop.x") := NULL]
  setnames(temp,old = c("allOverlap.y","BGC.prop.y"), new = c("allOverlap","BGC.prop"))
  combAll[,Flag := if(all(is.na(allOverlap.y))) T else F, by = list(SiteRef,FuturePeriod,BGC,SS_NoSpace,BGC.pred)]
  combAll <- combAll[(Flag),!"Flag"]
  combAll[,c("allOverlap.y","BGC.prop.y") := NULL]
  setnames(combAll,old = c("allOverlap.x","BGC.prop.x"), new = c("allOverlap","BGC.prop"))
  combAll <- rbind(combAll,temp)
  combAll[,MainUnit := gsub("[a-c]$|\\.[1-9]$","",SS.pred)]
  combAll <- combAll[!(BGC == BGC.pred  &  SS_NoSpace != MainUnit),] ### removes overlap where past BGC = future BGC
  combAll[,MainUnit := NULL]
  combAll <- unique(combAll[!is.na(SS_NoSpace),])
  
  
  ##add in BGC probability
  combAll <- na.omit(combAll)
  combAll[,SSratio := allOverlap/sum(allOverlap), by = list(SiteRef, FuturePeriod, BGC, BGC.pred,SS_NoSpace)] ##should check this?
  setorder(combAll, SiteRef, FuturePeriod, BGC, BGC.pred, SS_NoSpace)
  
  setkey(combAll, SiteRef, FuturePeriod, BGC,BGC.pred)
  temp <- unique(combAll[,list(SiteRef,FuturePeriod,BGC,BGC.pred,BGC.prop)])
  temp[,BGC.prop := BGC.prop/sum(BGC.prop), by = list(SiteRef,FuturePeriod,BGC)]
  temp <- unique(temp)
  combAll[,BGC.prop := NULL]
  combAll <- temp[combAll]
  combAll[,SSprob := SSratio*BGC.prop]
  combAll <- combAll[!duplicated(combAll),]
  print("Done EDA")
  return(combAll)
}
