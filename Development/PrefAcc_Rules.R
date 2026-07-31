##test pref/accept ruleset
library(data.table)
library(ccissr)
cfrg_rules <- data("cfrg_rules")
cfrg_rules <- melt(cfrg_rules,id.vars = "Spp",variable.name = "Feasible",value.name = "PrefAcc")
cfrg_rules[,Feasible := as.integer(gsub("E","",Feasible))]
suit <- copy(S1)
suit <- suit[Spp %chin% unique(cfrg_rules$Spp),]
suit[cfrg_rules,PrefAcc := i.PrefAcc, on = c("Spp","Feasible")]
suit[is.na(PrefAcc),PrefAcc := "X"]
suit[,NoPref := if(any(PrefAcc == "P")) T else F, by = .(BGC,SS_NoSpace)]
suit[NoPref == F & PrefAcc == "A", PrefAcc := "P"]

ss <- stocking_standards
ss2 <- dcast(ss, SS_NoSpace ~ Region, value.var = "Suitability")
