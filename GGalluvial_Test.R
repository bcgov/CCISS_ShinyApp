library(networkD3)
library(shiny)
library(ccissdev)
library(data.table)
library(pool)
library(RPostgres)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)
bgc <- dbGetCCISS(pool,siteno = 1958596, avg = T, modWeights = all_weight)
bgcDat <- dbGetQuery(pool,"select * from cciss_future12 where siteno = 1958596")
setDT(bgcDat)
dat2 <- dcast(bgcDat,siteno + bgc + gcm + scenario ~ futureperiod, value.var = "bgc_pred")
tp1 <- dat2[,.(N1 = .N), by = .(`2021-2040`,`2041-2060`)]
setnames(tp1,c("source","target","value"))
tp1[,`:=`(source = paste0(source,"_0"),target = paste0(target,"_1"))]
tp2 <- dat2[,.(N1 = .N), by = .(`2041-2060`,`2061-2080`)]
setnames(tp2,c("source","target","value"))
tp2[,`:=`(source = paste0(source,"_1"),target = paste0(target,"_2"))]
tp3 <- dat2[,.(N1 = .N), by = .(`2061-2080`,`2081-2100`)]
setnames(tp3,c("source","target","value"))
tp3[,`:=`(source = paste0(source,"_2"),target = paste0(target,"_3"))]

allDat <- rbind(tp1,tp2,tp3)
allDat <- allDat[value > 1,]
bgcNames <- unique(c(allDat$source,allDat$target))
name_cw <- data.table(BGC = bgcNames, Code = 0:(length(bgcNames)-1))
allDat[name_cw,source_code := i.Code, on = c(source = "BGC")]
allDat[name_cw,target_code := i.Code, on = c(target = "BGC")]

library(plotly)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = unique(c(tp1$source,tp2$source,tp3$source,tp3$target)),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = allDat$source_code,
    target = allDat$target_code,
    value =  allDat$value
  )
)

fig

library(networkD3)
setnames(name_cw,c("name","node"))
sankeyNetwork(Links = allDat,
              Nodes = name_cw,
              Source = 'source_code',
              Target = 'target_code',
              Value = 'value',
              NodeID = 'name',
              sinksRight = F)


library(data.tree)
tp1 <- dat2[,.(Freq = .N), by = .(`2021-2040`,`2041-2060`,`2061-2080`,`2081-2100`)]
tp1 <- tp1[Freq > 1,]
tp1[,pathString := paste("SBSdk",`2021-2040`,`2041-2060`,`2061-2080`,`2081-2100`,sep = "/")]
tNode <- as.Node(tp1)
library(collapsibleTree)
collapsibleTree(tNode)

library(alluvial)

allTest <- dat2[,.(Freq = .N), by = .(`2021-2040`,`2041-2060`,`2061-2080`,`2081-2100`)]
allTest <- allTest[Freq > 1,]
alluvial(allTest[,!"Freq"],freq = allTest$Freq)

library(ggalluvial)
ggplot(tp1,aes(y = Freq, axis1 = `2021-2040`, axis2 = `2041-2060`, 
               axis3 = `2061-2080`,axis4 = `2081-2100`)) +
  geom_alluvium(aes(fill = `2021-2040`)) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))
