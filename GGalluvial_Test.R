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
tp1 <- dat2[,.(Freq = .N), by = .(`2021-2040`,`2041-2060`,`2061-2080`,`2081-2100`)]
tp1 <- dat2[,.(N1 = .N), by = .(`2021-2040`,`2041-2060`)]

tp1 <- dat2[,.(Freq = .N), by = .(`2021-2040`,`2041-2060`,`2061-2080`,`2081-2100`)]
tp1 <- tp1[Freq > 1,]
library(ggalluvial)
ggplot(tp1,aes(y = Freq, axis1 = `2021-2040`, axis2 = `2041-2060`, 
               axis3 = `2061-2080`,axis4 = `2081-2100`)) +
  geom_alluvium(aes(fill = `2021-2040`)) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))
