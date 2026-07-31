library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(ccissr)

server <- analogsea::droplets()$`shiny-server`
reset_ssh_sessions()
droplet_ssh(server, "rm -R /srv/shiny-server/cciss/server")
#analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/ccissr/.Renviron")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss/instructions")
analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss/server/generate.R")
analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss/server/")

analogsea::droplet_upload(server, "./app/server/generate.R", "/srv/shiny-server/cciss/server/")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/cciss")
#analogsea::droplet_ssh(server, "R -e \"remotes::install_github('bcgov/ccissr@development', upgrade = FALSE)\"")
analogsea::droplet_upload(server, "./app/global.R", "/srv/shiny-server/cciss/global.R")
analogsea::droplet_upload(server, "./app/server.R", "/srv/shiny-server/cciss/server.R")
analogsea::droplet_upload(server, "./app/ui.R", "/srv/shiny-server/cciss/ui.R")
analogsea::droplet_upload(server, "./app/www", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/server", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/cciss_spatial", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/instructions", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/WNA_SZ_Cols_v13_6.csv", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, c("./app/cciss_metadata.csv","./app/README.txt","./app/fonts","./app/lib","./app/CCISS_Version_Info.csv"), "/srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")


analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/ccissdev/instructions")
analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/ccissdev/ui.R")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/ccissdev")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/ccissdev")
#analogsea::droplet_ssh(server, "R -e \"remotes::install_github('bcgov/ccissdevr@development', upgrade = FALSE)\"")
analogsea::droplet_upload(server, "./app/global.R", "/srv/shiny-server/ccissdev/global.R")
analogsea::droplet_upload(server, "./app/server.R", "/srv/shiny-server/ccissdev/server.R")
analogsea::droplet_upload(server, "./app/ui.R", "/srv/shiny-server/ccissdev/ui.R")
analogsea::droplet_upload(server, "./app/www", "/srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, "./app/server", "/srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, "./app/cciss_spatial", "/srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, "./app/instructions", "/srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, "./app/WNA_SZ_Cols_v13_6.csv", "/srv/shiny-server/ccissdev")
analogsea::droplet_upload(server, c("./app/cciss_metadata.csv","./app/README.txt","./app/fonts","./app/lib","./app/CCISS_Version_Info.csv"), "/srv/shiny-server/ccissdev")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")


library(data.table)
dat <- fread("WNA_BGCs_Info.csv")
bgcs <- dat[DataSet == "BC",BGC]
dput(sort(bgcs))

library(ccissr)
library(RPostgres)
library(pool)
dbCon <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "cciss_spatial",
  host = "178.128.233.227",
  port = 5432, 
  user = "cciss_user",
  password = "powerofbec"
)
dat <- dbGetQuery(dbCon, glue_sql("select ssp, gcm, run, period, bgc_pred, persistance, expansion from bgc_per_exp 
                                      where region = 'DCC' 
                                      and period = '2001_2020_obs'", .con = dbCon))
names(dat) <- c("ssp", "gcm", "run", "period", "bgc_pred", "Persistance", "Expansion")
setDT(dat)

period_sel <- "2001_2020_obs"
bgc_bubbleplot(dat, period = period_sel, scenario = "ssp245")

dat <- fread("app/test_alluvial_data.csv")
plot_alluvial(dat, spp = "Py", edatope = "C4")

for f in bgc_Ensemble*.mbtiles; do
sqlite3 "$f" "
  insert or replace into metadata(name,value) values
  ('minzoom','5'),
  ('maxzoom','12'),
  ('center','-126.526528,54.154306,6');
  "
done
