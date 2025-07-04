library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(ccissr)

server <- analogsea::droplets()$`shiny-server`
reset_ssh_sessions()
droplet_ssh(server, "rm -R /srv/shiny-server/cciss/server")
#analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/ccissr/.Renviron")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss/instructions")
analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss/server/points.R")
analogsea::droplet_upload(server, "./app/server/points.R", "/srv/shiny-server/cciss/server/")

analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/cciss")
#analogsea::droplet_ssh(server, "R -e \"remotes::install_github('bcgov/ccissr', upgrade = FALSE)\"")
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
