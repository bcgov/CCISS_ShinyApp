# Build a VM with postgis, shiny server and r/rstudio on Digital Ocean

# remotes::install_github("sckott/analogsea")
library(analogsea)
Sys.setenv(DO_PAT="eae4166ed2fac0e3c41660fe26a009bb0176ab8bceeaf753faf5189f58a06520")
library(ccissdev)

# rstudio_user = "meztez"
# rstudio_password = .rs.api.askForPassword("Choose a password for RStudio Server")
# postgis_password = .rs.api.askForPassword("Choose a password for PostGis database")

# Machine setup
server <- droplet_create("forestvision-server", region = "tor1",
                         image = "ubuntu-20-04-x64", tags = c("oldgrowth", "shiny-server"), wait = TRUE)
server <- droplet(id=server$id)
analogsea::debian_add_swap(server)
bccciss:::setup_firewall(server)
bccciss:::setup_updates(server)

# Install R
analogsea::droplet_ssh(server, c(
  "echo 'deb https://cran.rstudio.com/bin/linux/ubuntu focal-cran40/' >> /etc/apt/sources.list",
  "apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9"
))
analogsea::debian_apt_get_update(server)
analogsea::debian_install_r(server)

# Install RStudio (port 8787, proxied via nginx)
analogsea::debian_apt_get_install(server, "gdebi-core", "libapparmor1")
# analogsea::droplet_ssh(server,
#   "wget -q https://www.rstudio.org/download/latest/stable/server/bionic/rstudio-server-latest-amd64.deb",
#   "sudo gdebi rstudio-server-*-amd64.deb --non-interactive",
#   "rm rstudio-server-*-amd64.deb",
#   sprintf("adduser %s --disabled-password --gecos \"\"", rstudio_user),
#   sprintf("echo \"%s:%s\" | chpasswd", rstudio_user, rstudio_password)
# )

# Install Pandoc
analogsea::droplet_ssh(server,
  "wget https://github.com/jgm/pandoc/releases/download/2.13/pandoc-2.13-1-amd64.deb",
  "sudo gdebi pandoc-2.13-1-amd64.deb --non-interactive",
  "rm pandoc-2.13-1-amd64.deb"
)

# Setup a Postgis database using docker (localhost port 5432) and R RPostgres
# analogsea::droplet_ssh(server,
#   sprintf("docker run --name cciss-postgis -p 5432:5432 -e POSTGRES_PASSWORD=%s -d postgis/postgis", postgis_password)
# )
# analogsea::debian_apt_get_install(server, "--reinstall", "libpq-dev")
# analogsea::droplet_ssh(server, "R -e \"install.packages('RPostgres')\"")

# Connect from RStudio or Shiny app on server using
# con <- RPostgres::dbConnect(
#   Postgres(),
#   user = "postgres",
#   host = "localhost",
#   password = postgis_password,
#   port = 5432,
#   dbname = "postgres"
# )

# Install Shiny Server (port 3838, proxied via nginx)
analogsea::droplet_ssh(server,
  "R -e \"install.packages(c('rmarkdown','shiny'))\"",
  "wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb",
  "sudo gdebi shiny-server-1.5.16.958-amd64.deb --non-interactive",
  "rm shiny-server-*-amd64.deb")

# Setup nginx
bccciss:::install_nginx(server, config = "./data-raw/config/bsgs/nginx.conf", name = "shiny-server")

# utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(server), "/rstudio"))

# Install Shiny App

# system dependencies
# Install chromium to support pdf printing
analogsea::debian_apt_get_install(server,
                                  "git",
                                  "libaio1",
                                  "libc6",
                                  "libcurl4-openssl-dev",
                                  "libgdal-dev",
                                  "libgeos-dev",
                                  "libgit2-dev",
                                  "libgsl-dev",
                                  "libgsl-dev",
                                  "libjq-dev",
                                  "libproj-dev",
                                  "libprotobuf-dev",
                                  "libsecret-1-dev",
                                  "libsodium-dev",
                                  "libssl-dev",
                                  "libudunits2-dev",
                                  "libv8-dev",
                                  "libxml2-dev",
                                  "protobuf-compiler",
                                  "python3-venv",
                                  "sshpass",
                                  "libfontconfig1-dev",
                                  "libharfbuzz-dev",
                                  "libfribidi-dev",
                                  "texlive",
                                  "texlive-extra-utils",
                                  "texlive-latex-extra",
                                  "chromium-browser")
analogsea::droplet_ssh(server, "R -e \"install.packages('remotes')\"")


# upload app to server
server <- analogsea::droplets()$`shiny-server`
reset_ssh_sessions()
#analogsea::droplet_ssh(server,"rm -R /srv/shiny-server/ccissdev/.Renviron")
analogsea::droplet_ssh(server, "rm -R /srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./.Renviron", "/srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "R -e \"remotes::install_github('bcgov/CCISS_ShinyApp', upgrade = FALSE, dependencies = FALSE, force = TRUE)\"")
analogsea::droplet_upload(server, "./app/global.R", "/srv/shiny-server/cciss/global.R")
analogsea::droplet_upload(server, "./app/server.R", "/srv/shiny-server/cciss/server.R")
analogsea::droplet_upload(server, "./app/ui.R", "/srv/shiny-server/cciss/ui.R")
analogsea::droplet_upload(server, "./app/www", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/server", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./app/instructions", "/srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")
analogsea::droplet_ssh(server, "systemctl restart shiny-server")


utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(server), "/shiny/ccissdev"))
