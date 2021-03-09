library(analogsea)
library(bccciss)

rstudio_user = "meztez"
rstudio_password = .rs.api.askForPassword("Choose a password for RStudio Server")
postgis_password = .rs.api.askForPassword("Choose a password for PostGis database")

# Machine setup
# server <- droplet_create("BigShinyGisStudio", size = "s-4vcpu-8gb-intel", region = "tor1",
#                          image = "docker-20-04", tags = c("bccciss", "shiny", "rstudio", "postgis"), wait = TRUE)
server <- droplets()$BigShinyGisStudio
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
analogsea::debian_install_r(server, rprofile = "options(repos=c('RSPM'='https://packagemanager.rstudio.com/cran/__linux__/focal/latest','CRAN'='https://cloud.r-project.org/'))")

# Install RStudio (port 8787, proxied via nginx)
analogsea::debian_apt_get_install(server, "gdebi-core", "libapparmor1")
analogsea::droplet_ssh(server,
  "wget -q https://www.rstudio.org/download/latest/stable/server/bionic/rstudio-server-latest-amd64.deb",
  "sudo gdebi rstudio-server-*-amd64.deb --non-interactive",
  "rm rstudio-server-*-amd64.deb",
  sprintf("adduser %s --disabled-password --gecos \"\"", rstudio_user),
  sprintf("echo \"%s:%s\" | chpasswd", rstudio_user, rstudio_password)
)

# Install Pandoc
analogsea::droplet_ssh(server,
  "wget https://github.com/jgm/pandoc/releases/download/2.11.4/pandoc-2.11.4-1-amd64.deb",
  "sudo gdebi  pandoc-2.11.4-1-amd64.deb --non-interactive",
  "rm pandoc-2.11.4-1-amd64.deb"
)

# Setup a Postgis database using docker (localhost port 5432)
analogsea::droplet_ssh(server,
  sprintf("docker run --name cciss-postgis -p 5432:5432 -e POSTGRES_PASSWORD=%s -d postgis/postgis", postgis_password)
)
analogsea::debian_apt_get_install(server, "--reinstall", "libpq-dev")
analogsea::droplet_ssh(server, "R -e \"install.packages('RPostgreSQL')\"")

# Connect from RStudio or Shiny app on server using
# con <- RPostgreSQL::dbConnect(
#   PostgreSQL(),
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
bccciss:::install_nginx(server, config = "./data-raw/config/bsgs/nginx.conf", name = "BSGS")

utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(server), "/rstudio"))

# Install Shiny App

# dependencies
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
                                  "libfribidi-dev")
analogsea::droplet_ssh(server, "R -e \"install.packages('remotes')\"")

# upload app to server
analogsea::droplet_ssh(server, "mkdir /srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "R -e \"remotes::install_github('bcgov/CCISS_ShinyAPP@code_with_us', upgrade = TRUE, dependencies = TRUE, force = TRUE)\"")
analogsea::droplet_upload(server, "./inst/application/index.Rmd", "/srv/shiny-server/cciss/index.Rmd")
analogsea::droplet_upload(server, "./inst/application/www", "/srv/shiny-server/cciss")
analogsea::droplet_upload(server, "./inst/application/server", "/srv/shiny-server/cciss")
analogsea::droplet_ssh(server, "chown -R shiny:shiny /srv/shiny-server")

utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(server), "/shiny/cciss"))
