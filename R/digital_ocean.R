# Tiles hosting pipeline ----

#' Setup droplet
#' @rdname do_helpers
#' @param droplet A newly created droplet from a Docker+Ubuntu image
#' @importFrom analogsea droplet debian_add_swap
#' @export
setup_docklet <- function(droplet) {
  droplet <- analogsea::droplet(id=droplet$id)
  lines <- droplet_capture(droplet, 'swapon | grep "/swapfile" | wc -l')
  if (lines != "1"){
    analogsea::debian_add_swap(droplet)
  }
  setup_firewall(droplet)
  setup_updates(droplet)
  install_tippecanoe(droplet)
  install_nginx(droplet)
  prepare_tileserver(droplet)
  return(droplet)
}

remote_shp_geojson()

#' Launch tileserver GL
#' @rdname do_helpers
#' @param droplet DO droplet
#' @importFrom analogsea docklet_run droplet_ssh
#' @details Launch a tileserver GL docker image. Also stops
#' any already running image and delete nginx cache.
#' @export
launch_tileserver <- function(droplet, detached = TRUE) {
  # Clear nginx cache
  # analogsea::droplet_ssh(droplet, "rm -R /cache/nginx/*")
  # Run docker image
  # analogsea::droplet_ssh(droplet, "docker stop $(docker ps -a -q)")
  analogsea::droplet_ssh(droplet, "docker run --rm -v /mapdata:/data -p 8080:80 -d --user root maptiler/tileserver-gl")
}

# Helper task -----

#' Install tippecanoe + gdal | shp > geojson > mbtiles
#' @noRd
#' @importFrom analogsea debian_apt_get_install droplet_ssh 
install_tippecanoe <- function(droplet) {
  # tippecanoe from https://github.com/mapbox/tippecanoe)
  analogsea::debian_apt_get_install(droplet, "build-essential", "libsqlite3-dev", "zlib1g-dev", "gdal-bin")
  analogsea::droplet_ssh(droplet, "git clone https://github.com/mapbox/tippecanoe.git")
  analogsea::droplet_ssh(droplet, "cd tippecanoe && make -j && sudo make install")
}

#' Pre-pull tileserver docker image
#' @noRd
#' @importFrom analogsea docklet_pull droplet_ssh
prepare_tileserver <- function(droplet) {
  analogsea::droplet_ssh(droplet, "mkdir /mapdata && chmod 777 -R /mapdata")
  analogsea::docklet_pull(droplet, "maptiler/tileserver-gl")
}

#' Install and configure nginx
#' @noRd
#' @importFrom analogsea docklet_pull droplet_ssh debian_apt_get_install
install_nginx <- function(droplet){
  analogsea::debian_apt_get_install(droplet, "nginx")
  analogsea::droplet_ssh(droplet, "rm -f /etc/nginx/sites-enabled/default") # Disable the default site
  analogsea::droplet_ssh(droplet, "mkdir -p /cache/nginx && chmod -R 777 /cache/nginx")
  analogsea::droplet_upload(droplet, local=system.file("tileserver", "nginx.conf", package="bccciss"),
                            remote="/etc/nginx/sites-available/tileserver")
  analogsea::droplet_ssh(droplet, "ln -sf /etc/nginx/sites-available/tileserver /etc/nginx/sites-enabled/")
  analogsea::droplet_ssh(droplet, "systemctl reload nginx")
}

#' Setup firewall
#' @noRd
#' @importFrom analogsea droplet_ssh
setup_firewall <- function(droplet){
  analogsea::droplet_ssh(droplet, "ufw allow http")
  analogsea::droplet_ssh(droplet, "ufw allow ssh")
  analogsea::droplet_ssh(droplet, "ufw -f enable")
}

#' Setup system updates
#' @noRd
#' @importFrom analogsea droplet_ssh droplet_upload debian_apt_get_update
setup_updates <- function(droplet){
  # To avoid prompt as analogsea is not a TTy
  analogsea::droplet_ssh(droplet, "echo 'DEBIAN_FRONTEND=noninteractive' >> /etc/environment")
  analogsea::droplet_upload(droplet,
                            local=system.file("tileserver", "apt.conf.d", package="bccciss"),
                            remote = "/etc/apt")
  analogsea::debian_apt_get_update(droplet)
}

#' Captures the output from running some command via SSH
#' @noRd
#' @importFrom analogsea droplet_download droplet_ssh
droplet_capture <- function(droplet, command){
  tf <- tempdir()
  randName <- paste(sample(c(letters, LETTERS), size=10, replace=TRUE), collapse="")
  tff <- file.path(tf, randName)
  on.exit({
    if (file.exists(tff)) {
      file.remove(tff)
    }
  })
  analogsea::droplet_ssh(droplet, paste0(command, " > /tmp/", randName))
  analogsea::droplet_download(droplet, paste0("/tmp/", randName), tf)
  analogsea::droplet_ssh(droplet, paste0("rm /tmp/", randName))
  lin <- readLines(tff)
  lin
}