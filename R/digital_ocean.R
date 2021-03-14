# Tiles hosting pipeline with Tileserver GL ----

#' Setup droplet
#' @rdname do_helpers
#' @param size A character string. Size slug identifier. See [analogsea::sizes()] for a complete
#' list. Default to `s-2vcpu-4gb-intel`, large enough to serve layers covering B.C. with fast
#' memory and disk.
#' @param region A character string. The unique slug identifier for the region that you wish to
#' deploy in. See [analogsea::regions()] for a complete list. Default to `tor1` in Canada.
#' @param image A character string. The image ID of a public or private image, or the unique slug
#' identifier for a public image. This image should have Docker already installer.
#'  See [analogsea::images()] for a complete list. Default to `docker-20-04`.
#' @param tags A vector of tag names to apply to the Droplet after it is created.
#' Tag names can either be existing or new tags. Default to `c("bccciss", "tileserver")`
#' @param checks A boolean. Check that region is `tor1` and image is official `Docker`. Default
#' to false.
#' @param ... Additional options passed down to [analogsea::droplet_create()].
#' @details Setup a droplet with necessary tooling to convert shape files to vector tiles and
#' host them using tileserver gl.
#' @return A droplet.
#' @importFrom analogsea droplet debian_add_swap keys droplet_create
#' @importFrom utils menu
#' @export
setup_docklet <- function(size = "s-2vcpu-4gb-intel",
                          region = "tor1",
                          image = "docker-20-04",
                          tags = c("bccciss", "tileserver"),
                          checks = FALSE,
                          ...) {
  
  # Check if Digital Ocean has configured ssh keys
  if (!length(analogsea::keys())) {
    stop("Please add an ssh key to your Digital Ocean account. See `analogsea::key_create` method.")
  }
  
  # Check region is in Canada
  if (checks == TRUE && region != "tor1") {
    choices <- c("Yes", "No")
    title <- "Region selected is not `tor1`. You might be using a server located outside Canada. Are you sure?"
    if (utils::menu(choices = choices, title = title) == 2) {
      return(NULL)
    }
  }
  
  # Check image for Docker in region
  if (checks == TRUE && !image %in% docker_images(region)) {
    choices <- c("Absolutely", "What's a docker? (No)")
    title <- "Image selected is not an official Docker image in this region. Are you sure?"
    if (utils::menu(choices = choices, title = title) == 2) {
      return(NULL)
    }
  }
  
  # Create drocklet
  droplet <- analogsea::droplet_create(image = image, region = region,
                                       size = size, tags = tags, wait = TRUE, ...)
  
  # Wait for the network interface
  while (length(droplet$networks$v4) == 0) {
    droplet <- analogsea::droplet(id=droplet$id)
    Sys.sleep(10)
  }
  
  analogsea::debian_add_swap(droplet)
  setup_firewall(droplet)
  setup_updates(droplet)
  install_tippecanoe(droplet)
  install_nginx(droplet, system.file("tileserver", "nginx.conf", package="bccciss"))
  prepare_tileserver(droplet)
  return(invisible(droplet))
}

#' Transform shape files to mbtiles
#' @rdname do_helpers
#' @param droplet Tileserver droplet
#' @param ... additional command line arguments for `tippecanoe`
#' @param source_dir Directory containing shape files
#' @param remote_dir Directory where to upload files on remote. Default to `/tmp/shp`.
#' @param skip_upload A boolean. Skip upload and convert to geojson part. Default FALSE.
#' @details Look for shape files in `source_dir`. Upload `source_dir` content to droplet
#'  `remote_dir`. Transform shape files to geojson and call `tippecanoe` with each layers found
#' using `...` for parametrization creating `tiles.mbtiles`. See [tippecanoe_usage()] for 
#' `tippecanoe` usage documentation.
#' @importFrom tools file_path_sans_ext
#' @importFrom analogsea droplet_ssh droplet_upload 
#' @export
remote_shp_tiles <- function(droplet, ..., source_dir, remote_dir = "/tmp/shp", skip_upload = FALSE) {
  
  layers <- list.files(source_dir, "\\.shp$", ignore.case = TRUE)
  if (length(layers) == 0) {
    message("Could not find any shape files to upload")
    return(NULL)
  }
  names(layers) <- tools::file_path_sans_ext(layers)
  
  if (!skip_upload == TRUE) {
    analogsea::droplet_ssh(droplet, paste("mkdir -p", remote_dir))
    analogsea::droplet_upload(droplet, local = list.files(source_dir, full.names = TRUE), remote = remote_dir)
    geojsons <- character()
    for (i in 1:length(layers)) {
      geojson <- shQuote(file.path(remote_dir, paste0(names(layers[i]), ".geojson")))
      shp <- shQuote(file.path(remote_dir, layers[i]))
      analogsea::droplet_ssh(droplet, paste("ogr2ogr -f GeoJSON", geojson, shp))
      geojsons <- c(geojsons, geojson)
    }
    analogsea::droplet_ssh(droplet, paste("ls -alh", remote_dir,"| grep geojson"))
  } else {
    geojsons <- shQuote(file.path(remote_dir, paste0(names(layers), ".geojson")))
  }
  
  base_cmd <- "tippecanoe -o /mapdata/tiles.mbtiles"
  cmd <- paste(base_cmd, paste(..., collapse = " "), paste(geojsons, collapse = " "))
  analogsea::droplet_ssh(droplet, cmd)
  analogsea::droplet_ssh(droplet, "ls -alh /mapdata | grep mbtiles")
  
  return(invisible(droplet))

}

#' Open browser to tippecanoe usage manual
#' @rdname do_helpers
#' @export
tippecanoe_usage <- function() {
  utils::browseURL("https://github.com/mapbox/tippecanoe/blob/master/README.md#usage")
}

#' Open browser to tileserver GL documentation
#' @rdname do_helpers
#' @export
#' @importFrom utils browseURL
tileserver_doc <- function() {
  utils::browseURL("https://tileserver.readthedocs.io/en/latest/")
} 

#' Open browser to GL styling documentation
#' @rdname do_helpers
#' @export
styling_doc <- function() {
  utils::browseURL("https://docs.mapbox.com/mapbox-gl-js/style-spec/")
}

#' Reset SSH sessions
#' @rdname do_helpers
#' @export
reset_ssh_sessions <- function() {
  env <- analogsea:::analogsea_sessions
  rm(list = ls(envir = env), envir = env)
}

#' Launch tileserver GL
#' @rdname do_helpers
#' @param droplet DO droplet
#' @param config Local path to a tileserver config file
#' @param styles Local path to Mapbox GL compatible styles (file or dir)
#' @importFrom analogsea docklet_run droplet_ssh
#' @details Launch a tileserver GL docker image. Also stops
#' any already running instances, delete server cache and reload nginx.
#' 
#' If config file is provided, it will be uploaded to server and used
#' to launch tileserver. See https://tileserver.readthedocs.io/en/latest/config.html
#' for details about config
#' @export
launch_tileserver <- function(droplet, config, styles) {
  if (!missing(config)) {
    analogsea::droplet_upload(droplet, config, "/mapdata")
  }
  if (!missing(styles)) {
    analogsea::droplet_upload(droplet, styles, "/mapdata")
  }
  # Clear cache and reload nginx
  analogsea::droplet_ssh(droplet, "rm -rf /cache/nginx/*")
  analogsea::droplet_ssh(droplet, "systemctl reload nginx")
  # Stop all running containers
  analogsea::droplet_ssh(droplet, "docker ps -q -a | xargs -r docker stop")
  # Star tileserver as root
  analogsea::droplet_ssh(droplet, "docker run --rm -v /mapdata:/data -p 8080:8080 -d --user root maptiler/tileserver-gl -s")
  Sys.sleep(3)
  utils::browseURL(paste0("http://", analogsea:::droplet_ip_safe(droplet)))
  return(droplet)
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
  # tileserver from https://github.com/maptiler/tileserver-gl
  analogsea::droplet_ssh(droplet, "mkdir /mapdata/ && chmod 777 -R /mapdata")
  analogsea::docklet_pull(droplet, "maptiler/tileserver-gl")
}

#' Install and configure nginx
#' @noRd
#' @importFrom analogsea docklet_pull droplet_ssh debian_apt_get_install
install_nginx <- function(droplet, config, name = "tileserver"){
  analogsea::debian_apt_get_install(droplet, "nginx")
  analogsea::droplet_ssh(droplet, "rm -f /etc/nginx/sites-enabled/default") # Disable the default site
  analogsea::droplet_ssh(droplet, "mkdir -p /cache/nginx && chmod -R 777 /cache/nginx")
  analogsea::droplet_upload(droplet, local=config,
                            remote=paste0("/etc/nginx/sites-available/", name))
  analogsea::droplet_ssh(droplet, paste0("ln -sf /etc/nginx/sites-available/", name," /etc/nginx/sites-enabled/"))
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

#' @importFrom analogsea images
docker_images <- function(region) {
  slugs <- character()
  page <- 1
  repeat {
    img <- analogsea::images(page = page, per_page = 100L)
    if (length(img) == 0) break
    slugs <- c(slugs, find_slugs(img, "Docker", region))
    page <- page + 1
  }
  return(slugs)
}

find_slugs <- function(img, pattern, region) {
  slugs <- character()
  for (i in img[grepl(pattern, names(img))]) {
    if (region %in% unlist(i$regions)) {
      slugs <- c(slugs, i$slug)
    }
  }
  return(slugs)
}
