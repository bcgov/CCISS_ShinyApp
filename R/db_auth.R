.auth <- new.env()
.auth$creds <- list()

#' Auth methods
#'
#' Authentification management.
#' @name db_auth
#' @aliases init_con
NULL

#' Initiate a connection CCISS database
#' @rdname db_auth
#' @param host A character string. Postgres database host. Default to `localhost`.
#' @param dbname A character string. Postgres database name. Default to `cciss_data`.
#' @param port A numeric. Postgres databse port. Default to 5432.
#' @param user A character string. Postgres database user. Default to `postgres`.
#' @param ... Additional parameters for dbConnect.
#' @return A connection pointer to Postgres database.
#' @export
init_con <- function(host = "localhost", dbname = "cciss_data", port = 5432, user = "postgres", ...) {
  db_auth(host = host, dbname = dbname, port = port, user = user, ...)
}

#' Authenticate and deauthenticate to Postgres database
#' @rdname db_auth
#' @param force A boolean. Force reauthentification.
#' @details Manage authentification credentials using package `keyring`
#' @importFrom utils menu
#' @importFrom getPass getPass
#' @importFrom keyring key_get key_set_with_value
#' @export
db_auth <- function(host, dbname, port, user, force = FALSE, ...) {
  
  if (is.null(.auth$creds$pwd) || force == TRUE) {
    key <- paste0(host, dbname, port, user)
    tryCatch(
      {
        .auth$creds$pwd <- keyring::key_get("bccciss", key)
      },
      error = function(e) {
        if (interactive()) {
          .auth$creds$pwd <- getPass::getPass(
            paste0("Enter Postgres ", dbname, " database password for user ", user, " on ", host, ":", port)
          )
          if (is.null(.auth$creds$pwd)) { stop("Could not authorize", user) }
          if (utils::menu(c("Yes","No"), title = "Save password to your system keyring between R sessions?") == 1L) {
            keyring::key_set_with_value("bccciss", key, .auth$creds$pwd)
          }
        } else {
          stop("Could not obtain password to authenticate to database.
              Password has to be saved in system keyring for non-interactive sessions.")
        }
      }
    )
  }
  
  db_auth_check(host = host, dbname = dbname, port = port,
                user = user, password = .auth$creds$pwd,...)
}

#' @rdname db_auth
#' @details Deauthorize credentials. Remove from memory.
#' @export
db_deauth <- function() {
  .auth$creds <- list()
}

#' Check credentials
#' @rdname db_auth
#' @param password Password
#' @importFrom RPostgreSQL PostgreSQL dbConnect
#' @importFrom keyring key_delete
db_auth_check <- function(host, dbname, port, user, password, ...) {
  tryCatch(
    {
      drv <- RPostgreSQL::PostgreSQL()
      RPostgreSQL::dbConnect(drv, user = user, host = host, password = password, port = port, dbname = dbname, ...)
    },
    error = function(e) {
      db_deauth()
      try(keyring::key_delete("bccciss", paste0(host, dbname, port, user)), silent = TRUE)
      message(e)
      return(NULL)
    }
  )
}
