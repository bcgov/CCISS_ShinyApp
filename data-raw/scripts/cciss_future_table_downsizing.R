library(RPostgres)
library(DBI)
conn <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)

future_params <- data.table::setDT(dbReadTable(conn, "future_params"))
bgc_attribution <- dbGetQuery(conn, "SELECT DISTINCT bgc FROM bgc_attribution")

simple_table <- function(conn, table, values, replace = FALSE) {
  
  if (replace) {
    res <- dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s", table))  
  }
  
  query <- sprintf("
  CREATE TABLE %s (
    %s_id SMALLSERIAL PRIMARY KEY ,
    %s TEXT NOT NULL CHECK (%s <> '')
  );", table, table, table, table)
  
  res <- dbExecute(conn, query)
  
  query <- paste0(sprintf("
  INSERT INTO %s (%s)", table, table), "
  VALUES ", paste0("('", values, "')", collapse = ","))
  
  res <- dbExecute(conn, query)
  
  invisible()
}

simple_table(conn, "futureperiod", sort(unique(future_params$futureperiod)), replace = TRUE)
simple_table(conn, "gcm", sort(unique(future_params$gcm)), replace = TRUE)
simple_table(conn, "scenario", sort(unique(future_params$scenario)), replace = TRUE)

dat <- fread("~/CommonTables/WNA_SSeries_v12_6.csv")
bgcs <- unique(dat$BGC)
simple_table(conn, "bgc", sort(bgcs), replace = TRUE)


# Now cciss_future12
query <- "
  ALTER TABLE hex_points ADD PRIMARY KEY (siteno)"

dbExecute(conn, query)

query <- "
  CREATE TABLE cciss_future12SMLR (
    siteno INTEGER REFERENCES hex_points,
    gcm SMALLSERIAL REFERENCES gcm,
    scenario SMALLSERIAL REFERENCES scenario,
    futureperiod SMALLSERIAL REFERENCES futureperiod,
    bgc_pred SMALLSERIAL REFERENCES bgc
  )
"

dbExecute(conn, query)
