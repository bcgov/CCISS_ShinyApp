library(RPostgres)
library(DBI)
library(data.table)

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
simple_table(conn, "bgc", sort(bgc_attribution$bgc), replace = TRUE)


# Now cciss_future12
query <- "
  ALTER TABLE hex_points ADD PRIMARY KEY (siteno)"

dbExecute(conn, query)

# It seems everything has to take at least 8 bytes to respect columns alignment
dbExecute(conn, "DROP TABLE cciss_future12smlr_test")

query <- "
  CREATE TABLE cciss_future12smlr_test (
    siteno INTEGER REFERENCES hex_points,
    gcm_id SMALLINT REFERENCES gcm,
    scenario_id SMALLINT REFERENCES scenario,
    futureperiod_id SMALLINT REFERENCES futureperiod,
    bgc_pred_id SMALLINT REFERENCES bgc
  )
"

dbExecute(conn, query)

query <- "
EXPLAIN (ANALYSE, BUFFERS)
INSERT INTO cciss_future12smlr_test (
  siteno,
  gcm_id,
  scenario_id,
  futureperiod_id,
  bgc_pred_id
)
SELECT
  rast_id siteno,
  gcm.gcm_id,
  scenario.scenario_id,
  futureperiod.futureperiod_id,
  bgc.bgc_id bgc_pred_id
FROM (SELECT * FROM pts2km_future WHERE rast_id between 241002 and 250000) pts2km_future
JOIN gcm ON (pts2km_future.gcm = gcm.gcm)
JOIN scenario ON (pts2km_future.scenario = scenario.scenario)
JOIN futureperiod ON (pts2km_future.futureperiod = futureperiod.futureperiod)
JOIN bgc ON (pts2km_future.bgc_pred = bgc.bgc);
"

(res <- dbGetQuery(conn, query))
dbGetQuery(conn, "SELECT pg_table_size('cciss_future12smlr_test')")
dt_test<-dbReadTable(conn, "cciss_future12smlr_test")
setDT(dt_test)
dt_test <- dt_test[order(gcm_id, scenario_id, futureperiod_id)]
# Making we are not skipping values
unique(dt_test$gcm_id)
unique(dt_test$scenario_id)
unique(dt_test$futureperiod_id)


# Handle missing combinations
idx_ipt_len <- function(index, input, length) {
  x <- NA_integer_
  length(x) <- length
  x[index] <- input
  x
}

insert <- dt_test[,
  list(bgc_pred_id = paste0(idx_ipt_len(futureperiod_id, bgc_pred_id, 5L), collapse = ",")),
  by = list(siteno, gcm_id, scenario_id)
][,
  list(bgc_pred_id = paste0("{", idx_ipt_len(scenario_id, bgc_pred_id, 4L), "}", collapse = ",")),
  by = list(siteno, gcm_id)
][,
  list(bgc_pred_id = paste0("'{", paste0("{", idx_ipt_len(gcm_id, bgc_pred_id, 13L), "}", collapse = ","), "}'")),
  by = list(siteno)
][,
  list(siteno, bgc_pred_id = gsub("NA", "NULL", bgc_pred_id, fixed = TRUE))
]

dbExecute(conn, "DROP TABLE cciss_future12smlr_test2")

query <- "
  CREATE TABLE cciss_future12smlr_test2 (
    siteno INTEGER REFERENCES hex_points,
    -- [gcm][scenario][futureperiod]
    bgc_pred_id SMALLINT[13][4][5]
  )
"
dbExecute(conn, query)

query <- paste0("
EXPLAIN (ANALYSE, BUFFERS)
INSERT INTO cciss_future12smlr_test2 (
  siteno,
  bgc_pred_id
) VALUES ",
paste0("(", insert$siteno, ", ", insert$bgc_pred_id, ")", collapse = ", "))


dbExecute(conn, query)

# Writing queries and updating table will get slightly more complex but the size saving are huge
# and the performance are back
dbGetQuery(conn, "SELECT pg_table_size('cciss_future12smlr_test2')") /
dbGetQuery(conn, "SELECT pg_table_size('cciss_future12smlr_test')")

# [gcm][scenario][futureperiod]
dbGetQuery(conn, "SELECT siteno, bgc_pred_id[2][1][2] FROM cciss_future12smlr_test2")
dbGetQuery(conn, "SELECT siteno, bgc_pred_id[2:8][1][2] FROM cciss_future12smlr_test2")
dbGetQuery(conn, "SELECT siteno, unnest(bgc_pred_id[1:4][1:4][2]) FROM cciss_future12smlr_test2")

