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
gcmsOpts <- c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CNRM-ESM2-1", "CanESM5", "EC-Earth3", 
  "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
  "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
simple_table(conn, "gcmv2", gcmsOpts, replace = TRUE)
simple_table(conn, "scenario", sort(unique(future_params$scenario)), replace = TRUE)

dat <- fread("~/CommonTables/WNA_SSeries_v12_6.csv")
bgcs <- unique(dat$BGC)
simple_table(conn, "bgc", sort(bgcs), replace = TRUE)


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

##########################################
# Now do it full ----

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

dbExecute(conn, "DROP TABLE IF EXISTS cciss_future12_array")

query <- "
  CREATE TABLE cciss_future12_array (
    siteno INTEGER REFERENCES hex_points,
    -- [gcm][scenario][futureperiod]
    bgc_pred_id SMALLINT[13][4][5]
  )
"
dbExecute(conn, query)

# Handle missing combinations
idx_ipt_len <- function(index, input, length) {
  x <- NA_integer_
  length(x) <- length
  x[index] <- input
  x
}

insert_by_part <- function(conn, siteno_range) {

  query <- sprintf("
    SELECT
      siteno,
      gcm.gcm_id,
      scenario.scenario_id,
      futureperiod.futureperiod_id,
      bgc.bgc_id bgc_pred_id
    FROM cciss_future12
    JOIN gcm ON (cciss_future12.gcm = gcm.gcm)
    JOIN scenario ON (cciss_future12.scenario = scenario.scenario)
    JOIN futureperiod ON (cciss_future12.futureperiod = futureperiod.futureperiod)
    JOIN bgc ON (cciss_future12.bgc_pred = bgc.bgc)
    WHERE cciss_future12.siteno >= %s and cciss_future12.siteno <= %s", siteno_range[1], siteno_range[2]
  )
  
  dt <- setDT(dbGetQuery(conn, query))
  
  if (nrow(dt) > 0) {
    insert <- dt[,
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
    
    query <- paste0("
      INSERT INTO cciss_future12_array (
        siteno,
        bgc_pred_id
      ) VALUES ",
      paste0("(", insert$siteno, ", ", insert$bgc_pred_id, ")", collapse = ", ")
    )
    
    dbExecute(conn, query) 
  }
  
  invisible()

}

sitenos <- dbGetQuery(conn, "SELECT siteno FROM hex_points")$siteno

while (length(sitenos)) {
  range <- 1L:50000L
  insert_by_part(conn, c(
    min(sitenos[range], na.rm = TRUE),
    max(sitenos[range], na.rm = TRUE)
  ))
  sitenos <- sitenos[-(range)]
}

dbSendQuery(conn, glue::glue("
    CREATE INDEX {tb}_idx ON {tb} USING BTREE ({idx});
  ", tb = "cciss_future12_array", idx = "siteno"))
dbSendQuery(conn, glue::glue("VACUUM ANALYZE {tb};", tb = "cciss_future12_array"))

# Writing queries and updating table will get slightly more complex but the size saving are huge
# and the performance are back
dbGetQuery(conn, "SELECT pg_total_relation_size('cciss_future12_array')") /
  dbGetQuery(conn, "SELECT pg_total_relation_size('cciss_future12')")

# pg_total_relation_size
# 1             0.01450527

testLabel <- dbGetQuery(conn, "SELECT ROW_NUMBER() OVER(ORDER BY gcm, scenario, futureperiod) row_idx, gcm, scenario, futureperiod FROM gcm CROSS JOIN scenario CROSS JOIN futureperiod")

siteno <- "2318928"

testLabel <- dbGetQuery(conn, "SELECT gcm, scenario, futureperiod FROM futureperiod CROSS JOIN scenario CROSS JOIN gcm")

# Example on how to transform into original cciss_future12
dt1 <- setDT(dbGetQuery(conn, "
           SELECT siteno,
         labels.gcm,
         labels.scenario,
         labels.futureperiod,
         bgc.bgc
  FROM cciss_future12_array,
       unnest(bgc_pred_id) WITH ordinality as source(bgc_pred_id, row_idx)
  JOIN (SELECT ROW_NUMBER() OVER(ORDER BY gcm_id, scenario_id, futureperiod_id) row_idx,
               gcm,
               scenario,
               futureperiod
        FROM gcm 
        CROSS JOIN scenario
        CROSS JOIN futureperiod) labels
    ON labels.row_idx = source.row_idx
  JOIN bgc
    ON bgc.bgc_id = source.bgc_pred_id
  WHERE siteno = 2281768"))
# Example on how to transform into original cciss_future12

# modWeights[,comb := paste0("('",gcm,"','",rcp,"',",weight,")")]
# weights <- paste(modWeights$comb,collapse = ",")
# dt1 <- dbGetQuery(conn, paste0("SELECT cciss_future12_array.siteno,
#          labels.gcm,
#          labels.scenario,
#          labels.futureperiod,
#          bgc_attribution.bgc,
#          bgc.bgc bgc_pred,
#          w.weight
#   FROM cciss_future12_array
#   JOIN bgc_attribution
#     ON (cciss_future12_array.siteno = bgc_attribution.siteno),
#        unnest(bgc_pred_id) WITH ordinality as source(bgc_pred_id, row_idx)
#   JOIN (SELECT ROW_NUMBER() OVER(ORDER BY gcm_id, scenario_id, futureperiod_id) row_idx,
#                gcm,
#                scenario,
#                futureperiod
#         FROM gcm 
#         CROSS JOIN scenario
#         CROSS JOIN futureperiod) labels
#     ON labels.row_idx = source.row_idx
#     JOIN (values ",weights,") 
#     AS w(gcm,scenario,weight)
#     ON labels.gcm = w.gcm AND labels.scenario = w.scenario
#   JOIN bgc
#     ON bgc.bgc_id = source.bgc_pred_id
#   WHERE cciss_future12_array.siteno IN (", paste(unique(siteno), collapse = ","), ")
#   AND futureperiod IN ('2021','2041','2061','2081')"))

dt4 <- dbGetQuery(conn,"SELECT 
               futureperiod,
               scenario,
               gcm
        FROM futureperiod 
        CROSS JOIN scenario
        CROSS JOIN gcm")

dt2 <- setDT(dbGetQuery(conn, "
  SELECT siteno,
         gcm,
         scenario,
         futureperiod,
         bgc_pred
  FROM cciss_future12
  WHERE siteno = 2281768"))

dt2[,futureperiod := substr(futureperiod,1,4)]

dt2[dt1,XPred := i.bgc, on = c("siteno","gcm","scenario","futureperiod")]
dt2[,Same := bgc_pred == XPred]
