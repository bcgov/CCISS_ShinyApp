library(RPostgres)
library(pool)
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("BCGOV_DB"),
  host = Sys.getenv("BCGOV_HOST"),
  port = 5432, 
  user = Sys.getenv("BCGOV_USR"),
  password = Sys.getenv("BCGOV_PWD")
)


dbExecute(pool, "drop table cciss_users")
dbExecute(pool, "CREATE TABLE cciss_users (
	user_id serial PRIMARY KEY,
	username VARCHAR ( 50 ) UNIQUE NOT NULL,
	email VARCHAR ( 255 ) UNIQUE NOT NULL,
	role VARCHAR (255) NOT NULL,
	nsession INT
);")

dbExecute(pool, "INSERT INTO cciss_users (username, email, nsession) VALUES ('testuname','test@test.ca',0)")
dbGetQuery(pool, "SELECT exists (SELECT 1 FROM cciss_users WHERE username = 'sdfs' LIMIT 1);")

tryCatch({
  dbExecute(pool, "INSERT INTO cciss_users (username, email, nsession) VALUES ('testuname2','test@test1.ca',0)")
},
error = function(e) {
  print("Username exists")
})
