###  - Run setup environment script ----

source("extract-data/00_setup-environment.R")


# Define the database connection with SMRA ----
if (!exists("SMRA_connect")) {
  SMRA_connect <- dbConnect(odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")
  )
}

# Start Date of extract
start_date <- c("'2019-01-01'")

# Define SQL query
smr01_sql_query <- paste(
  "select upi_number, link_no, cis_marker, admission_date, ",
  "discharge_date ",
  "from SMR01_PI",
  "where discharge_date >= to_date(", start_date, ",'yyyy-MM-dd')",
  "ORDER BY upi_number, link_no, cis_marker, admission_date, discharge_date, admission, discharge, uri"
)

# Extract data from database using SQL query above
smr01_extract <- dbGetQuery(SMRA_connect, smr01_sql_query) %>%
  as_tibble()

# set to lower case
names(smr01_extract) <- tolower(names(smr01_extract))

# write extract
# TODO add extract date to file name
write_rds(smr01_extract, path(here("data", str_glue("SMR01_extract.rds"))), compress = "gz")
