###  - Run setup environment script ----

source("extract-data/00_setup-environment.R")


# Define the database connection with SMRA ----
if (!exists("SMRA_connect")) {
  SMRA_connect <- odbc::dbConnect(
    drv = odbc::odbc(),
    dsn = "SMRA",
    uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
    pwd = rstudioapi::askForPassword("SMRA Password:")
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
write_rds(smr01_extract, path(here("data", str_glue("{today()}_SMR01_extract.rds"))), compress = "gz")

# Copy to server
smr01_data_local <- path(here("data", str_glue("{today()}_SMR01_extract.rds")))

if (file_exists(smr01_data_local)) {
  file_copy(
    path = smr01_data_local,
    new_path = path(server_dir, str_glue("{today()}_SMR01_extract.rds"))
  )
}

# Clean up
rm(start_date, smr01_sql_query, smr01_extract)
