###  - Run setup environment script ----

source("extract-data/00_setup-environment.R")


# Define the database connection with SMRA
if (!exists("SMRA_connect")) {
  SMRA_connect <- odbc::dbConnect(
    drv = odbc::odbc(),
    dsn = "SMRA",
    uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
    pwd = rstudioapi::askForPassword("SMRA Password:")
  )
}


start_date <- c("'2020-01-01'")

# Define SQL query
nrs_sql_query <- paste(
  "select chi, date_of_death, underlying_cause_of_death, ",
  "cause_of_death_code_0, cause_of_death_code_1, cause_of_death_code_2, ",
  "cause_of_death_code_3, cause_of_death_code_4, cause_of_death_code_5, ",
  "cause_of_death_code_6, cause_of_death_code_7, cause_of_death_code_8, ",
  "cause_of_death_code_9 ",
  "from ANALYSIS.GRO_DEATHS_C",
  "where date_of_death >= to_date(", start_date, ",'yyyy-MM-dd')",
  "ORDER BY chi, date_of_death"
)

# Extract data from database using SQL query above
nrs_extract <- dbGetQuery(SMRA_connect, nrs_sql_query) %>%
  as_tibble() %>%
  rename(chi_number = CHI)

# set to lower case
names(nrs_extract) <- tolower(names(nrs_extract))

# write extract
write_rds(nrs_extract, path(here("data", str_glue("{today()}_NRS_extract.rds"))), compress = "gz")

# Copy to server
nrs_data_local <- path(here("data", str_glue("{today()}_NRS_extract.rds")))

if (file_exists(nrs_data_local)) {
  file_copy(
    path = nrs_data_local,
    new_path = path(server_dir, str_glue("{today()}_NRS_extract.rds"))
  )
}

# Clean up
rm(start_date, nrs_sql_query, nrs_extract)
