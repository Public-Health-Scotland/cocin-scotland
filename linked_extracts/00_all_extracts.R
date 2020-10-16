source("extract-data/00_setup-environment.R")

# Export COCIN data
# Note you will need an API key (a pop-up will ask for it)


# COCIN data ------------------------------------------------------------

# Note it will extract maximum once a week
if (floor_date(today(), "week") > floor_date(ymd_hm(latest_extract_date()), "week")) {
  message("Data is from a previous week - getting new extract")
  source("extract-data/01_get-scottish-cocin-data.R")
  source("extract-data/02_clean-cocin-data.R")
}

# Copy to server
cocin_data_local <- path(here("data", str_glue("{latest_extract_date()}_scot-data-clean.rds")))
cocin_metadata_local <- path(here("data", str_glue("{date(latest_extract_date())}_COCIN-metadata.csv")))

if (file_exists(cocin_data_local)) {
  file_copy(
    path = cocin_data_local,
    new_path = path(server_dir, str_glue("{date(latest_extract_date())}_cocin-clean-data.rds"))
  )

  if (file_exists(cocin_metadata_local)) {
    file_copy(
      path = cocin_metadata_local,
      new_path = path(server_dir, str_glue("{date(latest_extract_date())}_cocin-metadata.csv"))
    )
  }
}

# RAPID data --------------------------------------------------------------
# Get the RAPID data
source("extract-data/03_read_RAPID_data.R")
source("ad-hoc-analysis/tidy_RAPID_data.R")

# Copy to server
rapid_data_local <- path(here("data", str_glue("{date(latest_extract_date())}_RAPID-cleaned-filtered.rds")))


if (file_exists(rapid_data_local)) {
  file_copy(
    path = rapid_data_local,
    new_path = path(server_dir, str_glue("{date(latest_extract_date())}_RAPID-cleaned-filtered.rds"))
  )
}


# SMR01 data --------------------------------------------------------------
# Get the SMR01 data

# Define the database connection with SMRA
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
Query_SMR01 <- paste(
  "select upi_number, link_no, cis_marker, admission_date, ",
  "discharge_date ",
  "from SMR01_PI",
  "where discharge_date >= to_date(", start_date, ",'yyyy-MM-dd')",
  "ORDER BY upi_number, link_no, cis_marker, admission_date, discharge_date, admission, discharge, uri"
)

# Extract data from database using SQL query above
SMR01 <- dbGetQuery(SMRA_connect, Query_SMR01) %>%
  as_tibble()

# set to lower case
names(SMR01) <- tolower(names(SMR01))

# write extract
write_rds(SMR01, path(here("data", str_glue("SMR01_extract.rds"))), compress = "gz")

# SICSAG data --------------------------------------------------------------
# Get the ICU data

# Set correct filepath for server or desktop
sicsag_file_path <- path(
  if_else(version$platform == "x86_64-pc-linux-gnu",
    "/conf",
    "//stats"
  ),
  "PHSCOVID19_Analysis", "RAPID Reporting", "Daily_extracts", "ICU Linkage files",
  "File for PHS.csv"
)

# Read in data
sicsag <- read_tsv(sicsag_file_path)
if (ncol(sicsag) == 1) stop("Probable error with sicsag file (try swapping between read_csv and read_tsv")

# write extract
write_rds(sicsag, path(here("data", str_glue("SICSAG_extract.rds"))), compress = "gz")


# NRS data --------------------------------------------------------------
# Get the Deaths data

# Define the database connection with SMRA
if (!exists("SMRA_connect")) {
  SMRA_connect <- dbConnect(odbc(),
    dsn = "SMRA",
    uid = .rs.askForPassword("SMRA Username:"),
    pwd = .rs.askForPassword("SMRA Password:")
  )
}


start_date <- c("'2020-01-01'")

# Define SQL query
Query_NRS <- paste(
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
NRS <- dbGetQuery(SMRA_connect, Query_NRS) %>%
  as_tibble() %>%
  rename(chi_number = CHI)

# set to lower case
names(NRS) <- tolower(names(NRS))

# write extract
write_rds(NRS, path(here("data", str_glue("NRS_extract.rds"))), compress = "gz")


# Clean up old local data -------------------------------------------------------
source("extract-data/99_remove-old-data.R")
