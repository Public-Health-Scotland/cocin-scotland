source("extract-data/00_setup-environment.R", echo = TRUE)

# Export COCIN data
# Note you will need an API key (a pop-up will ask for it)


# COCIN data ------------------------------------------------------------

# Note it will extract maximum once a week
if (floor_date(today(), "week") > floor_date(ymd_hm(latest_extract_date()), "week")) {
  message("Data is from a previous week - getting new extract")
  source("extract-data/01_get-scottish-cocin-data.R", echo = TRUE)
  source("clean-data/clean_cocin_data.R", echo = TRUE)
}

# Copy to server
cocin_data_local <- path(here("data", str_glue("{latest_local_cocin_date()}_scot-data-clean.rds")))
cocin_metadata_local <- path(here("data", str_glue("{date(latest_local_cocin_date())}_COCIN-metadata.csv")))

if (file_exists(cocin_data_local)) {
  file_copy(
    path = cocin_data_local,
    new_path = path(server_dir, str_glue("{date(latest_local_cocin_date())}_cocin-clean-data.rds"))
  )

  # Include COCIN metadata
  if (file_exists(cocin_metadata_local)) {
    file_copy(
      path = cocin_metadata_local,
      new_path = path(server_dir, str_glue("{date(latest_local_cocin_date())}_cocin-metadata.csv"))
    )
  }
}

# RAPID data --------------------------------------------------------------
# Get the RAPID data
source("extract-data/02_read_RAPID_data.R", echo = TRUE)
source("clean-data/clean_RAPID_data.R", echo = TRUE)

# Copy to server
rapid_data_local <- path(here("data", str_glue("{date(latest_local_cocin_date())}_RAPID-cleaned-filtered.rds")))


if (file_exists(rapid_data_local)) {
  file_copy(
    path = rapid_data_local,
    new_path = path(server_dir, str_glue("{date(latest_local_cocin_date())}_RAPID-cleaned-filtered.rds"))
  )
}


# SMR01 data --------------------------------------------------------------
# Get the SMR01 data
source("extract-data/03_extract_smr01_data.R", echo = TRUE)

# NRS data --------------------------------------------------------------
# Get the Deaths data
source("extract-data/04_extract_nrs_deaths_data.R", echo = TRUE)

# Clear database connection
odbc::dbDisconnect(SMRA_connect)
rm(SMRA_connect)

# SICSAG data --------------------------------------------------------------
# Get the ICU data
source("extract-data/05_read_sicsag_icu_data.R", echo = TRUE)


# Clean up old local data -------------------------------------------------------
source("extract-data/99_remove-old-data.R", echo = TRUE)
