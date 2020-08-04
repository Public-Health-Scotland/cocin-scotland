source("extract-data/00_setup-environment.R")

# Export COCIN data
# Note you will need an API key (a pop-up will ask for it)


# COCIN data ------------------------------------------------------------

# Note it will extract maximum once a week
if (floor_date(today(), "week") > floor_date(ymd_hm(latest_extract_date()), "week")) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

# Copy to server
cocin_data <- path(here("data", str_glue("{latest_extract_date()}_scot-data-clean.rds")))
cocin_metadata <- path(here("data", str_glue("{date(latest_extract_date())}_COCIN-metadata.csv")))

if (file_exists(cocin_data)) {
  file_copy(
    path = cocin_data,
    new_path = path(server_dir, str_glue("{date(latest_extract_date())}_cocin-clean-data.rds"))
  )

  if (file_exists(cocin_metadata)) {
    file_copy(
      path = cocin_metadata,
      new_path = path(server_dir, str_glue("{date(latest_extract_date())}_cocin-metadata.csv"))
    )
  }
}

# RAPID data --------------------------------------------------------------
# Get the RAPID data
source("extract-data/03_read_RAPID_data.R")
source("ad-hoc-analysis/tidy_RAPID_data.R")

# Copy to server
rapid_data <- path(here("data", str_glue("{date(latest_extract_date())}_RAPID-cleaned-filtered.rds")))


if (file_exists(rapid_data)) {
  file_copy(
    path = rapid_data,
    new_path = path(server_dir, str_glue("{date(latest_extract_date())}_RAPID-cleaned-filtered.rds"))
  )
}


# Clean up old local data -------------------------------------------------------
source("extract-data/99_remove-old-data.R")
