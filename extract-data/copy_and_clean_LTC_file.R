# Load packages
source("extract-data/00_setup-environment.R")

# Set LTC file path from hscdiip - requires Unix hscdiip access
ltc_hscdiip_file <- "SCTASK0168597_extract_1_LTCs.csv.gz"
ltc_hscdiip_path <- path("/conf", "hscdiip", "IT extracts", ltc_hscdiip_file)

# Record the month - file is updated roughly every quarter
extract_month <- file_info(ltc_hscdiip_path)$birth_time %>% month(label = TRUE)

# Read the file (large ~1GB)
ltc_data <- read_csv(ltc_hscdiip_path)

# Tidy up the variables
ltc_data <- ltc_data %>%
  janitor::clean_names() %>%
  select(
    "patient_upi_c",
    "patient_postcode_c",
    "asthma_diag_date",
    "cancer_diag_date",
    "chron_liver_dis_diag_date",
    "copd_diag_date",
    "dementia_diag_date",
    "diabetes_diag_date",
    "heart_disease_diag_date",
    "renal_failure_diag_date",
    "blood_and_bfo_diag_date"
  ) %>%
  mutate(
    across(ends_with("_date"),
      parse_date_time2,
      exact = TRUE, orders = "%d-%m-%Y"
    ),
    across(ends_with("_date"), as_date)
  ) %>%
  rename_with(~ str_remove(.x, "_diag_date")) %>%
  rename_with(~ str_remove(.x, "_c")) %>%
  rename_with(~ paste0("ltc_", .x))


# Write out compressed to the server folder
write_rds(ltc_data,
  path(server_dir, str_glue("ltc_data-extracted_{extract_month}.rds")),
  compress = "xz"
)

# Clean up
rm(ltc_data, ltc_hscdiip_file, ltc_hscdiip_path, extract_month)
