###  - Run setup environment script ----

source("extract-data/00_setup-environment.R")

library(haven)


# Set correct filepath for server or desktop
sicsag_file_path <- path(
  stats_server_path(),
  "PHSCOVID19_Analysis", "Hospital_linked_dataset", "SICSAG File", 
  "SICSAG data approved for SARI.sav"
)

# Read in data
sicsag_data <- read_sav(sicsag_file_path)
# quick check for file error
if (ncol(sicsag_data) == 1) stop("Probable error with sicsag file (try swapping between read_csv and read_tsv")

# write extract
# TODO add date to file path
write_rds(sicsag_data, path(here("data", str_glue("{today()}_SICSAG_extract.rds"))), compress = "gz")

# Copy to server
sicsag_data_local <- path(here("data", str_glue("{today()}_SICSAG_extract.rds")))

if (file_exists(sicsag_data_local)) {
  file_copy(
    path = sicsag_data_local,
    new_path = path(server_dir, str_glue("{today()}_SICSAG_extract.rds"))
  )
}

# Clean up
rm(sicsag_file_path, sicsag_data)
