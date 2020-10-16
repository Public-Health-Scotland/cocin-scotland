###  - Run setup environment script ----

source("extract-data/00_setup-environment.R")


# Set correct filepath for server or desktop
sicsag_file_path <- path(
 stats_server_path(),
  "PHSCOVID19_Analysis", "RAPID Reporting", "Daily_extracts", "ICU Linkage files",
  "File for PHS.csv"
)

# Read in data
sicsag_data <- read_tsv(sicsag_file_path)
# quick check for file error
if (ncol(sicsag_data) == 1) stop("Probable error with sicsag file (try swapping between read_csv and read_tsv")

# write extract
# TODO add date to file path
write_rds(sicsag, path(here("data", str_glue("SICSAG_extract.rds"))), compress = "gz")