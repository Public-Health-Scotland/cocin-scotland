source("extract-data/00_setup-environment.R")

# Read in RAPID data

# Read in RAPID data
rapid_data <- read_rds(path(here(
  "data",
  str_glue("{today()}_RAPID-cleaned-filtered.rds")
)))

### Create Linkages

# COCIN (also includes all RAPID data)
source("linked_extracts/linkage_prep/linkage_cocin.R", echo = TRUE)

# Co-morbid Conditions (LTCs) - improve COCIN coverage
source("linked_extracts/linkage_prep/ltc_data.R", echo = TRUE)

# SICSAG
source("linked_extracts/linkage_prep/linkage_sicsag.R", echo = TRUE)

# SMR01
source("linked_extracts/linkage_prep/linkage_smr01_rapid.R", echo = TRUE)

# NRS
source("linked_extracts/linkage_prep/linkage_nrs.R", echo = TRUE)

### Link on all datasets
data <- list(rapid_cocin, rapid_icu, rapid_prevhosp, rapid_deaths) %>%
  reduce(left_join, by = c("chi_number", "adm_date"))

# write dataset
# TODO add date to file path
write_rds(data, path(here("data", "Linked_Dataset.rds")),
  compress = "gz"
)

# remove datasets not required
rm(rapid_data, rapid_cocin, rapid_deaths, rapid_icu, rapid_prevhosp)
