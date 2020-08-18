source("extract-data/00_setup-environment.R")
source("linked_extracts/00_all_extracts.R")

# Read in RAPID data

# define date of RAPID extraction
rapid_date <- file_info(path(here("data"),  "rapid_ecoss_joined.rds")) %>%
  pull(modification_time) %>%
  date()

# read in RAPID data
rapid_data <- read_rds(path(here("data", str_glue("{rapid_date}_RAPID-cleaned-filtered.rds"))))

### Create Linkages

# COCIN (also includes all RAPID data)
source("ad-hoc-analysis/linkage_COCIN.R")

# SICSAG
source("ad-hoc-analysis/linkage_sicsag.R")

# SMR01
source("ad-hoc-analysis/linkage_smr01_rapid.R")

# NRS
source("ad-hoc-analysis/linkage_nrs.R")


### Link on all datasets
data <- rapid_cocin %>%
  left_join(rapid_icu, by = c("chi_number","adm_date")) %>%
  left_join(rapid_prevhosp, by = c("chi_number","adm_date")) %>%
  left_join(rapid_deaths, by = c("chi_number","adm_date"))

# write dataset
write_rds(data, path(here("data", str_glue("Linked_Dataset.rds"))), 
          compress = "gz")

# remove datasets not required
rm(rapid_data, rapid_cocin, rapid_deaths, rapid_icu, rapid_prevhosp)

### IMOVE Recode
source("ad-hoc-analysis/IMOVE_Recode.R")

# write dataset
write_rds(IMOVE_data, path(here("data", str_glue("IMOVE_data.rds"))), 
          compress = "gz")

### Genomics Recode
source("ad-hoc-analysis/Genomics_Recode.R")

write_rds(genomics_hospdata, path(here("data", str_glue("Genomics_hospdata.rds"))), 
          compress = "gz")
