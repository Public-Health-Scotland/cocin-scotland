###########################################################
# Name of script: 01_get-scottish-data.R
# Written by: Analysts working in HPS Enhanced Surveillance
#             Cell - Hospital/ICU Work Stream
# Credit to: University of Edinburgh Surgical Informatics
#
# Type of script: Data Extraction
# Written/run on: R Studio Desktop
# Version of R: 3.6.1
#
# Description: Extract data from CoCIN RedCap database
#              and select Scotland records only
###########################################################


### 0 - Run setup environment script ----

source("extract-data/00_setup-environment.R")


### 1 - Extract patient id data from RedCap via API ----

# Enter API Token
if (is.na(Sys.getenv("ccp_token", unset = NA))) {
  Sys.setenv(
    ccp_token =
      rstudioapi::showPrompt(
        title = "Enter API token",
        message = "API token:"
      )
  )
} else {
  message("Using stored API token - run Sys.setenv(ccp_token = \"<API_TOKEN>\") to reset it")
}


## Note - need to come off the VPN connection for the below

### 2 - Extract Scottish data ----


# Avoid using the API on the hour as this is when a lot of reports refresh
while (minute(Sys.time()) %in% c(59, 0:5)) {
  message("Waiting till after the hour to avoid overloading the API")
  Sys.sleep(30)
}

# TODO add in a filter to only include variables we need here.
# variables <- c("subjid", "...")

extract <- redcap_read(
  redcap_uri = "https://ncov.medsci.ox.ac.uk/api/",
  export_data_access_groups = TRUE,
  token = Sys.getenv("ccp_token"),
  # fields = variables
)$data %>%
  as_tibble()

# Fix bad location codes
extract %<>%
  fix_bad_loc_codes()

# Create scottish location lookup
scot_locations <-
  # Extract Scottish hospital location codes
  read_csv(
    paste0(
      "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-",
      "d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/",
      "download/current_nhs_hospitals_in_scotland_200420.csv"
    ),
    col_types = cols_only(
      Location = col_character(),
      LocationName = col_character(),
      HB = col_character()
    )
  ) %>%
  # Extract health board names and join
  dplyr::left_join(
    read_csv(
      paste0(
        "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/",
        "resource/652ff726-e676-4a20-abda-435b98dd7bdc/",
        "download/hb14_hb19.csv"
      ),
      col_types = cols_only(
        HB = col_character(),
        HBName = col_character()
      )
    ),
    by = "HB"
  ) %>%
  clean_names()

# Match lookup to CoCIN extract
extract %<>%
  dplyr::mutate(hospid = str_sub(subjid, end = 5)) %>%
  dplyr::left_join(scot_locations, by = c("hospid" = "location"))


### 3 - Run factor/label code ----

data <- extract
source("extract-data/01a_cocin_factors.R")
extract <- data
rm(data)


### 4 - Save data extract and record summary ----

# Data extract
write_rds(
  extract,
  here("data", str_glue("{today()}_cocin_extract.rds")),
  compress = "gz"
)

# Summary of records by location
write_csv(
  extract %>%
    dplyr::count(hb_name, redcap_data_access_group, hospid, location_name),
  here("data", str_glue("{today()}_cocin_summary.csv"))
)

# Write current metadata for reference
redcap_metadata_read(
  redcap_uri = "https://ncov.medsci.ox.ac.uk/api/",
  token = Sys.getenv("ccp_token")
)$data %>%
  write_csv(here("data", str_glue("{today()}_cocin_metadata.csv")))

rm(scot_locations, extract)


### END OF SCRIPT ###
