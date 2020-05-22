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
Sys.setenv(
  ccp_token = 
    rstudioapi::showPrompt(
      title = "Enter API token",
      message = "API token:"
    ))


# Call API allowing for up to 5 tries 
tries <- 0
extract <- NA

## Note - need to come off the VPN connection for the below 
while (tries == 0 | (tries < 5 & inherits(extract, "try-error"))) {
  
  # Avoid using the API on the hour as this is when a lot of reports refresh
  while (minute(Sys.time()) %in% c(59, 0:5)) {
    message("Waiting till after the hour to avoid overloading the API")
    Sys.sleep(30)
  }
  
  print(tries)
  extract <- try(extract <- redcap_read(
    redcap_uri  = "https://ncov.medsci.ox.ac.uk/api/",
    export_data_access_groups = TRUE,
    token       = ccp_token,
    fields      = 'subjid',
  )$data
  )
  tries <- tries + 1
  Sys.sleep(10)
}

# Read csv extract
if (class(extract) == "character") {
  extract <- read_csv(extract, na = "", guess_max = 20000)
  extract_date <- Sys.time()
} else {
  warning("Something went wrong with the extract")
}


### 2 - Select Scottish data ----

# Fix bad location codes
extract %<>%
  fix_bad_loc_codes()

# Create scottish location lookup
scot_locations <-
  
  # Extract Scottish hospital location codes
  read_csv(
    paste0("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-",
           "d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/",
           "download/current_nhs_hospitals_in_scotland_200420.csv")
  ) %>%
  
  # Extract health board names and join
  left_join(
    read_csv(
      paste0("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-",
             "b534-d6e17229cc7b/resource/f177be64-e94c-4ddf-a2ee-ea58d648d55a/",
             "download/hb2019_codes_and_labels_21042020.csv")
    ),
    by = "HB"
  ) %>%
  
  clean_names()

# Match lookup to CoCIN extract
extract %<>%
  mutate(hospid = str_sub(subjid, end = 5)) %>%
  left_join(scot_locations, by = c("hospid" = "location"))

# Extract list of Data Access Groups (DAG) associated with Scottish locations
scot_dag <-
  extract %>%
  filter(!is.na(location_name)) %>%
  distinct(redcap_data_access_group)

# Select CoCIN records 
extract %<>%
  filter(redcap_data_access_group %in% scot_dag |
           !is.na(location_name))

## Download COCIN data for all Scottish patients

scotpat <-  unique(extract$subjid)

# Call API allowing for up to 5 tries 
tries <- 0
extract <- NA

## Note - need to come off the VPN connection for the below 
while (tries == 0 | (tries < 5 & inherits(extract, "try-error"))) {
  
  # Avoid using the API on the hour as this is when a lot of reports refresh
  while (minute(Sys.time()) %in% c(59, 0:5)) {
    message("Waiting till after the hour to avoid overloading the API")
    Sys.sleep(30)
  }
  
  print(tries)
  extract <- redcap_read(
    redcap_uri  = "https://ncov.medsci.ox.ac.uk/api/",
    export_data_access_groups = TRUE,
    token       = ccp_token,
    records = scotpat
  )$data
  tries <- tries + 1
  Sys.sleep(10)
}

# Fix bad location codes

extract %<>%
  fix_bad_loc_codes()

# Create scottish location lookup
scot_locations <-
  
  # Extract Scottish hospital location codes
  read_csv(
    paste0("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-",
           "d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/",
           "download/current_nhs_hospitals_in_scotland_200420.csv")
  ) %>%
  
  # Extract health board names and join
  left_join(
    read_csv(
      paste0("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-",
             "b534-d6e17229cc7b/resource/f177be64-e94c-4ddf-a2ee-ea58d648d55a/",
             "download/hb2019_codes_and_labels_21042020.csv")
    ),
    by = "HB"
  ) %>%
  
  clean_names()

# Match lookup to CoCIN extract
extract %<>%
  mutate(hospid = str_sub(subjid, end = 5)) %>%
  left_join(scot_locations, by = c("hospid" = "location"))


### 3 - Run factor/label code ----

data <- extract
source("extract-data/CCPUKSARI_R_2020-03-04_1532.R")
extract <- data
rm(data)


### 4 - Save data extract and record summary ----

# Data extract
write_rds(
  extract,
  here("data", paste0(format(extract_date, "%Y-%m-%d_%H-%M"), 
                      "_scot-data.rds")),
  compress = "gz"
)

# Summary of records by location
write_csv(
  extract %>%
    count(hb_name, redcap_data_access_group, hospid, location_name),
  here("data", paste0(format(extract_date, "%Y-%m-%d_%H-%M"), 
                      "_scot-record-summary.csv"))
)


### END OF SCRIPT ###