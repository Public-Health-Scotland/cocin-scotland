## CoCIN Data Extraction 
## Enhanced Surveillance Cell 
## ICU and Hospital Work Stream 
## Analytical Team 

## Pull data

# Libraries
library(RCurl)
library(tidyverse)
library(lubridate)
library(finalfit)
library(tidylog)
library(Hmisc)

## API pull
# If the file with the API token doesn't exist create it now
if (!file.exists("stored_ccp_token.txt")) {
  message("stored_ccp_token.txt doesn't exist, creating it now.")
  file.create("stored_ccp_token.txt")

  readline(prompt = "Enter API token:") %>% 
    write_lines("stored_ccp_token.txt")
}

# Read the token from file
Sys.setenv("ccp_token" = read_lines("stored_ccp_token.txt"))

## The API call fail randomly due to traffic
## Try 5 times then stop
tries <- 0
extract <- NA

## Note - need to come off the VPN connection for the below 
while (tries == 0 | (tries < 5 & inherits(extract, "try-error"))) {

  # Avoid using the API on the hour as this is when a lot of reports refresh
  while (minute(Sys.time()) %in% c(59, 0:5)) {
    message("Waiting till after the hour to avoid overloading the API")
    Sys.sleep(30)
  }

  extract <- try(postForm(
    uri = "https://ncov.medsci.ox.ac.uk/api/",
    token = Sys.getenv("ccp_token"),
    content = "record",
    format = "csv",
    type = "flat",
    rawOrLabel = "raw",
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "false",
    exportSurveyFields = "false",
    exportDataAccessGroups = "true",
    returnFormat = "json"
  ))
  tries <- tries + 1
  # let's wait to let the API cool off
  Sys.sleep(10)
}

if (class(extract) == "character") {
  extract <- read_csv(extract, na = "", guess_max = 20000)
} else {
  warning("Something went wrong with the extract")
}

# Store extract date
extract_date <- Sys.time()

## Add on Location details for Scottish hospitals where we can
# Get Hosptial Lookup and Board names from the NHS Scotland Opendata platform

# Hospital codes
# https://www.opendata.nhs.scot/dataset/hospital-codes/resource/c698f450-eeed-41a0-88f7-c1e40a568acc
scot_locations <- read_csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current_nhs_hospitals_in_scotland_200420.csv",
                           col_types = c("ccc-c----"))

# Board names (using 2019 specification)
# https://www.opendata.nhs.scot/dataset/geography-codes-and-labels/resource/f177be64-e94c-4ddf-a2ee-ea58d648d55a
hb_names <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/f177be64-e94c-4ddf-a2ee-ea58d648d55a/download/hb2019_codes_and_labels_21042020.csv",
                     col_types = "cc-")

# Join on HB names
scot_locations <- left_join(scot_locations, hb_names, by = "HB") %>% 
  # Do some renaming to make it match old lookup so we don't have to change other code
  rename(Locname = LocationName,
         HB2019Name = HBName,
         HB2019 = HB)

extract <- extract %>%
  mutate(hospid = str_sub(subjid, end = 5)) %>%
  left_join(scot_locations, by = c("hospid" = "Location"))

## Work out what the Scottish data access groups are
scot_groups <- extract %>%
  filter(!is.na(Locname)) %>%
  distinct(redcap_data_access_group)

# Filter to Scottish data
scot_data <- extract %>%
  inner_join(scot_groups, by = "redcap_data_access_group") %>%
  # Had a Scottish hosps with data access group = NA
  filter(!is.na(redcap_data_access_group) | hospid %in% pull(scot_locations, Location))

# Check data
scot_data %>%
  count(HB2019Name, redcap_data_access_group, hospid, Locname) %>%
  View()

# Run UoE labelling code (should probably tidy this up to figure out what is actually needed)
data <- scot_data
source("CCPUKSARI_R_2020-03-04_1532.R")
scot_data <- data

# Clean up
rm(extract, data, tries, hb_names)
