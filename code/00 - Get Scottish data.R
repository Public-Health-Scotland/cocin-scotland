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

## The API call fail randomly due to traffic
## Try 5 times then stop
Sys.setenv(ccp_token = rstudioapi::showPrompt(
  title = "Enter API token",
  message = "API token:"
))
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

## Add on Location details for Scottish hospitals where we can
# Lookup created by create_scottish_lookups.R
scot_locations <- read_csv("lookups/scot_locations.csv")

extract <- extract %>%
  mutate(hospid = str_sub(subjid, end = 5)) %>%
  left_join(scot_locations, by = c("hospid" = "Location"))

## Work out what the Scottish data access groups are
scot_groups <- extract %>%
  filter(!is.na(Locname)) %>%
  distinct(redcap_data_access_group) %>%
  write_csv("lookups/scottish_data_access_groups.csv")

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
rm(extract, data, tries)
