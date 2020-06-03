# Packages
library(REDCapR)
library(dplyr)
library(magrittr)
library(janitor)
library(stringr)
library(lubridate)
library(readr)

library(phsmethods) # Install from Github with remotes::install_github("Health-SocialCare-Scotland/phsmethods")

# Store the API token as a sytem env variable
# Other options would be;
# put the it in your Renviron file (if you can edit it) - ccp_token = <API Token>
# Read from a text file which is stored in a private location
# Enter API Token
Sys.setenv(
  ccp_token =
    rstudioapi::showPrompt(
      title = "Enter API token",
      message = "API token:"
    )
)

# Call API allowing for up to 5 tries 
tries <- 0
extract <- NA

## Note - need to come off the VPN connection for the below (Rserver works though)
while (tries == 0 | (tries < 5 & inherits(extract, "try-error"))) {
  
  # Avoid using the API on the hour as this is when a lot of reports refresh
  while (minute(Sys.time()) %in% c(59, 0:5)) {
    message("Waiting till after the hour to avoid overloading the API")
    Sys.sleep(30)
  }
  
  print(tries)
  extract <- try(extract <- redcap_read(
    redcap_uri = "https://ncov.medsci.ox.ac.uk/api/",
    export_data_access_groups = TRUE,
    token = Sys.getenv("ccp_token"),
    fields = c("subjid", "nhs_chi")
  )$data)
  tries <- tries + 1
  Sys.sleep(30)
}

### 2 - Select Scottish data ----

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
  left_join(
    read_csv(
      paste0(
        "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-",
        "b534-d6e17229cc7b/resource/f177be64-e94c-4ddf-a2ee-ea58d648d55a/",
        "download/hb2019_codes_and_labels_21042020.csv"
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
  mutate(hospid = str_sub(subjid, end = 5)) %>%
  left_join(scot_locations, by = c("hospid" = "location"))

valid_scotpat <- extract %>%
  group_by(subjid) %>% 
  summarise(nhs_chi = first(na.omit(nhs_chi))) %>% 
  filter(chi_check(nhs_chi) == "Valid CHI") %>% 
  pull(subjid)

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
    redcap_uri = "https://ncov.medsci.ox.ac.uk/api/",
    export_data_access_groups = TRUE,
    token = Sys.getenv("ccp_token"),
    records = valid_scotpat
  )$data
  tries <- tries + 1
  Sys.sleep(10)
}

# Record extract time
extract_date <- Sys.time()

# Fix bad location codes
extract %<>%
  mutate(subjid = str_replace(subjid, "S341H(-\\d+)$", "S314H\\1"),
         subjid = str_replace(subjid, "N100H(-\\d+)$", "N101H\\1"),
         subjid = str_replace(subjid, "GN405(-\\d+)$", "G405H\\1"),
         subjid = str_replace(subjid, "SL116(-\\d+)$", "S116H\\1"))

write_csv(
  extract,
  str_glue("data/cocin_edris_raw_{date}.csv", date = format(extract_date, "%Y-%m-%d_%H-%M"))
)

system(str_glue("gzip data/cocin_edris_raw_{date}.csv", date = format(extract_date, "%Y-%m-%d_%H-%M")))
