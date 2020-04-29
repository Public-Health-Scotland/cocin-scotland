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
