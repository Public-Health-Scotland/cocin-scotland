source("extract-data/00_setup-environment.R")

# Read in SMR01 extract
SMR01 <- read_rds(path(here("data", str_glue("SMR01_extract.rds"))))

# Prepare data for matching
SMR01 <- SMR01 %>%
  rename(chi_number = upi_number) %>%
  mutate(admission_date = as_date(admission_date),
         discharge_date = as_date(discharge_date)) %>%
  group_by(chi_number, link_no, cis_marker) %>%
  summarise(smr01_adm = min(admission_date),
            smr01_dis = max(discharge_date))

# Match on to data
data_smr01 <- rapid_data %>%
  left_join(SMR01, by = "chi_number") %>%
  select(chi_number, adm_date, dis_date, smr01_dis) %>%
  filter(smr01_dis < adm_date) %>%
  filter(smr01_dis > (adm_date - years(1))) %>%
  distinct(chi_number) %>% 
  mutate(prev_hosp_smr01 = "Yes")

# read in raw RAPID file
rapid <- read_rds(path(here("data", str_glue("rapid_ecoss_joined.rds"))))

# Prepare data for matching
rapid <- rapid %>%
  select(chi_number, admission_date, discharge_date) %>%
  rename(rapid_adm = admission_date,
         rapid_dis = discharge_date) %>%
  mutate(chi_number = as.character(chi_number),
  mutate(chi_number = ifelse(nchar(chi_number) == 9, paste0('0', chi_number),
                             chi_number))
         rapid_adm  = as_date(rapid_adm),
         rapid_dis  = as_date(rapid_dis)) %>%

# Match on to data
data_rapid <- rapid_data %>%
  left_join(rapid, by = "chi_number") %>%
  select(chi_number, adm_date, dis_date, rapid_adm, rapid_dis) %>%
  filter(rapid_dis < adm_date) %>%
  mutate(year_marker = adm_date - years(1)) %>%
  filter(rapid_dis > year_marker) %>%
  distinct(chi_number) %>% 
  mutate(prev_hosp_rapid = "Yes")

# Now match these CHIs on to full data
rapid_prevhosp <- rapid_data %>%
  left_join(data_smr01, by = "chi_number") %>%
  left_join(data_rapid, by = "chi_number") %>%
  mutate(prevhosp = ifelse(prev_hosp_smr01 == "Yes" | prev_hosp_rapid == "Yes",
                           "Yes", "No")) %>%
  mutate(prevhosp = ifelse(is.na(prevhosp), "No", prevhosp)) %>%
  select(chi_number, adm_date, prevhosp)

# remove datasets no longer needed
rm(SMR01, rapid, data_smr01, data_rapid)