source("extract-data/00_setup-environment.R")

# Read in SMR01 extract
NRS <- read_rds(path(here("data", str_glue("NRS_extract.rds"))))

# Check whether patient in RAPID data has died and DOD
deaths1 <- rapid_data %>%
  inner_join(NRS, by = "chi_number") %>%
  mutate(death = "Yes") %>%
  select(chi_number, adm_date, death, date_of_death)

# Check whether patient died of COVID or not
deaths2 <- rapid_data %>%
  inner_join(NRS, by = "chi_number") %>%
  filter_at(vars(contains("cause_of_death")), any_vars(str_detect(., "U07"))) %>%
  mutate(covid_death = "Yes") %>%
  select(chi_number, adm_date, covid_death)

# Join together
deaths <- deaths1 %>%
  left_join(deaths2, by = c("chi_number", "adm_date")) %>%
  mutate(covid_death = if_else(is.na(covid_death), "No", covid_death))

# Join on to all data
rapid_deaths <- rapid_data %>%
  left_join(deaths, by = c("chi_number", "adm_date")) %>%
  mutate(date_of_death = as_date(date_of_death)) %>%
  select(chi_number, adm_date, death, date_of_death, covid_death)

rm(NRS, deaths, deaths1, deaths2)
