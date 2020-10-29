source("extract-data/00_setup-environment.R")

# Read in SMR01 extract
nrs <- read_rds(path(here("data", str_glue("NRS_extract.rds"))))

# Check whether patient in RAPID data has died and DOD
deaths <- nrs %>%
  # Remove invalid (mostly missing) CHIs
  filter(chi_check(chi_number) == "Valid CHI") %>%
  # Filter deaths to only chis which appear in RAPID
  semi_join(rapid_data, by = "chi_number") %>%
  # Check whether patient died of COVID or not
  left_join(filter_at(., vars(contains("cause_of_death")), any_vars(str_detect(., "U07"))) %>%
    mutate(covid_death = "Yes") %>%
    select(chi_number, covid_death)) %>%
  mutate(death = "Yes")

# Join on to all data
rapid_deaths <- rapid_data %>%
  left_join(deaths, by = c("chi_number")) %>%
  mutate(date_of_death = as_date(date_of_death)) %>%
  select(chi_number, adm_date, death, date_of_death, covid_death) %>%
  replace_na(list(death = "No", covid_death = "No"))

rm(nrs, deaths)
