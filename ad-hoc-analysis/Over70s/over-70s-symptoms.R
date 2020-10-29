## Over 70s analysis
## 08/05/2020
## Nicole Jarvie

## Run after 00-Get Scottish Data & 01-Prep data
## Add age band
## Still need to add in code to analysis any symptom differences between those patients admitted
## Before and after the 30th of april


scot_data <- scot_data %>%
  mutate(
    age.factor = case_when(
      age < 17 ~ "<17",
      age < 30 ~ "17-29",
      age < 40 ~ "30-39",
      age < 50 ~ "40-49",
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      is.na(age) ~ NA_character_,
      TRUE ~ "80+"
    )
  )


# Set-up ------------------------------------------------------------------

# Create lists of subjids who meet criteria
corna_confirmed <- scot_data %>%
  filter(corna_mbcat == "YES - Confirmed") %>%
  distinct(subjid) %>%
  pull(subjid)

corna_suspected <- scot_data %>%
  filter(corna_mbcat == "YES - Probable") %>%
  distinct(subjid) %>%
  pull(subjid)

corna_unknown <- scot_data %>%
  distinct(subjid) %>%
  filter(!subjid %in% c(corna_confirmed, corna_suspected)) %>%
  pull(subjid)

travelled <- scot_data %>%
  filter(travel_erterm == "Yes") %>%
  distinct(subjid) %>%
  pull(subjid)

died <- scot_data %>%
  filter(dsterm == "Death") %>%
  distinct(subjid) %>%
  pull(subjid)

male <- scot_data %>%
  filter(sex == "Male") %>%
  distinct(subjid) %>%
  pull(subjid)

female <- scot_data %>%
  filter(sex == "Female") %>%
  distinct(subjid) %>%
  pull(subjid)

# Breakdown of gender, age and death
# Used for population pyramid


pop_data <- scot_data %>%
  group_by(subjid) %>%
  mutate(died = if_else(subjid %in% died, 1, 0)) %>%
  summarise(
    age = first(na.omit(age.factor)),
    sex = first(na.omit(sex)),
    ethnicity = first(na.omit(ethnicity)),
    pregnancy = first(na.omit(pregyn_rptestcd)),
    admission = first(na.omit(cestdat)),
    outcome = first(na.omit(dsterm))
  ) %>%
  mutate(died = case_when(outcome == "Death" ~ "Yes", TRUE ~ "No") %>% factor(levels = c("No", "Yes")))

age_summary <- pop_data %>%
  group_by(age) %>%
  summarise(total_cases = n())

admission_data <- pop_data %>%
  filter(age %in% c("70-79", "80+")) %>%
  ## filter(admission <= today() ) %>%
  mutate(
    admissionweek =
      case_when(
        admission < "2020-04-30" ~ "Before 30April",
        admission >= "2020-04-06" ~ "On or After 30April",
        is.na(admission) ~ NA_character_,
        TRUE ~ "NA"
      )
  ) %>%
  group_by(admissionweek) %>%
  summarise(total_cases = n())



over70s <- pop_data %>%
  filter(age %in% c("70-79", "80+")) %>%
  mutate(
    admissionweek =
      case_when(
        admission < "2020-04-30" ~ "Before 30April",
        admission >= "2020-04-06" ~ "On or After 30April",
        is.na(admission) ~ NA_character_,
        TRUE ~ "NA"
      )
  ) %>%
  group_by(admissionweek, age) %>%
  summarise(total_cases = n())
