library(readr)
library(dplyr)
library(lubridate)

# Read data emailed from Bob
rapid <- read_rds(here("data", "rapid_ecoss_joined.rds")) %>%
  select(
    chi_number,
    age_year,
    sex,
    diagnosis_1_code_4_char,
    diagnosis_2_code_4_char,
    diagnosis_3_code_4_char,
    diagnosis_4_code_4_char,
    diagnosis_5_code_4_char,
    diagnosis_6_code_4_char,
    temporal_link_id,
    location_link_id,
    admission_date,
    discharge_date,
    hospital_of_treatment_code,
    health_board_of_treatment,
    specimen_date,
    result
  )

# Aggregate to 'stay' level - this just uses a marker Bob created which tags episodes which are close in time
# Note we don't group episodes which change hospitals as COCIN CRFs are single hospital
rapid <- rapid %>%
  filter(result == 1) %>%
  group_by(chi_number, temporal_link_id, location_link_id, hospital_of_treatment_code) %>%
  summarise(
    adm_date = min(admission_date),
    dis_date = max(discharge_date),
    test_date = first(specimen_date),
    dis1 = last(na.omit(diagnosis_1_code_4_char)),
    dis2 = last(na.omit(diagnosis_2_code_4_char)),
    dis3 = last(na.omit(diagnosis_3_code_4_char)),
    dis4 = last(na.omit(diagnosis_4_code_4_char)),
    dis5 = last(na.omit(diagnosis_5_code_4_char)),
    dis6 = last(na.omit(diagnosis_6_code_4_char))
  ) %>%
  ungroup()

# Want to get one stay per patient
# Filter off as we find a sensible match

# Find paitents who had a positive test during the stay and use that
test_in_stay <- rapid %>%
  filter((adm_date <= test_date & dis_date >= test_date)|
      (is.na(dis_date) & (test_date >= adm_date))
  )

# Find patients who had a positive test in the 1/2 days before admission
# Maybe we should do this one last and increase the time
test_before_stay <- rapid %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  filter((test_date < adm_date) & ((test_date + days(2)) >= adm_date))

# This is an odd group - was considering excluding these
test_after_dis <- rapid %>% 
  anti_join(test_in_stay, by = "chi_number") %>% 
  anti_join(test_before_stay, by = "chi_number") %>% 
  group_by(chi_number) %>% 
  filter(test_date > max(dis_date)) %>% 
  ungroup()

# See what we have
rapid %>% 
  anti_join(test_in_stay, by = "chi_number") %>% 
  anti_join(test_before_stay, by = "chi_number") %>% 
  anti_join(test_after_dis, by = "chi_number") %>%
  View()

# Other thoughts for filtering is to use diag codes and look for
# U071 (Emergency code for COVID) or B342 (Code for COVID)

# Last thought would be to pick the longest, closest stay to the test date

