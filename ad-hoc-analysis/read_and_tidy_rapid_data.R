source("extract-data/00_setup-environment.R")

# Read data found here: 
# \\stats\PHSCOVID19_Analysis\RAPID Reporting\Daily_extracts
linked_file_path <- path("//stats", "PHSCOVID19_Analysis", "RAPID Reporting", "Daily_extracts", "rapid_ecoss_joined.rds")
if (vpn_active()) {
  message("Connection to NHS network is active")
  if (file_exists(linked_file_path)) {
    server_last_modified <- file_info(linked_file_path) %>%
      pull(modification_time)

    local_last_modified <- file_info("data/rapid_ecoss_joined.rds") %>%
      pull(modification_time)

    if (date(server_last_modified) > date(local_last_modified) | is.na(local_last_modified)) {
      message(str_glue("The server file looks newer (modified:{server_date}), so replacing local file (modified:{local_date})",
        server_date = date(server_last_modified),
        local_date = date(local_last_modified)
      ))
      file_copy(linked_file_path, "data/rapid_ecoss_joined.rds", overwrite = TRUE)
      
      local_last_modified <- server_last_modified
      
      message("New file copied")
    } else {
      message(str_glue("Local file is current (modified:{local_date})",
        local_date = date(local_last_modified)
      ))
    }
  } else {
    message("File 'rapid_ecoss_joined.rds' does not exist")
  }
} else {
  message("Not connected to NHS network so can't check for new file")
}

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
    age = first(age_year),
    sex = first(sex),
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
# Take the latest stay if needed
test_in_stay <- rapid %>%
  filter((adm_date <= test_date & dis_date >= test_date) |
    (is.na(dis_date) & (test_date >= adm_date))) %>%
  arrange(desc(adm_date)) %>%
  distinct(chi_number, .keep_all = TRUE)

# Find patients who had a positive test before an admision and take that patients latest admission
test_before_stay <- rapid %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  filter((test_date < adm_date)) %>%
  arrange(desc(adm_date)) %>%
  distinct(chi_number, .keep_all = TRUE)

# Exclude any who have tested positive after the latest discharge we have for them
test_after_dis <- rapid %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  anti_join(test_before_stay, by = "chi_number") %>%
  group_by(chi_number) %>%
  filter(test_date > max(dis_date)) %>%
  ungroup()

# See what we have - should be no records left
rapid %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  anti_join(test_before_stay, by = "chi_number") %>%
  anti_join(test_after_dis, by = "chi_number") %>%
  View()

# Create a dataset of single admission per CHI
covid_admissions <- bind_rows(test_in_stay, test_before_stay)

# Read COCIN data
cocin <- read_rds(str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date()))

# Create completness per hospital
hosp_completeness <- full_join(
  cocin %>%
    distinct(subjid, hospid) %>%
    count(hospid),
  covid_admissions %>%
    count(hospital_of_treatment_code),
  by = c("hospid" = "hospital_of_treatment_code")
) %>%
  rename(
    cocin_patients = n.x,
    rapid_patients = n.y
  ) %>%
  mutate(
    cocin_patients = if_else(is.na(cocin_patients), 0L, cocin_patients),
    pct_complete = cocin_patients / rapid_patients * 100
  )

# Create completeness by age
age_completness <- 
  full_join(
    cocin %>%
      group_by(subjid) %>% 
      summarise(age = first(na.omit(age))) %>% 
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
      ) %>% 
      count(age.factor),
    covid_admissions %>%
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
      ) %>% 
      count(age.factor),
    by = c("age.factor")
  ) %>%
  rename(
    cocin_patients = n.x,
    rapid_patients = n.y
  ) %>%
  mutate(
    cocin_patients = if_else(is.na(cocin_patients), 0L, cocin_patients),
    pct_complete = cocin_patients / rapid_patients * 100
  )


# Create completeness by sex
sex_completness <- 
  full_join(
    cocin %>%
      group_by(subjid) %>% 
      summarise(sex = as.numeric(first(na.omit(sex)))) %>% 
      count(sex),
    covid_admissions %>%
      mutate(sex = case_when(sex == "M" ~ 1,
                             sex == "F" ~ 2)) %>% 
      count(sex),
    by = c("sex")
  ) %>%
  rename(
    cocin_patients = n.x,
    rapid_patients = n.y
  ) %>%
  mutate(
    cocin_patients = if_else(is.na(cocin_patients), 0L, cocin_patients),
    pct_complete = cocin_patients / rapid_patients * 100
  )

## Todo 
# Make breakdown of hospital per ISO week per sex and pead/ adult/ old
# Last weeks CHIs RAPID + ECOSS vs COCIN



