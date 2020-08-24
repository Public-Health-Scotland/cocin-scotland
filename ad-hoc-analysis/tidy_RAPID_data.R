source("extract-data/00_setup-environment.R")

# Read COCIN data
cocin_with_chi <- read_rds(path(server_dir, str_glue("{date}_cocin-clean-data.rds", date = latest_server_data("cocin")))) %>%
  select(subjid, nhs_chi, hostdat, dsstdtc) %>%
  mutate_at(vars(hostdat, dsstdtc), as_date) %>%
  group_by(subjid) %>%
  mutate(
    chi_number = first(na.omit(nhs_chi)),
    cocin_adm = first(na.omit(hostdat)),
    cocin_dis = first(na.omit(dsstdtc))
  ) %>%
  summarise_at(vars(chi_number, cocin_adm, cocin_dis), first) %>%
  filter(!is.na(chi_number), !is.na(cocin_adm))


# Read in the RAPID file and select variables we need
rapid <- read_rds(here("data", "rapid_ecoss_joined.rds")) %>%
  select(
    chi_number,
    rapid_id,
    patient_dob,
    age_year,
    patient_gender_description,
    sex,
    patient_ethnic_group_description,
    patient_ethnic_group_code,
    patient_postcode,
    health_board_of_residence,
    health_board_of_treatment,
    admitted_transfer_from_type,
    discharge_type,
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
    ecossid,
    forename,
    surname,
    specimen_date,
    result
  )

# Aggregate to 'stay' level - this just uses a marker Bob created which tags episodes which are close in time
# Note we don't group episodes which change hospitals as COCIN CRFs are single hospital
rapid_stay_level <- rapid %>%
  group_by(chi_number, temporal_link_id, location_link_id) %>%
  summarise(
    dob = first(na.omit(patient_dob)),
    age = first(age_year),
    postcode = first(na.omit(patient_postcode)),
    health_board_of_residence = first(na.omit(health_board_of_residence)),
    health_board_of_treatment = first(na.omit(health_board_of_treatment)),
    sex = first(na.omit(patient_gender_description)),
    ethnicity = first(na.omit(patient_ethnic_group_description)),
    ethnicity_code = first(na.omit(patient_ethnic_group_code)),
    rapid_id = first(rapid_id),
    adm_date = min(admission_date),
    dis_date = max(discharge_date),
    hospital_of_treatment_code = last(hospital_of_treatment_code),
    admitted_transfer_from_type = first(na.omit(admitted_transfer_from_type)),
    discharge_type = last(na.omit(discharge_type)),
    diag_1 = first(na.omit(diagnosis_1_code_4_char)),
    diag_2 = first(na.omit(diagnosis_2_code_4_char)),
    diag_3 = first(na.omit(diagnosis_3_code_4_char)),
    diag_4 = first(na.omit(diagnosis_4_code_4_char)),
    diag_5 = first(na.omit(diagnosis_5_code_4_char)),
    diag_6 = first(na.omit(diagnosis_6_code_4_char)),
    result = first(result), # Result is the result of the PCR test from ECOSS
    ecossid = first(ecossid),
    test_date = first(specimen_date),
    forename = first(forename),
    surname = first(surname)
  ) %>%
  ungroup() %>%
  # If we can calculate the age from the RAPID dob and adm_date
  # Otherwise use the age which came from ECOSS
  mutate(age = if_else(!is.na(time_length(dob %--% adm_date, "years")),
    as.integer(floor(time_length(dob %--% adm_date, "years"))),
    age
  ))


# Identify the records with multiple admissions so we can choose one
cocin_matched <- rapid_stay_level %>%
  group_by(chi_number) %>%
  filter(n() > 1) %>%
  left_join(cocin_with_chi, by = c("chi_number")) %>%
  filter(!is.na(subjid)) %>%
  mutate(
    adm_match = if_else(abs(time_length(adm_date %--% cocin_adm, unit = "days")) <= 1, TRUE, FALSE),
    dis_match = if_else(abs(time_length(dis_date %--% cocin_dis, unit = "days")) <= 1, TRUE, FALSE)
  ) %>%
  replace_na(list(adm_match = FALSE, dis_match = FALSE)) %>%
  # Select the admission because the dates match COCIN
  mutate(select_adm = if_else(adm_match & dis_match, TRUE, FALSE)) %>%
  # Mark the CHI as done
  mutate(chi_done = max(select_adm)) %>%
  # Cases where COCIN has no discharge but the adm date matches
  mutate(select_adm = if_else(!chi_done & adm_match & is.na(cocin_dis), TRUE, select_adm)) %>%
  # Mark the CHI as done
  mutate(chi_done = max(select_adm)) %>%
  # Filter out the records which we haven't selected
  filter(select_adm | !chi_done | is.na(chi_done)) %>%
  # Deal with the admissions which need grouping to match COCIN
  mutate(join_records = if_else(interval(adm_date, dis_date) %within% interval(cocin_adm - days(1), cocin_dis + days(1)),
    0L, NA_integer_
  )) %>%
  # Limit to records we're about to merge and the single admissions which have been selected already
  filter(join_records == 0 | select_adm) %>%
  group_by(chi_number, join_records) %>%
  summarise(
    dob = first(na.omit(dob)),
    age = first(age),
    postcode = first(na.omit(postcode)),
    health_board_of_residence = first(na.omit(health_board_of_residence)),
    health_board_of_treatment = first(na.omit(health_board_of_treatment)),
    sex = first(na.omit(sex)),
    ethnicity = first(na.omit(ethnicity)),
    ethnicity_code = first(na.omit(ethnicity_code)),
    rapid_id = first(rapid_id),
    adm_date = min(adm_date),
    dis_date = max(dis_date),
    hospital_of_treatment_code = last(hospital_of_treatment_code),
    admitted_transfer_from_type = first(na.omit(admitted_transfer_from_type)),
    discharge_type = last(na.omit(discharge_type)),
    diag_1 = first(na.omit(diag_1)),
    diag_2 = first(na.omit(diag_2)),
    diag_3 = first(na.omit(diag_3)),
    diag_4 = first(na.omit(diag_4)),
    diag_5 = first(na.omit(diag_5)),
    diag_6 = first(na.omit(diag_6)),
    result = first(result), # Result is the result of the PCR test from ECOSS
    ecossid = first(ecossid),
    test_date = first(test_date),
    forename = first(forename),
    surname = first(surname)
  ) %>%
  ungroup() %>%
  select(-join_records) %>%
  mutate(cocin_admission = TRUE)

rapid_cocin_filtered <- rapid_stay_level %>%
  anti_join(cocin_matched, by = "chi_number") %>%
  bind_rows(cocin_matched) %>%
  # Use the diagnosis data (where availiable) to highlight certain stays
  mutate(
    covid_lab = grepl("U071", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6), fixed = TRUE),
    covid_clinical = grepl("U072", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6), fixed = TRUE),
    covid_other = grepl("(:?B342)|(:?B972)", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)),
    no_diag_data = is.na(paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)),
    covid = case_when(
      covid_lab ~ "Lab comfirmed",
      covid_clinical ~ "Clinically suspected",
      covid_other ~ "Other coronavirus",
      no_diag_data ~ "No diag data"
    )
  )

rm(cocin_with_chi, rapid, rapid_stay_level, cocin_matched)

# Want to get one stay per patient
# Filter off as we find a sensible match

# Use the COCIN matched admissions as a start
cocin_match <- rapid_cocin_filtered %>%
  filter(cocin_admission) %>%
  mutate(reason = "COCIN matched")

episode_break_days <- 14
# Find paitents who had a positive test during the stay and use that
coded_as_covid <- rapid_cocin_filtered %>%
  # Remove any CHIs which already have a matched admission
  anti_join(cocin_match, by = "chi_number") %>%
  filter(covid %in% c("Lab comfirmed", "Clinically suspected")) %>%
  group_by(chi_number) %>%
  mutate(epnum = row_number()) %>%
  ungroup() %>%
  mutate(date_diff = time_length(dis_date %--% lead(adm_date), "days")) %>%
  mutate(
    temporal_link_forward = case_when(
      date_diff <= episode_break_days & lead(epnum) != 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    temporal_link_backward = case_when(
      lag(temporal_link_forward) == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(chi_number)

# only do episode linking on stripped down dataframe, so things are marginally less grim.
# notably, do not need to link time or location for CHIs that only have one associated episode
df_mini <- coded_as_covid %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(chi_number, epnum, temporal_link_backward)

temporal_link_ids <- numeric(dim(df_mini)[1])

# construct linking identifier vectors (could rewrite in C/Rcpp for extra speed?)
chis <- df_mini$chi_number
temporal_values <- df_mini$temporal_link_backward
ep <- df_mini$epnum

for (i in 1:length(chis)) {
  if (ep[i] == 1) {
    temporal_link_ids[i] <- 1
  } else {
    if (chis[i] == chis[i - 1]) {
      temporal_link_ids[i] <- if_else(temporal_values[i] == TRUE, temporal_link_ids[i - 1], temporal_link_ids[i - 1] + 1)
    }
  }
}

# add the linking identifiers back into the dataframe
df_mini <- df_mini %>%
  ungroup() %>%
  mutate(episode_break_link = temporal_link_ids)

coded_as_covid <- coded_as_covid %>%
  left_join(df_mini) %>%
  ungroup() %>%
  mutate(episode_break_link = replace_na(episode_break_link, 1)) %>%
  select(-ends_with("_forward"), -ends_with("_backward"), -starts_with("date_diff")) %>%
  mutate(los = time_length(adm_date %--% dis_date, "days")) %>%
  group_by(chi_number, episode_break_link) %>%
  summarise(
    dob = first(na.omit(dob)),
    age = first(age),
    postcode = first(na.omit(postcode)),
    health_board_of_residence = first(na.omit(health_board_of_residence)),
    health_board_of_treatment = first(na.omit(health_board_of_treatment)),
    sex = first(na.omit(sex)),
    ethnicity = first(na.omit(ethnicity)),
    rapid_id = first(rapid_id),
    adm_date = min(adm_date),
    dis_date = max(dis_date),
    hospital_of_treatment_code = last(hospital_of_treatment_code),
    covid = last(covid),
    diag_1 = first(na.omit(diag_1)),
    diag_2 = first(na.omit(diag_2)),
    diag_3 = first(na.omit(diag_3)),
    diag_4 = first(na.omit(diag_4)),
    diag_5 = first(na.omit(diag_5)),
    diag_6 = first(na.omit(diag_6)),
    result = first(result), # Result is the result of the PCR test from ECOSS
    ecossid = first(ecossid),
    test_date = first(test_date),
    forename = first(forename),
    surname = first(surname),
    los = sum(los)
  ) %>%
  # Clean up any los where this is longer than possible (caused by overlapping dates)
  # The remove any los for episodes where the dates match the los
  mutate(
    derived_los = time_length(adm_date %--% dis_date, "days"),
    los = if_else(los > derived_los, derived_los, los),
    los = if_else(los == derived_los, NA_real_, los),
    los = as.integer(los)
  ) %>%
  select(-derived_los) %>%
  group_by(chi_number) %>%
  mutate(
    ep_num = row_number(),
    readmission = if_else(ep_num > 1, 1L, 0L),
    reinfection = if_else(readmission == 1 & (time_length(lag(dis_date) %--% adm_date, "days") >= 42), 1L, 0L)
  ) %>%
  ungroup() %>%
  mutate(reason = case_when(
    covid == "Lab comfirmed" ~ case_when(
      result == 1 ~ "Lab comfirmed - ECOSS +ve & Diag",
      result == 0 ~ "Lab comfirmed - ECOSS -ve & Diag",
      is.na(result) ~ "Lab comfirmed - Diag, no ECOSS test"
    ),
    covid == "Clinically suspected" ~ case_when(
      result == 1 ~ "Clinically suspected - ECOSS +ve & Diag",
      result == 0 ~ "Clinically suspected - ECOSS -ve & Diag",
      is.na(result) ~ "Clinically suspected - Diag, no ECOSS test"
    )
  ))

rm(df_mini, ep, episode_break_days, i, temporal_link_ids, temporal_values)

test_in_stay <- rapid_cocin_filtered %>%
  # Remove any CHIs which already have a matched admission
  anti_join(cocin_match, by = "chi_number") %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  # Only keep records with a +ve ECOSS test
  filter(result == 1) %>%
  # Keep admissions where the test was within the dates
  filter((adm_date <= test_date & dis_date >= test_date) |
    (is.na(dis_date) & (test_date >= adm_date))) %>%
  # If we have multiple admissions which overlap the test date merge them
  group_by(chi_number, temporal_link_id) %>%
  # Use last as then we are more likely to avoid an initial transfer hospital
  summarise_all(~ last(na.omit(.))) %>%
  ungroup() %>%
  # Arrange to keep the earliest admission if we still have multiple
  arrange(desc(adm_date)) %>%
  distinct(chi_number, .keep_all = TRUE) %>%
  mutate(reason = "Test during stay")

# Find patients who had a positive test before an admision and take that patients latest admission
test_before_stay <- rapid_cocin_filtered %>%
  # Remove any CHIs which already have a matched admission
  anti_join(cocin_match, by = "chi_number") %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  # anti_join(other_coronavirus, by = "chi_number") %>%
  filter(result == 1) %>%
  filter(test_date %within% ((adm_date - days(21)) %--% adm_date)) %>%
  # If we have multiple admissions which overlap the test date merge them
  group_by(chi_number, temporal_link_id) %>%
  # Use last as then we are more likely to avoid an initial transfer hospital
  summarise_all(~ last(na.omit(.))) %>%
  ungroup() %>%
  arrange(chi_number, adm_date) %>%
  distinct(chi_number, .keep_all = TRUE) %>%
  mutate(reason = "Test <= 21 days before stay")

# See what we have - we're assuming these are non-covid admissions
rapid_cocin_filtered %>%
  # Remove any CHIs which already have a matched admission
  anti_join(cocin_match, by = "chi_number") %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  # anti_join(other_coronavirus, by = "chi_number") %>%
  anti_join(test_before_stay, by = "chi_number") %>%
  distinct(chi_number, result) %>%
  count(result)

# Create a dataset of single admission per CHI
covid_admissions <- bind_rows(
  cocin_match,
  test_in_stay,
  test_before_stay,
  coded_as_covid
) %>%
  select(
    -temporal_link_id,
    -location_link_id,
    -episode_break_link,
    -cocin_admission,
    -covid_lab,
    -covid_clinical,
    -covid_other
  ) %>%
  replace_na(list(ep_num = 1L, readmission = 0L, reinfection = 0L)) %>%
  # Temp measure, remove test date on readmissions (likely a 'better' test date in ECOSS)
  mutate(test_date = if_else(readmission > 0, NA_Date_, test_date))

# Check we have one admission per CHI
map(list(covid_admissions, cocin_match, coded_as_covid, test_before_stay, test_in_stay), ~ .x %>%
  count(chi_number) %>%
  count(n))

# Look at the reasons for each admission
covid_admissions %>% count(reason)

reason_levels <- covid_admissions %>%
  count(reason) %>%
  arrange(n) %>%
  pull(reason)


# Plot the admission reason by admission date
ggplot2::ggplot(covid_admissions) +
  geom_histogram(aes(adm_date, fill = factor(reason, reason_levels)), binwidth = 7) +
  theme_minimal() +
  scale_fill_brewer("Reason", type = "qual", palette = "Set3") +
  theme(legend.position = "top") +
  xlab("Admission week") +
  ylab("Number of admissions")


# Not sure if this is needed
# Might need reinstating with changes?

# covid_admissions <- covid_admissions %>%
#   mutate(
#     age.factor = case_when(
#       age < 17 ~ "<17",
#       age < 30 ~ "17-29",
#       age < 40 ~ "30-39",
#       age < 50 ~ "40-49",
#       age < 60 ~ "50-59",
#       age < 70 ~ "60-69",
#       age < 80 ~ "70-79",
#       is.na(age) ~ NA_character_,
#       TRUE ~ "80+"
#     ),
#     age_band = case_when(
#       age < 18 ~ "Pediatric",
#       age >= 18 ~ "Adult"
#     ) %>%
#       as_factor() %>%
#       fct_explicit_na(na_level = "Unknown"),
#     sex = case_when(
#       sex == "M" ~ "Male",
#       sex == "F" ~ "Female"
#     ) %>%
#       factor(levels = c("Male", "Female", "Not specified")),
#     admission_iso = isoweek(adm_date),
#     admission_week = floor_date(adm_date, unit = "week", week_start = 1),
#     health_board_of_treatment = str_sub(health_board_of_treatment, 5) %>%
#       str_to_title() %>%
#       str_replace("&", "and") %>%
#       str_c("NHS ", .)
#   )

rm(cocin_match, coded_as_covid, rapid_cocin_filtered, test_before_stay, test_in_stay, reason_levels)

rapid_date <- file_info(path(here("data"), "rapid_ecoss_joined.rds")) %>%
  pull(modification_time) %>%
  date()
write_rds(covid_admissions,
  path(here("data", str_glue("{rapid_date}_RAPID-cleaned-filtered.rds"))),
  compress = "gz"
)

rm(rapid_date, covid_admissions)
