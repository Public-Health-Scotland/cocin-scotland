source("extract-data/00_setup-environment.R")

# Read the linked RAPID / ECOSS file, found here:
# \\stats\PHSCOVID19_Analysis\RAPID Reporting\Daily_extracts

# Set correct filepath for server or desktop
linked_file_path <- path(
  if_else(version$platform == "x86_64-pc-linux-gnu",
    "/conf",
    "//stats"
  ),
  "PHSCOVID19_Analysis", "RAPID Reporting", "Daily_extracts", "rapid_ecoss_joined.rds"
)

# Check the VPN is active (or that we're on the server) and if so try to update the file
if (vpn_active()) {
  message("Connection to NHS network is active")
  if (file_exists(linked_file_path)) {
    server_last_modified <- file_info(linked_file_path) %>%
      pull(modification_time)

    local_last_modified <- file_info("data/rapid_ecoss_joined.rds") %>%
      pull(modification_time)

    # If the server version is newer (or the local copy doesn't exist) copy it accross
    if (date(server_last_modified) > date(local_last_modified) | is.na(local_last_modified)) {
      message(str_glue("The server file looks newer (modified:{server_date}), so replacing local file (modified:{local_date})",
        server_date = date(server_last_modified),
        local_date = date(local_last_modified)
      ))
      file_copy(linked_file_path, "data/rapid_ecoss_joined.rds", overwrite = TRUE)

      local_last_modified <- server_last_modified

      message("New file coppied")
    } else {
      message(str_glue("Local file is current (modified:{local_date})",
        local_date = date(local_last_modified)
      ))
    }
  } else {
    message("File 'rapid_ecoss_joined.rds' does not exist - contact Bob Taylor")
  }
} else {
  message("Not connected to NHS network so can't check for new file")
}

if (date(local_last_modified) > ymd_hm(latest_extract_date())) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
  source("extract-data/99_remove-old-data.R")
}

rm(linked_file_path, local_last_modified, server_last_modified)

# Read COCIN data
cocin_with_chi <- read_rds(str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date())) %>%
  select(subjid, chi_number = nhs_chi, cocin_adm = hostdat, cocin_dis = dsstdtc) %>%
  mutate_at(vars(cocin_adm, cocin_dis), ~ as_date(.)) %>%
  group_by(subjid) %>%
  summarise_all(~ first(na.omit(.))) %>%
  filter(!is.na(chi_number), !is.na(cocin_adm))


# Read in the RAPID file and select variables we need
rapid <- read_rds(here("data", "rapid_ecoss_joined.rds")) %>%
  select(
    chi_number,
    rapid_id,
    age_year,
    sex,
    admission_type,
    admitted_transfer_from_type,
    admitted_transfer_from_description,
    management_of_patient,
    management_of_patient_description,
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
rapid_stay_level <- rapid %>%
  group_by(chi_number, temporal_link_id, location_link_id) %>%
  summarise(
    result = first(result), # Result is the result of the PCR test from ECOSS
    rapid_id = first(rapid_id),
    adm_date = min(admission_date),
    dis_date = max(discharge_date),
    test_date = first(specimen_date),
    hospital_of_treatment_code = first(hospital_of_treatment_code),
    age = first(na.omit(age_year)),
    sex = first(na.omit(sex)),
    diag_1 = first(na.omit(diagnosis_1_code_4_char)),
    diag_2 = first(na.omit(diagnosis_2_code_4_char)),
    diag_3 = first(na.omit(diagnosis_3_code_4_char)),
    diag_4 = first(na.omit(diagnosis_4_code_4_char)),
    diag_5 = first(na.omit(diagnosis_5_code_4_char)),
    diag_6 = first(na.omit(diagnosis_6_code_4_char))
  ) %>%
  ungroup()


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
    result = first(result),
    rapid_id = first(rapid_id),
    adm_date = min(adm_date),
    dis_date = max(dis_date),
    test_date = first(test_date),
    hospital_of_treatment_code = first(hospital_of_treatment_code),
    age = first(na.omit(age)),
    sex = first(na.omit(sex)),
    diag_1 = first(na.omit(diag_1)),
    diag_2 = first(na.omit(diag_2)),
    diag_3 = first(na.omit(diag_3)),
    diag_4 = first(na.omit(diag_4)),
    diag_5 = first(na.omit(diag_5)),
    diag_6 = first(na.omit(diag_6))
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

# Find paitents who had a positive test during the stay and use that
coded_as_covid <- rapid_cocin_filtered %>%
  # Remove any CHIs which already have a matched admission
  anti_join(cocin_match, by = "chi_number") %>%
  filter(covid %in% c("Lab comfirmed", "Clinically suspected")) %>%
  # If we have multiple admission close in time all with good diag data then merge them
  group_by(chi_number, temporal_link_id) %>%
  # Use last as then we are more likely to avoid an initial transfer hospital
  summarise_all(~ last(na.omit(.))) %>%
  ungroup() %>%
  group_by(chi_number) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  mutate(readmission = if_else(chi_number == lag(chi_number), 
                               if_else((lag(dis_date) - adm_date) > 14, 
                                       1L, 
                                       0L), 
                               0L)
         ) %>% View()
  # Arrange to keep the earliest admission if we still have multiple
  arrange(desc(adm_date)) %>%
  distinct(chi_number, .keep_all = TRUE) %>%
  mutate(reason = case_when(
    covid == "Lab comfirmed" ~ case_when(result == 1 ~ "Lab comfirmed - ECOSS +ve & Diag",
                                         result == 0 ~ "Lab comfirmed - ECOSS -ve & Diag",
                                         is.na(result) ~ "Lab comfirmed - Diag, no ECOSS test"
    ),
    covid == "Clinically suspected" ~ case_when(result == 1 ~ "Clinically suspected - ECOSS +ve & Diag",
                                              result == 0 ~ "Clinically suspected - ECOSS -ve & Diag",
                                              is.na(result) ~ "Clinically suspected - Diag, no ECOSS test"
    )
  ))


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
  count(result)

# Create a dataset of single admission per CHI
covid_admissions <- bind_rows(
  cocin_match,
  test_in_stay,
  test_before_stay,
  coded_as_covid,
  # other_coronavirus
)

# Check we have one admission per CHI
map(list(covid_admissions,cocin_match, coded_as_covid, test_before_stay, test_in_stay), ~ .x %>% count(chi_number) %>% count(n))

# Look at the reasons for each admission
covid_admissions %>% count(reason)

reason_levels <- covid_admissions %>% count(reason) %>% arrange(n) %>% pull(reason)


# Plot the admission reason by admission date
ggplot2::ggplot(covid_admissions) +
  geom_histogram(aes(adm_date, fill = factor(reason, reason_levels)), binwidth = 7) +
  theme_minimal() +
  scale_fill_brewer("Reason", type = "qual", palette = "Set3")


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

rm(coded_as_covid, rapid_cocin_filtered, test_before_stay, test_in_stay)
