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
}

# Read COCIN data
cocin <- read_rds(str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date())) %>%
  # Repair any age/sex we can using CHI
  fix_age_sex_from_chi()

## TO DO
# Remove other coronavirus / limit dates
# Keep only one episode per patient (possible match to COCIN)
# 42 days between episodes for new case

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
  # filter(result == 1) %>%
  group_by(chi_number, temporal_link_id, location_link_id, hospital_of_treatment_code) %>%
  summarise(
    result = first(result), # Result is the result of the PCR test from ECOSS
    rapid_id = first(rapid_id),
    adm_date = min(admission_date),
    dis_date = max(discharge_date),
    test_date = first(specimen_date),
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


# Use the diagnosis data (where availiable) to highlight certain stays
rapid_stay_level <- rapid_stay_level %>%
  mutate(
    covid_lab = grepl("U071", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6), fixed = TRUE),
    covid_clinical = grepl("U072", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6), fixed = TRUE),
    covid_other = grepl("(:?B342)|(:?B972)", paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)),
    no_diag_data = is.na(paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)),
    covid = case_when(
      covid_lab ~ "Lab comfirmed",
      covid_clinical ~ "Clinical suspected",
      covid_other ~ "Other coronavirus",
      no_diag_data ~ "No diag data"
    )
  ) %>%
  replace_na(list(result = 0L))

# Want to get one stay per patient
# Filter off as we find a sensible match

# Find paitents who had a positive test during the stay and use that
# Take the latest stay if needed
coded_as_covid <- rapid_stay_level %>%
  filter(covid %in% c("Lab comfirmed", "Clinical suspected")) %>%
  mutate(reason = case_when(
    covid == "Lab comfirmed" ~ if_else(result == 1,
      "Lab comfirmed - ECOSS + Diag",
      "Lab comfirmed - diag only"
    ),
    covid == "Clinical suspected" ~ if_else(result == 1,
      "Clinical suspected - ECOSS confirmed",
      "Clinical suspected - no ECOSS"
    )
  ))

test_in_stay <- rapid_stay_level %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  filter(result == 1) %>%
  filter((adm_date <= test_date & dis_date >= test_date) |
    (is.na(dis_date) & (test_date >= adm_date))) %>%
  arrange(desc(adm_date)) %>%
  distinct(chi_number, .keep_all = TRUE) %>%
  mutate(reason = "Test during stay")

other_coronavirus <- rapid_stay_level %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  filter(covid == "Other coronavirus", age >= 18 | result == 1) %>%
  mutate(reason = if_else(result == 1, "Other coronavirus - ECOSS +ve", "Other coronavirus (>18 only)"))

# Find patients who had a positive test before an admision and take that patients latest admission
test_before_stay <- rapid_stay_level %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  anti_join(other_coronavirus, by = "chi_number") %>%
  filter(result == 1) %>%
  filter(test_date %within% ((adm_date - days(21)) %--% adm_date)) %>%
  arrange(adm_date) %>%
  distinct(chi_number, .keep_all = TRUE) %>%
  mutate(reason = "Test <= 21 days before stay")

# Exclude any who have tested positive after the latest discharge we have for them
test_after_dis <- rapid_stay_level %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  anti_join(other_coronavirus, by = "chi_number") %>%
  anti_join(test_before_stay, by = "chi_number") %>%
  group_by(chi_number) %>%
  filter(test_date > max(dis_date)) %>%
  ungroup()

# See what we have - should be no records left
rapid_stay_level %>%
  anti_join(coded_as_covid, by = "chi_number") %>%
  anti_join(test_in_stay, by = "chi_number") %>%
  anti_join(other_coronavirus, by = "chi_number") %>%
  anti_join(test_before_stay, by = "chi_number") %>%
  anti_join(test_after_dis, by = "chi_number") %>%
  count(result)

# Create a dataset of single admission per CHI
covid_admissions <- bind_rows(
  test_in_stay,
  test_before_stay,
  coded_as_covid,
  other_coronavirus
)

covid_admissions %>% count(reason)

ggplot2::ggplot(covid_admissions) +
  geom_freqpoly(aes(adm_date, colour = reason), binwidth = 7)

covid_admissions <- covid_admissions %>%
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
    ),
    age_band = case_when(
      age < 18 ~ "Pediatric",
      age >= 18 ~ "Adult"
    ) %>%
      as_factor() %>%
      fct_explicit_na(na_level = "Unknown"),
    sex = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female"
    ) %>%
      factor(levels = c("Male", "Female", "Not specified")),
    admission_iso = isoweek(adm_date),
    admission_week = floor_date(adm_date, unit = "week", week_start = 1),
    health_board_of_treatment = str_sub(health_board_of_treatment, 5) %>%
      str_to_title() %>%
      str_replace("&", "and") %>%
      str_c("NHS ", .)
  )

rm(rapid_stay_level, test_after_dis, test_in_stay, test_before_stay, linked_file_path)
