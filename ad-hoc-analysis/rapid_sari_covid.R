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
    result,
    keyemployer
  ) %>%
  # Create a variable for health_care_worker status
  mutate(
    keyemployer = str_squish(str_to_lower(keyemployer)),
    hcw = case_when(
      keyemployer == "health care" ~ 1L,
      keyemployer == "citizen" ~ 0L,
      TRUE ~ 8L
    )
  ) %>%
  # Do some simple validation on the postcode and make it NA if it's invalid
  mutate(patient_postcode = if_else(str_detect(
    patient_postcode,
    regex("^[a-z]{1,2}\\d[a-z\\d]?\\s*\\d[a-z]{2}$",
          ignore_case = TRUE
    )
  ), patient_postcode, NA_character_)) %>%
  # Make Unknown Ethnicities NA which will mean we ignore them and possibly pick a known one when aggregating
  mutate(
    patient_ethnic_group_description =
      if_else(patient_ethnic_group_description %in% c("Not Known", "Refused/Not Provided by patient"),
              NA_character_, patient_ethnic_group_description
      )
  ) %>%
  # Drop any invalid CHIs
  mutate(chi_number = if_else(chi_check(chi_number) == "Valid CHI", chi_number, NA_character_)) %>%
  # Replace missing gender (should use a phsmethods function for this)
  mutate(patient_gender_description = case_when(
    !is.na(patient_gender_description) ~ patient_gender_description,
    # If the CHI is missing use sex from ECOSS
    is.na(chi_number) ~ case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      TRUE ~ NA_character_
    ),
    # Otherwise use the sex from the CHI
    parse_integer(str_sub(chi_number, 9, 9)) %% 2 == 0 ~ "Female",
    TRUE ~ "Male"
  )) %>%
  mutate(patient_dob = case_when(
    !is.na(patient_dob) ~ patient_dob,
    TRUE ~ dob_from_chi(chi_number)
  )) %>% 
  select(-sex, -keyemployer)

# Aggregate to 'stay' level - this just uses a marker Bob created which tags episodes which are close in time
# Note we don't group episodes which change hospitals as COCIN CRFs are single hospital
rapid_stay_level <- rapid %>%
  group_by(chi_number, temporal_link_id, location_link_id) %>%
  summarise(
    dob = first(na.omit(patient_dob)),
    age = first(age_year),
    postcode = first(na.omit(patient_postcode)),
    sex = first(na.omit(patient_gender_description)),
    ethnicity = first(na.omit(patient_ethnic_group_description)),
    ethnicity_group = first(na.omit(patient_ethnic_group_code)),
    hcw = first(hcw),
    rapid_id = first(rapid_id),
    adm_date = min(admission_date),
    dis_date = max(discharge_date),
    hospital_of_treatment_code = last(hospital_of_treatment_code),
    health_board_of_residence = first(na.omit(health_board_of_residence)),
    health_board_of_treatment = first(na.omit(health_board_of_treatment)),
    admitted_transfer_from_type = first(na.omit(admitted_transfer_from_type)),
    discharge_type = first(na.omit(discharge_type)),
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
  mutate(age = if_else(!is.na(dob) & !is.na(adm_date),
                       as.integer(floor(time_length(dob %--% adm_date, "years"))),
                       age
  )
  )

clean_icd10 <- function(icd10) {
  if (icd10 == ".") {
    return(icd10)
  } else if (nchar(icd10) == 6) {
    return(icd10)
  } else if (nchar(icd10) == 5) {
    return(str_c(icd10, c("D", "A")))
  } else if (nchar(icd10) == 4) {
    return(str_c(icd10, c(as.character(0:9), " D", " A")))
  } else if (nchar(icd10) == 3) {
    # Fill out with all possible 4th characters
    return(str_c(icd10, c(as.character(0:9), "X", ".")))
  } else {
    stop("ICD-10 code is wrong length")
  }
}

# Generate sari codes from F09 - F22
symptom_codes <- paste0("R", str_pad(0:69, pad = "0", width = 2, side = "left")) %>%
  map(clean_icd10) %>%
  flatten_chr()

sari_symptoms <- c(
  "R05", "R06", "R070", "R13", "R509", "R51", "R531", "R5381", "R5383", "R060",
  "R069", "R0602", "R0600", "R0609", "R063", "R0689", "R6510", "R6511", "R531", "R5381", "R5383", "R630", "R633", "R634", "R638", "R410", "R42",
  "R402", "R404", "R400", "R401", "R5600", "R5601"
) %>%
  map(clean_icd10) %>%
  flatten_chr()

sari_codes <- c(
  "R05", "R06", "R070", "R13", "R509", "R51", "M791", "R531", "R5381", "R5383", "I20",
  "I21", "I22", "I23", "I24", "I25", "I50", "I51", "J439", "J449", "J45", "M791", "R060",
  "R069", "R0602", "R0600", "R0609", "R063", "R0689", "J09", "J10", "J11", "J12", "J13",
  "J14", "J15", "J16", "J17", "J18", "J20", "J21", "J22", "B349", "A499", "J41", "J41",
  "R6510", "R6511", "R531", "R5381", "R5383", "R630", "R633", "R634", "R638", "R410", "R42",
  "F05", "R402", "R404", "R400", "R401", "R5600", "R5601"
) %>%
  map(clean_icd10) %>%
  flatten_chr()

j_codes <- paste0("J", str_pad(09:22, pad = "0", width = 2, side = "left")) %>%
  map(clean_icd10) %>%
  flatten_chr()

code_search <- function(seach_codes, diag_1, diag_2, diag_3, diag_4, diag_5, diag_6) {
  if_else(diag_1 %in% seach_codes | diag_2 %in% seach_codes |
            diag_3 %in% seach_codes | diag_4 %in% seach_codes |
            diag_5 %in% seach_codes | diag_6 %in% seach_codes, 1L, 0L)
}


rapid_flagged <- rapid_stay_level %>%
  mutate(covid_adm = adm_date %within% interval(test_date - days(2), test_date + days(28)),
         result = if_else(covid_adm, result, NA_integer_)) %>% 
  filter(!is.na(age), !is.na(sex)) %>%
  mutate(age_grp = age_group(age, from = 0, to = 80, by = 20, as_factor = TRUE)) %>%
  mutate(
    all_rapid = 1L,
    any_icd10 = case_when(is.na(paste(diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)) ~ 1L, TRUE ~ 0L),
    any_symptoms = code_search(symptom_codes, diag_1, diag_2, diag_3, diag_4, diag_5, diag_6),
    sari_symptoms = code_search(sari_symptoms, diag_1, diag_2, diag_3, diag_4, diag_5, diag_6),
    sari_codes = code_search(sari_codes, diag_1, diag_2, diag_3, diag_4, diag_5, diag_6),
    j_codes = code_search(j_codes, diag_1, diag_2, diag_3, diag_4, diag_5, diag_6)
  )

data <- rapid_flagged %>% 
  pivot_longer(
    cols = all_rapid:j_codes,
    names_to = "code_type",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%
  count(age_grp, sex, code_type, result) %>%
  group_by(sex, age_grp, code_type) %>%
  mutate(pct = n / sum(n) * 100) %>%
  mutate(ECOSS_covid = factor(result, levels = c(0, 1), labels = c("Negative", "Positive")) %>% fct_explicit_na("No test"))

rapid_covid_final <- read_rds(path(here("data", str_glue("{today()}_RAPID-cleaned-filtered.rds"))))

data_w_covid <- rapid_flagged %>% 
  mutate(iso_week = isoweek(adm_date)) %>% 
  select(iso_week, any_symptoms:j_codes) %>% 
  group_by(iso_week) %>% 
  summarise_all(sum) %>% 
  left_join(rapid_covid_final %>% 
              mutate(iso_week = isoweek(adm_date)) %>% 
              count(iso_week, name = "covid_admissions")) %>% 
  pivot_longer(cols = -iso_week, names_to = "code_type", values_to = "value")


factor_order <- data_w_covid %>% 
  group_by(code_type) %>% 
  summarise(count = sum(value, na.rm = TRUE)) %>% 
  arrange(desc(count)) %>% 
  pull(code_type)


trend_admissions <- data_w_covid %>% 
  ggplot(aes(x = iso_week, colour = factor(code_type, levels = factor_order))) +
  geom_line(aes(y = value, linetype = if_else(code_type == "covid_admissions", 2, 1) %>% as_factor)) +
  theme_minimal() +
  scale_linetype(guide = FALSE) +
  scale_color_brewer("Admission type", type = "qual", palette = 2) +
  xlab("ISO Week (2020)") +
  ylab("Count of admissions")

# Number - all
plot_n_all <- data %>%
  group_by(code_type, ECOSS_covid) %>%
  summarise(
    n = sum(n),
    pct = n / sum(n)
  ) %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = n)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number - ex all
plot_n_ex_all <- data %>%
  filter(code_type != "all_rapid") %>%
  group_by(code_type, ECOSS_covid) %>%
  summarise(
    n = sum(n),
    pct = n / sum(n)
  ) %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = n)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Percent - all
plot_pct_all <- data %>%
  group_by(code_type, ECOSS_covid) %>%
  summarise(
    n = sum(n),
    pct = n / sum(n)
  ) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = pct)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Facet - all
plot_n_all_facet <- data %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = n)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(sex ~ age_grp)

# Facet - ex all
plot_n_ex_all_facet <- data %>%
  filter(code_type != "all_rapid") %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = n)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(sex ~ age_grp)

# Facet - pct
plot_pct_facet <- data %>%
  ggplot(aes(x = code_type, fill = ECOSS_covid)) +
  geom_col(aes(y = pct)) +
  theme_minimal() +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(sex ~ age_grp)
