source("extract-data/00_setup-environment.R")

ltc_data <- read_rds(path(server_dir, "ltc_data-extracted_Oct.rds"))

rapid_cocin <- rapid_cocin %>%
  # Correct some factors
  mutate(across(c(anaemia, stroke), ~ fct_recode(.x, "Unknown" = "N/A")),
    hypert = factor(hypert, levels = c("YES", "NO", "Unknown"))
  ) %>%

  # Match on the LTC data
  left_join(ltc_data, by = c("chi_number" = "ltc_patient_upi")) %>%

  # Fill in postcode from LTC data if needed
  mutate(postcode = if_else(is.na(postcode),
    ltc_patient_postcode, postcode
  )) %>%
  select(-ltc_patient_postcode) %>%

  # Recode the LTC dates to Y/N factors
  # depending on the date diagnosed vs admission
  mutate(across(
    c(starts_with("ltc_")),
    ~ case_when(. <= (adm_date + days(7)) ~ "YES", TRUE ~ "NO") %>%
      factor(levels = c("YES", "NO", "Unknown"))
  )) %>%

  # Fill in LTC data where it is missing or unknown
  mutate(
    anaemia = if_else(is.na(anaemia) | asthma == "N/A",
      ltc_blood_and_bfo, anaemia
    ),
    asthma = if_else(is.na(asthma) | asthma == "Unknown",
      ltc_asthma, asthma
    ),
    cancer = if_else(is.na(cancer) | cancer == "Unknown",
      ltc_cancer, cancer
    ),
    dementia = if_else(is.na(dementia) | dementia == "Unknown",
      ltc_dementia, dementia
    ),
    diabetes = if_else(is.na(diabetes) | diabetes == "Unknown",
      ltc_diabetes, diabetes
    ),
    heartdis = if_else(is.na(heartdis) | heartdis == "Unknown",
      ltc_heart_disease, heartdis
    ),
    liverdis = if_else(is.na(liverdis) | liverdis == "Unknown",
      ltc_chron_liver_dis, liverdis
    ),
    lungdis = ltc_copd,
    rendis = if_else(is.na(rendis) | rendis == "Unknown",
      ltc_renal_failure, rendis
    ),
  ) %>%
  # Drop the uneeded variables
  select(-starts_with("ltc"))

rm(ltc_data)
