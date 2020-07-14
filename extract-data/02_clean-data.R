###########################################################
# Name of script: 02_clean-data.R
# Written by: Analysts working in HPS Enhanced Surveillance
#             Cell - Hospital/ICU Work Stream
# Credit to: University of Edinburgh Surgical Informatics
#
# Type of script: Data Cleaning
# Written/run on: R Studio Desktop
# Version of R: 3.6.1
#
# Description: Clean CoCIN data extract
###########################################################


### 0 - Run setup environment script ----

source("extract-data/00_setup-environment.R")


### 1 - Read in data ----

scot_data <-
  read_rds(
    here(
      "data",
      paste0(
        latest_extract_date(),
        "_scot-data.rds"
      )
    )
  )


### 2 - Fix RedCap variables ----

# Fix for day 1 tier 1 repeating daily form
# This was an error in the REDCap specification.
# The daily form should only appear once per event, but here it is a repeating instrument
# 1. Change redcap event name for repeated forms so they are not duplicated
# 2. Mark redcap_repeat_instance and redcap_repeat_instrument as NA
# For repeating forms, rewrite redcap_event_name for the day corresponding with the instance.
# This does not use date, only instance.
# This may conflict if daily forms completed in other event with same name, i.e. Day 3.

scot_data %<>%
  mutate(

    # Rows to update
    mark_to_change = if_else(
      redcap_event_name == "Day 1 Hospital&ICU Admission (Arm 2: TIER 1)" &
        redcap_repeat_instrument == "Daily Form",
      TRUE, FALSE, FALSE
    ),

    # Change to character as factor causes issues
    redcap_event_name = as.character(redcap_event_name),
    redcap_repeat_instrument = as.character(redcap_repeat_instrument),

    # Change event name
    redcap_event_name = ifelse(mark_to_change,
      paste0(
        "Day ", redcap_repeat_instance + 1,
        " Hospital&ICU Admission (Arm 2: TIER 1)"
      ),
      redcap_event_name
    ),

    # Set redcap_repeat_instrument and redcap_repeat_instance to NA as they would be normally.
    redcap_repeat_instance = ifelse(mark_to_change, NA, redcap_repeat_instance),
    redcap_repeat_instrument = ifelse(mark_to_change, NA, redcap_repeat_instrument)
  ) %>%

  select(-mark_to_change)


### 3 - Remove subjects with no coronavirus diagnosis ----

scot_data %<>%
  dplyr::group_by(subjid) %>%
  dplyr::filter(!any(corna_mbcat == "NO", na.rm = TRUE)) %>%
  dplyr::ungroup()


### 4 - General data cleaning ----

# Dates
scot_data %<>%

  dplyr::mutate(

    # If admission date missing, use daily sheet 1 date if available
    hostdat = dplyr::case_when(
      str_detect(redcap_event_name, "^Day 1 ") &
        is.na(hostdat) &
        !is.na(daily_dsstdat) ~ daily_dsstdat,
      TRUE ~ hostdat
    ),

    # Calculate days between onset and admission
    onset2admission = (hostdat - cestdat) %>%
      as.numeric() %>%
      ff_label("Onset to admission (days)")
  )

# Restore label to hostdat otherwise case_when fails for different types
label(scot_data$hostdat) <- "Admission date to facility"

# Age
scot_data %<>%

  dplyr::mutate(

    # Date at which to calculate age
    anydat = dplyr::case_when(
      !is.na(hostdat) ~ hostdat, # Use admission date first, then,
      any(!is.na(daily_dsstdat)) ~ coalesce(daily_dsstdat), # first non-missing daily form across all forms
      !is.na(cestdat) ~ cestdat, # onset date,
      !is.na(dsstdat) ~ dsstdat # enrolement date
    ),

    # Calculate age using DOB and anydat derived above
    age = interval(agedat, anydat) %>%
      as.period() %>%
      year(),

    # Add infants to age variable by making months a fraction of year
    age_estimateyears = as.numeric(age_estimateyears),
    age_estimateyears = ifelse(age_estimateyearsu == "Months",
      age_estimateyears / 12,
      age_estimateyears
    ),

    # Where DOB missing, use age_estimateyears
    age = ifelse(is.na(agedat), age_estimateyears, age) %>%
      ff_label("Age on admission (years)"),

    age.factor = dplyr::case_when(
      age < 10 ~ "<10",
      age < 20 ~ "10-19",
      age < 30 ~ "20-29",
      age < 40 ~ "30-39",
      age < 50 ~ "40-49",
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      is.na(age) ~ NA_character_,
      TRUE ~ "80+"
    )
  )


# Continuous variables
scot_data %<>%

  dplyr::mutate_at(

    # Remove text/punctuation/etc and convert to numeric
    vars(
      temp_vsorres, hr_vsorres, rr_vsorres,
      sysbp_vsorres, admission_diabp_vsorres,
      oxy_vsorres, daily_fio2_lborres, daily_sao2_lborres,
      daily_pao2_lborres, daily_pco2_lborres, daily_ph_lborres,
      daily_hco3_lborres, daily_baseex_lborres,
      daily_gcs_vsorres,
      systolic_vsorres, diastolic_vsorres, daily_meanart_vsorres,
      daily_urine_lborres,
      daily_hb_lborres, daily_wbc_lborres, daily_lymp_lborres,
      daily_neutro_lborres, daily_haematocrit_lborres, daily_plt_lborres,
      daily_aptt_lborres, daily_pt_lborres, daily_inr_lborres,
      daily_alt_lborres, daily_bil_lborres, daily_ast_lborres,
      daily_glucose_lborres,
      daily_bun_lborres, daily_lactate_lborres, daily_ldh_lborres,
      daily_creat_lborres,
      daily_sodium_lborres, daily_potassium_lborres, daily_procal_lborres,
      daily_crp_lborres
    ),
    ~ as.character(.) %>% parse_number()
  ) %>%

  dplyr::mutate(

    # Fix issues with units
    daily_potassium_lborres = case_when(
      daily_potassium_lborres > 100 ~ NA_real_,
      daily_potassium_lborres > 12 ~ daily_potassium_lborres / 10,
      daily_potassium_lborres < 0 ~ abs(daily_potassium_lborres),
      TRUE ~ daily_potassium_lborres
    ),

    daily_hb_lborres = ifelse(
      daily_hb_lborres < 25,
      daily_hb_lborres * 10,
      daily_hb_lborres
    ),

    daily_wbc_lborres = ifelse(
      daily_wbc_lborres > 100,
      NA_real_,
      daily_wbc_lborres
    ),

    daily_neutro_lborres = ifelse(
      daily_neutro_lborres > 100,
      daily_neutro_lborres / 1000,
      daily_neutro_lborres
    ),

    daily_lymp_lborres = ifelse(
      daily_lymp_lborres > 100,
      daily_lymp_lborres / 1000,
      daily_lymp_lborres
    ),

    daily_pt_lborres = ifelse(
      daily_pt_lborres < 9,
      daily_pt_lborres * 12,
      daily_pt_lborres
    ),

    daily_glucose_lborres = ifelse(
      daily_glucose_lborres > 100,
      NA_real_,
      daily_glucose_lborres
    ),

    daily_pao2_lborres = ifelse(
      daily_pao2_lborresu == "mmHg",
      daily_pao2_lborres / 7.5,
      daily_pao2_lborres
    ),

    daily_lactate_lborres = ifelse(
      daily_lactate_lborres > 100,
      NA_real_,
      daily_lactate_lborres
    ),

    daily_fio2_lborres = dplyr::case_when(
      daily_fio2_lborres <= 1 ~ daily_fio2_lborres, # Presume FiO2
      daily_fio2_lborres <= 2 ~ 0.24, # Presume these are all L/min
      daily_fio2_lborres <= 3 ~ 0.28,
      daily_fio2_lborres <= 4 ~ 0.32,
      daily_fio2_lborres <= 5 ~ 0.36,
      daily_fio2_lborres <= 6 ~ 0.40,
      daily_fio2_lborres <= 10 ~ 0.50,
      daily_fio2_lborres <= 15 ~ 0.70,
      TRUE ~ daily_fio2_lborres
    )
  )


# Ethnicity

scot_data %<>%

  dplyr::mutate(
    ethnicity = dplyr::case_when(
      ethnic___1 == "Checked" ~ "Arab",
      ethnic___2 == "Checked" ~ "Black",
      ethnic___3 == "Checked" ~ "East Asian",
      ethnic___4 == "Checked" ~ "South Asian",
      ethnic___5 == "Checked" ~ "West Asian",
      ethnic___6 == "Checked" ~ "Latin American",
      ethnic___7 == "Checked" ~ "White",
      ethnic___8 == "Checked" ~ "Aboriginal/First Nations",
      ethnic___9 == "Checked" ~ "Other"
    ),

    ethnicity_grouped = dplyr::case_when(
      ethnicity %in% c("East Asian", "South Asian", "West Asian") ~ "Asian",
      ethnicity %in%
        c("Other", "Arab", "Latin American", "Aboriginal/First Nations") ~ "Other",
      TRUE ~ ethnicity
    )
  )


### 5 - More cleaning that may alter original data ----

scot_data %<>%

  dplyr::mutate(

    # Fill in GCS with AVPU
    daily_gcs_vsorres = dplyr::case_when(
      is.na(daily_gcs_vsorres) & avpu_vsorres == "Alert" ~ 15,
      is.na(daily_gcs_vsorres) & avpu_vsorres == "Verbal" ~ 12,
      is.na(daily_gcs_vsorres) & avpu_vsorres == "Pain" ~ 9,
      is.na(daily_gcs_vsorres) & avpu_vsorres == "Unresponsive" ~ 3,
      TRUE ~ daily_gcs_vsorres
    ),

    # Collapse smoking to active smokers
    smoking_mhyn_2levels = dplyr::case_when(
      smoking_mhyn %in% c("Never Smoked", "Former Smoker") ~ "NO",
      smoking_mhyn == "Yes" ~ "YES"
    ),

    daily_pt_lborres_add_inr = dplyr::case_when(
      is.na(daily_pt_lborres) & !is.na(daily_inr_lborres) ~
      (daily_inr_lborres * 12),
      TRUE ~ daily_pt_lborres
    )
  )


### 6 - Add various flags ----

scot_data %<>%

  # Add flag if any record of ICU admission
  dplyr::group_by(subjid) %>%
  dplyr::mutate(any_icu = case_when(
    any(daily_hoterm == "Yes") | any(icu_hoterm == "Yes") ~ "Yes",
    all(is.na(daily_hoterm), is.na(icu_hoterm)) ~ NA_character_,
    TRUE ~ "No"
  )) %>%
  dplyr::ungroup() %>%

  # Add flag for Day 1 data
  dplyr::mutate(topline = case_when(
    str_detect(redcap_event_name, "^Day 1 ") &
      is.na(redcap_repeat_instrument) ~ 1,
    TRUE ~ 0
  )) %>%

  # Add flag for patients submitted >= 14 days ago
  dplyr::group_by(subjid) %>%
  dplyr::mutate(keep_14 = case_when(
    any(
      (Sys.Date() - hostdat) %>% as.numeric() %>% {
        . >= 14
      },
      na.rm = TRUE
    ) ~ 1,
    TRUE ~ 0
  )) %>%

  # Add flag for patients admitted >= 14 days, but <= 28 days
  dplyr::mutate(keep_14_28 = case_when(
    any(
      (Sys.Date() - hostdat) %>% as.numeric() %>% {
        . >= 14 & . <= 28
      },
      na.rm = TRUE
    ) ~ 1,
    TRUE ~ 0
  )) %>%
  ungroup()


### 7 - Save cleaned data ----

write_rds(
  scot_data,
  here(
    "data",
    paste0(
      latest_extract_date(),
      "_scot-data-clean.rds"
    )
  ),
  compress = "gz"
)


### END OF SCRIPT ###
