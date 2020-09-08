source("extract-data/00_setup-environment.R", echo = T)

# Read NHS 24 data
nhs_24_raw <- read_csv(path(server_dir, "COVID_NHS24_extract.csv"),
  col_types =
    cols(
      `NHS 24 Call ID` = col_integer(),
      `NHS 24 CUP Marker` = col_integer(),
      `NHS 24 COVID-19 Flag` = readr::col_factor(levels = c("Y", "N")),
      `NHS 24 COVID-19 Contact Tracing Required Flag` = readr::col_factor(levels = c("Y", "N")),
      .default = col_character()
    )
) %>%
  clean_names()

nhs_24_clean <- nhs_24_raw %>%
  mutate(
    chi = upi_number_c,
    postcode_nhs24 = nhs_24_patient_postcode_c,
    call_id = nhs_24_call_id,
    cup_marker = nhs_24_cup_marker,
    call_date = as_date(nhs_24_call_rcvd_date_time),
    call_datetime = as_datetime(nhs_24_call_rcvd_date_time),
    cup_pathway = nhs_24_pathway_name,
    symptoms_free = nhs_24_call_reason_c %>%
      str_to_upper() %>%
      str_squish() %>%
      str_remove_all("[[:alnum:]\\s]"),
    protocol = nhs_24_symptoms %>%
      str_to_upper() %>%
      str_squish() %>%
      str_remove_all("[[:alnum:]\\s]"),
    symptom_days = nhs_24_covid_19_symptom_duration %>%
      str_to_upper() %>%
      str_squish() %>%
      str_extract("^\\w+"),
    symptom_days = case_when(
      symptom_days == "LESS" ~ 0L,
      symptom_days == "ONE" ~ 1L,
      symptom_days == "TWO" ~ 2L,
      symptom_days == "THREE" ~ 3L,
      symptom_days == "FOUR" ~ 4L,
      symptom_days == "FIVE" ~ 5L,
      symptom_days == "SIX" ~ 6L,
      symptom_days == "SEVEN" ~ 7L,
      symptom_days == "EIGHT" ~ 8L,
      symptom_days == "NINE" ~ 9L,
      symptom_days == "TEN" ~ 10L,
      TRUE ~ NA_integer_
    ),
    covid_flag = nhs_24_covid_19_flag,
    contact_tracing_flag = nhs_24_covid_19_contact_tracing_required_flag,
    .keep = "unused"
  )
