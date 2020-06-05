source("ad-hoc-analysis/read_and_tidy_rapid_data.R")

if (date(local_last_modified) > ymd_hm(latest_extract_date())) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

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

hosp_completeness %>%
  left_join(scot_locations, by = c("hospid" = "location")) %>%
  select(
    Health_Board = hb_name,
    Hospital_Name = location_name,
    Hospital_Code = hospid,
    cocin_patients,
    rapid_patients,
    pct_complete
  ) %>%
  bind_rows(hosp_completeness %>%
              summarise(
                cocin_patients = sum(cocin_patients, na.rm = TRUE),
                rapid_patients = sum(rapid_patients, na.rm = TRUE)
              ) %>%
              mutate(
                Health_Board = "Scotland",
                pct_complete = cocin_patients / rapid_patients * 100,
                order = 1
              )) %>%
  arrange(order, Health_Board, Hospital_Name) %>%
  select(-order) %>%
  write_csv(
    str_glue("output/{date}_completness_summary.csv", date = date(latest_extract_date()))
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

age_completness %>%
  rename(age_band = age.factor) %>%
  write_csv(
    str_glue("output/{date}_age_completness_summary.csv", date = date(latest_extract_date()))
  )


# Create completeness by sex
sex_completness <-
  full_join(
    cocin %>%
      group_by(subjid) %>%
      summarise(sex = as.numeric(first(na.omit(sex)))) %>%
      count(sex),
    covid_admissions %>%
      mutate(sex = case_when(
        sex == "M" ~ 1,
        sex == "F" ~ 2
      )) %>%
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

sex_completness %>%
  write_csv(
    str_glue("output/{date}_sex_completness_summary.csv", date = date(latest_extract_date()))
  )










## Todo
# Make breakdown of hospital per ISO week per sex and pead/ adult/ old
# Last weeks CHIs RAPID + ECOSS vs COCIN
