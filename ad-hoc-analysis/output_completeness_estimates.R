source("ad-hoc-analysis/read_and_tidy_rapid_data.R")

if (date(local_last_modified) > ymd_hm(latest_extract_date())) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

# Read COCIN data
cocin <- read_rds(str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date())) %>%
  # Repair any age/sex we can using CHI
  fix_age_sex_from_chi()

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

hb_completeness <- full_join(
  cocin %>%
    distinct(subjid, hb_name) %>%
    count(hb_name),
  covid_admissions %>%
    count(health_board_of_treatment),
  by = c("hb_name" = "health_board_of_treatment")
) %>%
  rename(
    cocin = n.x,
    rapid = n.y
  ) %>%
  mutate(
    cocin = replace_na(cocin, 0L),
  ) %>%
  bind_rows(summarise(.,
    cocin = sum(cocin, na.rm = TRUE),
    rapid = sum(rapid, na.rm = TRUE)
  ) %>%
    mutate(
      hb_name = "Scotland",
      order = 1
    )) %>%
  mutate("Percent Complete" = scales::percent(cocin / rapid, accuracy = 0.1)) %>% 
  arrange(order, hb_name) %>%
  select(-order) %>% 
  rename(
    "NHS Health Board" = hb_name, 
    "COCIN count" = cocin,
    "RAPID count" = rapid
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
      summarise(sex = first(na.omit(sex))) %>%
      count(sex),
    covid_admissions %>%
      count(sex),
    by = c("sex")
  ) %>%
  rename(
    cocin_patients = n.x,
    rapid_patients = n.y
  ) %>%
  mutate(
    cocin = replace_na(cocin, 0L),
    pct_complete = cocin_patients / rapid_patients * 100
  )

sex_completness %>%
  write_csv(
    str_glue("output/{date}_sex_completness_summary.csv", date = date(latest_extract_date()))
  )

## Todo
# Make breakdown of hospital per ISO week per sex and pead/ adult/ old
# Last weeks CHIs RAPID + ECOSS vs COCIN

master_completness <-
  full_join(
    cocin %>%
      group_by(subjid) %>%
      summarise_at(vars(hb_name, age, sex), ~ first(na.omit(.))) %>%
      mutate(age_band = case_when(
        age < 18 ~ "Pediatric",
        age >= 18 ~ "Adult"
      ) %>%
        as_factor()) %>%
      count(health_board_of_treatment = hb_name, age_band, sex),
    covid_admissions %>%
      count(health_board_of_treatment, age_band, sex),
    by = c("health_board_of_treatment", "age_band", "sex")
  ) %>%
  rename(
    cocin = n.x,
    rapid = n.y
  ) %>%
  mutate(cocin = replace_na(cocin, 0L)) %>%
  sum_totals(c("cocin", "rapid"),
    c("health_board_of_treatment", "age_band", "sex"),
    na.rm = TRUE
  ) %>%
  drop_na() %>%
  mutate_at(vars(health_board_of_treatment), ~ if_else(. == "Total", "Scotland", .)) %>%
  mutate_at(vars(age_band, sex), ~ if_else(. == "Total", "All", .)) %>%
  arrange(health_board_of_treatment, age_band, sex) %>%
  mutate("Percent Complete" = scales::percent(cocin / rapid, accuracy = 0.1)) %>% 
  rename(
    "NHS Health Board" = health_board_of_treatment, 
    "Age Group" = age_band,
    "Sex" = sex,
    "COCIN count" = cocin,
    "RAPID count" = rapid
  )

hb_list <- master_completness %>% group_split(`NHS Health Board`)
names(hb_list) <- master_completness %>% pull("NHS Health Board") %>% unique()

write_xlsx(c(list(Overview = hb_completeness), hb_list),
           str_glue("output/{date}_HB_Completeness.xlsx", date = date(latest_extract_date()))
)

