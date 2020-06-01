source("ad-hoc-analysis/read_and_tidy_rapid_data.R")

if (date(local_last_modified) > ymd_hm(latest_extract_date())) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

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
    str_glue("output/{date}_completness_summary.csv", date = latest_extract_date())
    )

age_completness %>% 
  rename(age_band = age.factor) %>% 
  write_csv(
    str_glue("output/{date}_age_completness_summary.csv", date = latest_extract_date())
  )

sex_completness %>% 
  write_csv(
    str_glue("output/{date}_sex_completness_summary.csv", date = latest_extract_date())
  )
