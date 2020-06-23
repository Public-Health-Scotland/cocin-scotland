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

# Create Completness estimates per HB (would be better per hospital / DaG)
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
    cocin = replace_na(cocin, 0L)
  ) %>%
  # Add in a Scotland row
  bind_rows(summarise(.,
    cocin = sum(cocin, na.rm = TRUE),
    rapid = sum(rapid, na.rm = TRUE)
  ) %>%
    mutate(
      hb_name = "Scotland",
      order = 1
    )) %>%
  mutate("Percent Complete" = percent(cocin / rapid, accuracy = 0.1)) %>%
  arrange(order, hb_name) %>%
  select(-order) %>%
  rename(
    "NHS Health Board" = hb_name,
    "COCIN count" = cocin,
    "RAPID count" = rapid
  )

## Todo
# Make breakdown of hospital per ISO week per sex and pead/ adult/ old
# Last weeks CHIs RAPID + ECOSS vs COCIN

age_sex_hb_completness <-
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
  mutate("Percent Complete" = percent(cocin / rapid, accuracy = 0.1)) %>%
  rename(
    "NHS Health Board" = health_board_of_treatment,
    "Age Group" = age_band,
    "Sex" = sex,
    "COCIN count" = cocin,
    "RAPID count" = rapid
  )

# Split into a list of data frames by HB
hb_list <- age_sex_hb_completness %>%
  group_split(`NHS Health Board`)
# Name the list
names(hb_list) <- age_sex_hb_completness %>%
  pull("NHS Health Board") %>%
  unique()

# Write out as Excel workbook with multiple tabs
write_xlsx(
  c(list(Overview = hb_completeness), hb_list),
  str_glue("output/{date}_HB_Completeness.xlsx",
    date = date(latest_extract_date())
  )
)
