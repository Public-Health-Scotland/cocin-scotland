source("extract-data/00_setup-environment.R")

flu_data <- read_rds(path(server_dir, "flutested_allages.rds")) %>%
  mutate(
    week = as.ordered(week),
    sex = as_factor(sex)
  ) %>%
  clean_names()

season_dates <- flu_data %>%
  group_by(season) %>%
  summarise(
    earliest_adm = min(datref) - weeks(1),
    latest_dis = max(datref) + weeks(1)
  )

cis_early_date <- season_dates %>%
  pull(earliest_adm) %>%
  min() %>%
  floor_date("years") - years(3)

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
sari_codes <- paste0("F", str_pad(09:22, pad = "0", width = 2, side = "left")) %>%
  map(clean_icd10) %>%
  flatten_chr()

# Set up the SMRA connection
smra_conn <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "SMRA",
  uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
  pwd = rstudioapi::askForPassword("SMRA Password:")
)

# Set up a simple SMR01 extract (same as in the previous example)
smr01 <- tbl(smra_conn, "SMR01_PI") %>%
  # Use the 'stay' sort order
  arrange(
    LINK_NO,
    ADMISSION_DATE,
    DISCHARGE_DATE,
    ADMISSION,
    DISCHARGE,
    URI
  ) %>%
  select(
    UPI_NUMBER,
    CIS_MARKER,
    ADMISSION_DATE,
    DISCHARGE_DATE,
    MAIN_CONDITION,
    OTHER_CONDITION_1,
    OTHER_CONDITION_2,
    OTHER_CONDITION_3,
    OTHER_CONDITION_4,
    OTHER_CONDITION_5
  ) %>%
  filter(
    !is.na(UPI_NUMBER),
    ADMISSION_DATE >= cis_early_date
  ) %>%
  mutate(
    sari_main = if_else(MAIN_CONDITION %in% sari_codes, 1L, 0L),
    sari_any = if_else(MAIN_CONDITION %in% sari_codes | OTHER_CONDITION_1 %in% sari_codes |
      OTHER_CONDITION_2 %in% sari_codes | OTHER_CONDITION_3 %in% sari_codes |
      OTHER_CONDITION_4 %in% sari_codes | OTHER_CONDITION_5 %in% sari_codes, 1L, 0L)
  )

data <- smr01 %>%
  collect() %>%
  clean_names()

data_cis <- data %>%
  replace_na(list(sari_any = 0L)) %>%
  mutate(across(c(admission_date, discharge_date), as_date)) %>% 
  group_by(upi_number, cis_marker) %>%
  summarise(
    admission_date = min(admission_date),
    discharge_date = max(discharge_date),
    across(main_condition:other_condition_5, ~ first(na.omit(.x))),
    sari_main_cis = max(sari_main),
    sari_any_cis = max(sari_any),
    sari_main = first(sari_main),
    sari_any = first(sari_any)
  ) %>%
  ungroup() %>%
  filter(admission_date >= (season_dates %>% pull(earliest_adm) %>% min() - days(2)))


test <- data_cis %>%
  left_join(flu_data %>%
    filter(fluaorb == 1) %>%
    select(chi, datref, season),
  by = c("upi_number" = "chi")
  ) %>%
  mutate(
    flu_hospitalisation = if_else(admission_date %within% interval(datref - days(2), datref + days(7)),
      1L, 0L
    ),
    flu_hospitalisation = replace_na(flu_hospitalisation, 0L)
  ) %>%
  distinct(upi_number, cis_marker, .keep_all = TRUE)

test %>%
  mutate(adm_week = str_glue("{season} - {str_pad(isoweek(admission_date), width = 2, side = 'left', pad = '0')}") %>% 
  select(adm_week, sari_main_cis, sari_main, sari_any_cis, sari_any, flu_hospitalisation) %>%
  pivot_longer(cols = -adm_week, names_to = "adm_type", values_to = "value") %>%
  filter(value == 1) %>%
  ggplot(aes(x = as_date(adm_week), colour = adm_type)) +
  geom_freqpoly(binwidth = 7)
