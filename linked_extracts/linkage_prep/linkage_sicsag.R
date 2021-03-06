source("extract-data/00_setup-environment.R")

# Read in SICSAG extract
sicsag_extract <- read_rds(path(server_dir, str_glue("{today()}_SICSAG_extract.rds")))

# Prepare data for matching - create ICU 'stays'
icu <- sicsag_extract %>%
  select(ChiNo, AdmitUnit, DiscDate, covidICUorHDU) %>%
  rename(chi_number = ChiNo) %>%
  mutate(chi_number = as.character(chi_number)) %>%
  mutate(chi_number = chi_pad(chi_number)) %>% # Fix missing leading zeros
  filter(chi_check(chi_number) == "Valid CHI") %>% # Remove any rows with bad CHI numbers
  mutate(
    AdmitUnit = as.Date(AdmitUnit, format = "%m/%d/%Y"),
    DiscDate = as.Date(DiscDate, format = "%m/%d/%Y")
  ) %>%
  arrange(chi_number, AdmitUnit, DiscDate) %>%
  group_by(chi_number) %>%
  # If ICU admission date is the same (or within 1 day) of the previous admission,
  # count as in same stay
  mutate(
    row = row_number(),
    stay_marker = ifelse(AdmitUnit <= (lag(DiscDate, n = 1L) + 1), 0, row)
  ) %>%
  mutate(stay_marker = ifelse(is.na(stay_marker), 1, stay_marker))


# For loop ensures we assign every record to the correct stay
for (i in 1:10) {
  icu <- icu %>%
    mutate(stay_marker = ifelse(stay_marker == 0, lag(stay_marker, n = 1L), stay_marker))
}

# Group up into one row per stay
icu <- icu %>%
  # Calc LOS
  mutate(los_icu = DiscDate - AdmitUnit) %>%
  group_by(chi_number, stay_marker) %>%
  summarise(
    AdmitUnit = min(AdmitUnit),
    DiscDate = max(DiscDate),
    covidICUorHDU = first(na.omit(covidICUorHDU)),
    los_icu = as.numeric(sum(los_icu))
  ) %>%

  # Clean up any los where this is longer than possible (caused by overlapping dates)
  # The remove any los for episodes where the dates match the los
  mutate(
    derived_los = time_length(AdmitUnit %--% DiscDate, "days"),
    los_icu = if_else(los_icu >= derived_los, NA_real_, los_icu),
    los_icu = as.integer(los_icu)
  )

# Get CHIs in data with ICU data
icu_data <- rapid_data %>%
  # Join data on
  inner_join(icu, by = "chi_number") %>%
  # Recode variables
  mutate(
    icuadmitdate = AdmitUnit,
    icudisdate = DiscDate
  ) %>%
  # Select only variables required
  select(
    chi_number, adm_date, dis_date, icuadmitdate, icudisdate, los_icu,
    covidICUorHDU
  ) %>%
  # Ensure ICU admission date is within the hospital admission dates
  mutate(interval_marker_adm = ifelse(!icuadmitdate %within%
    (adm_date %--% dis_date), 1, 0)) %>%
  filter(interval_marker_adm == 0) %>%
  # Ensure ICU discharge date is within the hospital admission dates
  mutate(interval_marker_dis = ifelse(!icudisdate %within%
    (adm_date %--% dis_date), 1, 0)) %>%
  filter(interval_marker_dis == 0) %>%
  group_by(chi_number, adm_date) %>%
  summarise(
    icuadmitdate = min(icuadmitdate),
    icudisdate = max(icudisdate),
    los_icu = sum(los_icu),
    icu = "Yes",
    covidICUorHDU = first(na.omit(covidICUorHDU))
  )

# Add in ICU data
rapid_icu <- rapid_data %>%
  # Join data on
  left_join(icu_data, by = c("chi_number", "adm_date")) %>%
  # NAs coded as No
  mutate(icu = ifelse(is.na(icu), "No", icu)) %>%
  select(chi_number, adm_date, icu, icuadmitdate, icudisdate, los_icu, covidICUorHDU)

rm(sicsag_extract, icu, icu_data)
