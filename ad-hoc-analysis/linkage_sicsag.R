source("extract-data/00_setup-environment.R")

# Read in SMR01 extract
icu <- read_rds(path(here("data", str_glue("SICSAG_extract.rds"))))

# Prepare data for matching - create ICU 'stays'
icu <- icu %>%
  select(ChiNo, AdmitUnit, DiscDate, covidICUorHDU) %>%
  rename(chi_number = ChiNo) %>%
  mutate(AdmitUnit = as.Date(AdmitUnit, format="%d-%B-%Y"),
         DiscDate  = as.Date(DiscDate, format="%d-%B-%Y")) %>%
  arrange(chi_number, AdmitUnit, DiscDate) %>%
  group_by(chi_number) %>%
  # If ICU admission date is the same (or within 1 day) of the previous admission,
  # count as in same stay
  mutate(row = row_number(),
         stay_marker = ifelse(AdmitUnit <= (lag(DiscDate, n = 1L) + 1), 0, row)) %>%
  mutate(stay_marker = ifelse(is.na(stay_marker), 1, stay_marker))


# For loop ensures we assign every record to the correct stay
for(i in 1:10){
  
  icu <- icu %>%
    mutate(stay_marker = ifelse(stay_marker == 0, lag(stay_marker, n = 1L), stay_marker))
  
}

# Group up into one row per stay
icu <- icu %>%
  group_by(chi_number, stay_marker) %>%
  summarise(AdmitUnit = min(AdmitUnit),
            DiscDate  = max(DiscDate),
            covidICUorHDU = first(na.omit(covidICUorHDU)))


# Get CHIs in data with ICU data
icu_data <- rapid_data %>%
  # Join data on
  inner_join(icu, by = "chi_number") %>%
  # Recode variables
  mutate(icuadmitdate = AdmitUnit,
         icudisdate   = DiscDate) %>%
  # Calc LOS 
  mutate(los_icu = icudisdate - icuadmitdate) %>%
  # Select only variables required
  select(chi_number, adm_date, dis_date, icuadmitdate, icudisdate, los_icu, 
         covidICUorHDU) %>%
  # Ensure ICU admission date is within the hospital admission dates
  mutate(interval_marker_adm = ifelse(!icuadmitdate %within% 
                                        (adm_date %--% dis_date), 1, 0)) %>%
  filter(interval_marker_adm == 0) %>%
  # Ensure ICU discharge date is within the hospital admission dates
  mutate(interval_marker_dis = ifelse(!icudisdate %within% 
                                        (adm_date %--% dis_date), 1, 0)) %>%
  filter(interval_marker_dis == 0) %>%
  group_by(chi_number, adm_date) %>%
  summarise(icuadmitdate = min(icuadmitdate),
            icudisdate   = max(icudisdate),
            los_icu = sum(los_icu),
            icu = "Yes",
            covidICUorHDU = first(na.omit(covidICUorHDU)))

# Add in ICU data
rapid_icu <- rapid_data %>%
  # Join data on
  left_join(icu_data, by = c("chi_number","adm_date")) %>%
  # NAs coded as No
  mutate(icu = ifelse(is.na(icu), 'No', icu)) %>%
  select(chi_number, adm_date, icu, icuadmitdate, icudisdate, los_icu, covidICUorHDU)

rm(icu, icu_data)