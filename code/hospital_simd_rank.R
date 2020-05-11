library(dplyr) # Data manipulation
library(dbplyr) # Easy SQL extracting
library(janitor) # Cleaning variable names
library(readr) # Read SIMD lookup
library(lubridate) # Working with dates

# Set up the SMRA connection
SMRA_connection <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "SMRA",
  uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
  pwd = rstudioapi::askForPassword("SMRA Password:"),
  encoding = "ASCII"
)

extract <- tbl(SMRA_connection, "SMR01_PI") %>%
  # Filter to the records we want
  filter(
    between(
      ADMISSION_DATE,
      # Go back earlier to catch the start of CIS stays
      TO_DATE("01/01/2016", "dd/mm/yyyy"),
      TO_DATE("31/12/2018", "dd/mm/yyyy")
    )
  ) %>%
  # Use correct sort order for stays
  arrange(
    LINK_NO,
    ADMISSION_DATE,
    DISCHARGE_DATE,
    ADMISSION,
    DISCHARGE,
    URI
  ) %>%
  # Variables we want to keep
  select(
    ADMISSION_DATE,
    ADMISSION_TYPE,
    ENCRYPTED_UPI,
    CIS_MARKER,
    LOCATION,
    DATAZONE_2011,
    AGE_IN_YEARS,
    SEX
  )

# From NHS Scotland Open data (SIMD2020)
# https://www.opendata.nhs.scot/dataset/scottish-index-of-multiple-deprivation/resource/acade396-8430-4b34-895a-b3e757fa346e
simd_lookup <- read_csv("https://www.opendata.nhs.scot/dataset/78d41fa9-1a62-4f7b-9edb-3e8522a93378/resource/acade396-8430-4b34-895a-b3e757fa346e/download/simd2020_02042020.csv",
  col_types = "c____i__________"
) %>%
  clean_names()

# Set codes for non-elective admissions
non_el_codes <-
  c(20, 21, 22, 30, 31, 32, 33, 34, 35, 36, 38, 39)

# Do the extract, as dblplyr doesn't do summarising well yet.
data <- collect(extract) %>%
  # clean up variable names
  clean_names() %>%
  # Aggregate to stay level
  group_by(encrypted_upi, cis_marker) %>%
  summarise(
    admission_date = min(admission_date),
    # Work out if the stay was elective or non-elective
    non_elective = if_else(first(admission_type) %in% non_el_codes,
                           TRUE,
                           FALSE
    ),
    dag_id = first(location), # Use UoE varname for hosp
    datazone_2011 = first(datazone_2011),
    age_in_years = first(age_in_years),
    sex = first(sex)
  ) %>%
  # Only keep stays which oringinally admitted in 2018
  filter(admission_date >= dmy("01/01/2018"))

# Add age bands and sex as factor
data_clean <- data %>% 
  mutate(
    age.factor = case_when(
      age_in_years < 10 ~ "<10",
      age_in_years < 20 ~ "10-19",
      age_in_years < 30 ~ "20-29",
      age_in_years < 40 ~ "30-39",
      age_in_years < 50 ~ "40-49",
      age_in_years < 60 ~ "50-59",
      age_in_years < 70 ~ "60-69",
      age_in_years < 80 ~ "70-79",
      is.na(age_in_years) ~ NA_character_,
      TRUE ~ "80+"
    ) %>%
      factor(),
    sex.factor = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    ) %>% 
      factor()
  )

# Add on SIMD ranks by datazone
data_clean <- left_join(data_clean, simd_lookup, by = c("datazone_2011" = "data_zone"))

# Work out the arithmetic mean SIMD rank per hospital
data_clean %>%
  group_by(dag_id) %>%
  summarise(mean_simd = mean(simd2020rank, na.rm = TRUE)) %>%
  write_csv("hospital_simd_rank.csv")
  
# Work out the arithmetic mean SIMD rank per hospital by age and sex
data_clean %>% 
  group_by(dag_id, age.factor, sex.factor) %>%
  summarise(mean_simd = mean(simd2020rank, na.rm = TRUE)) %>% 
  write_csv("hospital_simd_rank_age_sex.csv")



