source("extract-data/00_setup-environment.R")

# Create/Update COCIN admissions
source("ad-hoc-analysis/COCIN_admissions_data.R")

##############################
######### JOIN BY CHI ########
##############################


# COCIN with CHI numbers for joining - Normal Admission Dates
cocin_adms_chi <- cocin_admissions %>%
  filter(!is.na(chi_number)) %>%
  mutate(adm_date = admitdate_cocin)

# COCIN with CHI numbers for joining - Admission Dates - 1 day
cocin_adms_chi_minus1 <- cocin_admissions %>%
  filter(!is.na(chi_number)) %>%
  mutate(adm_date = admitdate_cocin - 1)

# COCIN with CHI numbers for joining - Admission Dates + 1 day
cocin_adms_chi_plus1 <- cocin_admissions %>%
  filter(!is.na(chi_number)) %>%
  mutate(adm_date = admitdate_cocin + 1)

# COCIN with missing CHI numbers for joining - adm date, sex, DOB, and hospital
# Create joining variables
cocin_adms_nonchi_1 <- cocin_admissions %>%
  filter(is.na(chi_number)) %>%
  mutate(
    adm_date = admitdate_cocin,
    sex = sex_cocin,
    hospital_of_treatment_code = hospitalcode,
    dob = dob_cocin
  ) %>%
  # Remove CHI as not needed
  select(-chi_number) %>%
  mutate(sex = case_when(
    sex == "Male" ~ "M",
    sex == "Female" ~ "F",
    sex == "Not specified" ~ "U",
    TRUE ~ NA_character_
  ))

# COCIN with missing CHI numbers for joining- adm date, sex, DOB, and test date
# Create joining variables
cocin_adms_nonchi_2 <- cocin_admissions %>%
  filter(is.na(chi_number)) %>%
  mutate(
    adm_date = admitdate_cocin,
    sex = sex_cocin,
    dob = dob_cocin,
    test_date = swabdate
  ) %>%
  # Remove CHI as not needed
  select(-chi_number) %>%
  mutate(sex = case_when(
    sex == "Male" ~ "M",
    sex == "Female" ~ "F",
    sex == "Not specified" ~ "U",
    TRUE ~ NA_character_
  ))


#### CHI LINKAGE ####

# RAPID with CHI COCIN - Exact Adm Dates
rapid_cocin_chi_same <- rapid_data %>%
  inner_join(cocin_adms_chi, by = c("chi_number", "adm_date"))

# Join on data with admission day 1 day out
rapid_cocin_chi_minus1 <- rapid_data %>%
  # first, filter out data that's already been matched
  anti_join(cocin_adms_chi, by = c("chi_number", "adm_date")) %>%
  # Match on by adm date minus 1 dataset
  inner_join(cocin_adms_chi_minus1, by = c("chi_number", "adm_date"))

# Join on data with admission day 1 day out
rapid_cocin_chi_plus1 <- rapid_data %>%
  # first, filter out data that's already been matched
  anti_join(cocin_adms_chi, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_minus1, by = c("chi_number", "adm_date")) %>%
  # Match on by adm date plus 1 dataset
  inner_join(cocin_adms_chi_plus1, by = c("chi_number", "adm_date"))


#### NON-CHI LINKAGE ####

# Try Link on non CHI data
rapid_cocin_nonchi_1 <- rapid_data %>%
  # first, filter out data that's already been matched
  anti_join(cocin_adms_chi, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_minus1, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_plus1, by = c("chi_number", "adm_date")) %>%
  inner_join(cocin_adms_nonchi_1, by = c(
    "adm_date", "sex",
    "dob", "hospital_of_treatment_code"
  ))

rapid_cocin_nonchi_2 <- rapid_data %>%
  # first, filter out data that's already been matched
  anti_join(cocin_adms_chi, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_minus1, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_plus1, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_nonchi_1, by = c(
    "adm_date", "sex",
    "dob", "hospital_of_treatment_code"
  )) %>%
  inner_join(cocin_adms_nonchi_2, by = c(
    "adm_date", "sex",
    "dob", "test_date"
  ))

#### FINAL LINKED DATASET ####

rapid_cocin <- rapid_data %>%
  # first, filter out data that's already been matched
  anti_join(cocin_adms_chi, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_minus1, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_chi_plus1, by = c("chi_number", "adm_date")) %>%
  anti_join(cocin_adms_nonchi_1, by = c(
    "adm_date", "sex",
    "dob", "hospital_of_treatment_code"
  )) %>%
  anti_join(cocin_adms_nonchi_2, by = c(
    "adm_date", "sex",
    "dob", "test_date"
  )) %>%
  # Bind all rows together
  bind_rows(
    rapid_cocin_chi_same, rapid_cocin_chi_minus1, rapid_cocin_chi_plus1,
    rapid_cocin_nonchi_1, rapid_cocin_nonchi_2
  )

rm(
  cocin_admissions, cocin_adms_chi, cocin_adms_chi_minus1, cocin_adms_chi_plus1,
  cocin_adms_nonchi_1, cocin_adms_nonchi_2,
  rapid_cocin_chi_minus1, rapid_cocin_chi_plus1, rapid_cocin_chi_same,
  rapid_cocin_nonchi_1, rapid_cocin_nonchi_2
)
