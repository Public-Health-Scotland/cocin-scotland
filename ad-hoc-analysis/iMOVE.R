## Enhanced Surveillance Cell
## ICU and Hospital Workstream
## I-MOVE Data Prep
## Nicole Jarvie
## 20th May 2020

## This code follows on from 00 - Get Scottish data and 01 -Prep data
## Requires the prepared Scottish Data

## Libraries
source("extract-data/00_setup-environment.R")

if (!vpn_active() & latest_extract_date() < today()) {
  message("Local COCIN extract is out of date, getting new one")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
} else {
  message(if_else(latest_extract_date() == today(),
    "Not updating extract as it's up-to-date",
    "Disconnect from the VPN to perform the extract"
  ))
}

scot_data <- read_rds(str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date()))

## Study Identifiers
## Includes - country, patient ID, hospital and consent

studyidentifiers <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    hospitalcode = first(na.omit(hospid)),
    idcountry = "Scotland",
    id = first(na.omit(subjid)),
    consent = "NA",
    consent_sp = "NA"
  )

## Hospital and ward information

hospitalward <- scot_data %>%
  group_by(subjid) %>%
  mutate(
    swabdate = case_when(any(mborres == "Positive") ~ mbdat),
    dischargedate = case_when(any(dsterm == "Death") ~ dsstdtcyn)
  ) %>%
  summarise(
    prevhosp = NA,
    admitdate = first(na.omit(hostdat)),
    dischargedate = first(na.omit(dischargedate)),
    multiple_hosp = NA,
    los_hosp = NA,
    hospitalward = NA,
    hospitalward_oth = NA
  )

icuward <- scot_data %>%
  filter(redcap_repeat_instrument != "Infectious Respiratory Disease Pathogen Testing" |
    is.na(redcap_repeat_instrument)) %>%
  filter(redcap_repeat_instrument != "Pre-admission medication" |
    is.na(redcap_repeat_instrument)) %>%
  mutate(swabdate = case_when(any(mborres == "Positive") ~ mbdat)) %>%
  group_by(subjid) %>%
  summarise(
    icu_hoterm = case_when(
      any(icu_hoterm == "Yes") ~ "Yes",
      any(icu_hoterm == "No") ~ "No",
      TRUE ~ NA_character_
    ),
    daily_hoterm = case_when(
      any(daily_hoterm == "Yes") ~ "Yes",
      any(daily_hoterm == "No") ~ "No",
      TRUE ~ NA_character_
    ),
    icu = case_when(
      daily_hoterm == "Yes" | icu_hoterm == "Yes" ~ "Yes",
      is.na(daily_hoterm) & is.na(icu_hoterm) ~ NA_character_,
      TRUE ~ "No"
    ),
    icuadmitdate = first(na.omit(icu_hostdat)),
    icudisdate = first(na.omit(icu_hoendat)),
    icu_los = case_when((as.Date(icudisdate) >= as.Date(icuadmitdate)) & !(is.na(icuadmitdate) & is.na(icudisdate)) ~ as.Date(icudisdate) - as.Date(icuadmitdate)),
    swabdate = first(na.omit(swabdate))
  )

icuward <- select(icuward, -c(2, 3))

hospitalward <- merge(hospitalward, icuward, by = "subjid")
rm(icuward)

## Patient Characteristics

patientchar <- scot_data %>%
  group_by(subjid) %>%
  mutate(
    age_y = case_when(age_estimateyearsu == "Years" ~ first(na.omit(age_estimateyears))),
    age_m = case_when(age_estimateyearsu == "Months" ~ first(na.omit(age_estimateyears)))
  ) %>%
  summarise(
    sex = first(na.omit(sex)),
    dob = NA,
    age_y = first(na.omit(age_y)),
    age_m = first(na.omit(age_m)),
    residence = NA,
    postcode = substr(first(na.omit(Postcode)), 1, 3),
    smoking = first(na.omit(smoking_mhyn)),
    pregnant = first(na.omit(pregyn_rptestcd)),
    trimester = first(na.omit(egestage_rptestcd)),
    postpartum = first(na.omit(postpart_rptestcd)),
    hcw = first(na.omit(healthwork_erterm))
  )

## Disease information
## Commenting out for now

## diseaseinfo <- scot_data %>%
## group_by(subjid) %>%
## mutate(outcome = case_when(any(dsterm == "Death") ~ "Death",
##                             any(dsterm == "Discharged alive") ~ "Alive"),
##     deathdate = case_when(any(dsterm =="Death") ~ dsstdtc)) %>%
## summarise(
## outcome = first(na.omit(outcome)),
## deathdate = first(na.omit(deathdate))
## )


## Case/Severity Information

caseseverity <- scot_data %>%
  group_by(subjid) %>%
  mutate(
    lab_covtest = case_when(any(mbperf == "YES") ~ "Yes"),
    labcovid = case_when(
      any(corna_mbcat == "YES - Confirmed") ~ "Confirmed",
      any(corna_mbcat == "YES - Probable") ~ "Probable",
      any(corna_mbcat == "NO") ~ "No"
    ),
    covid = case_when(any(corna_mbcat == "YES - Confirmed") ~ corna_mbcaty),
    ddeath = case_when(any(dsterm == "Death") ~ dsstdtc),
    ventilation = case_when(
      any(noninvasive_proccur == "YES") ~ "NonInvasive",
      any(invasive_proccur == "YES") ~ "Invasive"
    )
  ) %>%
  summarise(
    lab_covtest = first(na.omit(lab_covtest)),
    lab_covtesttype = NA,
    lab_covtesttype_sp = NA,
    lab_covid = first(na.omit(labcovid)),
    covid = first(na.omit(covid)),
    healthcare_contact = NA,
    feverishness = first(na.omit(fever)),
    fever = NA,
    malaise = first(na.omit(fatigue_ceoccur_v2)),
    headache = first(na.omit(headache_ceoccur_v2)),
    myalgia = first(na.omit(myalgia_ceoccur_v2)),
    sorethroat = first(na.omit(sorethroat_ceoccur_v2)),
    cough = first(na.omit(cough_ceoccur_v2)),
    suddenonset = NA,
    sob = first(na.omit(shortbreath_ceoccur_v2)),
    general_deter = NA,
    vomit = first(na.omit(vomit_ceoccur_v2)),
    diarr = first(na.omit(diarrhoea_ceoccur_v2)),
    abdopain = first(na.omit(abdopain_ceoccur_v2)),
    ageusia = NA,
    chills = NA,
    tach = NA,
    coryza = NA,
    dizzy = NA,
    confusion = NA,
    chest = first(na.omit(chestpain_ceoccur_v2)),
    palp = NA,
    conjunct = first(na.omit(conjunct_ceoccur_v2)),
    rash = first(na.omit(rash_ceoccur_v2)),
    onsetdate = first(na.omit(cestdat)),
    outcome = first(na.omit(dsterm)),
    deathdate = first(na.omit(ddeath)),
    caseofdeath = NA,
    vent = first(na.omit(ventilation)),
    bilat_pneu = NA,
    pneu_any = case_when(
      any(vrialpneu_ceoccur == "YES") ~ "Vrialpneu",
      any(bactpneu_ceoccur == "YES") ~ "Bactpneu"
    )
  )


## Risk Factors (close contact setting)

rkclosecontact <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    closecont = first(na.omit(symptoms_epi_physical)),
    closecont_sp = NA,
    closecont_type = NA
  )


## Risk Factors - Exams/Lab

rkexamlab <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    ct_us_ecg = case_when(any(xray_prperf == "YES") ~ "Yes", any(xray_prperf == "NO") ~ "No"),
    ct_res = NA,
    ct_res_sp = NA,
    cxr = case_when(any(infiltrates_faorres == "YES") ~ "Yes", any(infiltrates_faorres == "NO") ~ "No"),
    examoth_sp = NA,
    ecg_qt = NA,
    oxsat = first(na.omit(oxy_vsorres)),
    seq = NA,
    genetic_group = NA
  )

## Underlying chronic conditions

underlyingcc <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    anaemia = case_when(
      any(aneamia_ceterm == "YES") ~ "Yes",
      any(aneamia_ceterm == "NO") ~ "No"
    ),
    asplenia = NA,
    asthma = case_when(
      any(asthma_mhyn == "YES") ~ "Yes",
      any(asthma_mhyn == "NO") ~ "No"
    ),
    cancer = case_when(
      any(malignantneo_mhyn == "YES") ~ "Yes",
      any(malignantneo_mhyn == "NO") ~ "No"
    ),
    hypert = NA,
    dementia = case_when(
      any(dementia_mhyn == "YES") ~ "Yes",
      any(dementia_mhyn == "NO") ~ "No"
    ),
    diabetes = case_when(
      any(diabetes_mhyn == "YES") ~ "Yes",
      any(diabetes_mhyn == "NO") ~ "No"
    ),
    heartdis = case_when(
      any(chrincard == "YES") ~ "Yes",
      any(chrincard == "No") ~ "No"
    ),
    immuno = case_when(
      any(aidshiv_mhyn == "YES") ~ "Yes",
      any(aidshiv_mhyn == "No") ~ "No"
    ),
    liverdis = case_when(
      any(modliv == "YES") ~ "Yes",
      any(modliv == "No") ~ "No"
    ),
    lungdis = NA,
    lungdis_sp = NA,
    neuromusc = NA,
    Height = NA,
    Weight = NA,
    Bmi = NA,
    obese = case_when(
      any(obesity_mhyn == "YES") ~ "Yes",
      any(obesity_mhyn == "NO") ~ "No"
    ),
    Rendis = case_when(
      any(renal_mhyn == "YES") ~ "Yes",
      any(renal_mhyn == "NO") ~ "No"
    ),
    rheumat = case_when(
      any(rheumatologic_mhyn == "YES") ~ "Yes",
      any(rheumatologic_mhyn == "NO") ~ "No"
    ),
    stroke = NA,
    tuberc = NA
  )


## Risk Factors - In Hospital medication

rkhospmedc <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    ox_nasal = NA,
    prone = case_when(
      any(daily_prone_cmtrt == "YES") ~ "Yes",
      any(daily_prone_cmtrt == "NO") ~ "No"
    ),
    nebu = NA
  )


## Vaccine (at later date)
## Set all values to NA for now

vaccine <- scot_data %>%
  group_by(subjid) %>%
  summarise(
    panvaccany = NA,
    panvaccdate1 = NA,
    panvaccdate2 = NA,
    panvacctype = NA,
    panvaccdose = NA
  )

## Merge all data to create final data set

completedataset <- list(
  studyidentifiers, hospitalward, patientchar, caseseverity,
  rkclosecontact, rkexamlab, underlyingcc, rkhospmedc, vaccine
) %>%
  reduce(merge, by = "subjid")

completedataset <- select(completedataset, -c(1))

rm(
  studyidentifiers, hospitalward, patientchar, caseseverity,
  rkclosecontact, rkexamlab, underlyingcc, rkhospmedc, vaccine
)
