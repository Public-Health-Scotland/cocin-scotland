source("extract-data/00_setup-environment.R")

# Create COCIN admissions

cocin <- read_rds(path(server_dir, str_glue("{date}_cocin-clean-data.rds", 
                                            date = latest_server_data("cocin")))) 

#########################################################
#################### STUDY IDENTIFIERS ##################
#########################################################

studyidentifiers <- cocin %>%
  mutate(
    # Take the date of a positive test result
    swabdate = case_when(any(mborres == "Positive") ~ mbdat)) %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    hospitalcode = first(na.omit(hospid)),
    dob_cocin = first(na.omit(agedat)),
    swabdate = first(na.omit(swabdate))
  )

#########################################################
################### HOSPITAL/WARD INFO ##################
#########################################################

hospitalward <- cocin %>%
  group_by(subjid) %>%
  mutate(
    # Discharge Date from Hospital
    dischargedate_cocin = case_when(any(dsterm %in% c("Discharged alive", "Death",
                                                      "Palliative discharge")) ~ dsstdtc)
  ) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Admission Date
    admitdate_cocin = first(na.omit(hostdat)),
    # Discharge Date
    dischargedate_cocin = first(na.omit(dischargedate_cocin))
  ) 


#########################################################
############## PATIENT CHARACTERISTICS ##################
#########################################################

patientchar <- cocin %>%
  group_by(subjid) %>%
  mutate(
    age_y = case_when(age_estimateyearsu == "Years" ~ first(na.omit(age_estimateyears))),
    age_m = case_when(age_estimateyearsu == "Months" ~ first(na.omit(age_estimateyears)))
  ) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    sex_cocin = first(na.omit(sex)),
    age_y = first(na.omit(age_y)),
    age_m = first(na.omit(age_m)),
    smoking = first(na.omit(smoking_mhyn)),
    pregnant = first(na.omit(pregyn_rptestcd)),
    trimester = first(na.omit(egestage_rptestcd)),
    postpartum = first(na.omit(postpart_rptestcd)),
    hcw = first(na.omit(healthwork_erterm)), 
    ethnicity_cocin = first(na.omit(ethnicity)),
    ethnicity_grouped_cocin = first(na.omit(ethnicity_grouped))
  ) 

#########################################################
######### CASE SEVERITY - COVID OR NOT ##################
#########################################################



#########################################################
########### CASE SEVERITY - SYMPTOMS ####################
#########################################################

caseseverity_symptoms <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    fever = first(na.omit(fever)),
    malaise = first(na.omit(fatigue_ceoccur_v2)),
    headache = first(na.omit(headache_ceoccur_v2)),
    myalgia = first(na.omit(myalgia_ceoccur_v2)),
    sorethroat = first(na.omit(sorethroat_ceoccur_v2)),
    cough = first(na.omit(cough_ceoccur_v2)),
    sob = first(na.omit(shortbreath_ceoccur_v2)),
    vomit = first(na.omit(vomit_ceoccur_v2)),
    diarr = first(na.omit(diarrhoea_ceoccur_v2)),
    abdopain = first(na.omit(abdopain_ceoccur_v2)),
    ageusia = first(na.omit(ageusia_ceoccur_v2)),
    anosmia = first(na.omit(anosmia_ceoccur_v2)),
    confusion = first(na.omit(confusion_ceoccur_v2)),
    chest = first(na.omit(chestpain_ceoccur_v2)),
    conjunct = first(na.omit(conjunct_ceoccur_v2)),
    dermato = first(na.omit(rash_ceoccur_v2)),
    onsetdate = first(na.omit(cestdat))
  ) 

#########################################################
######## CASE SEVERITY - SEVERITY INDICATORS ############
#########################################################


caseseverity_ind <- cocin %>%
  group_by(subjid) %>%
  mutate(
    ddeath = case_when(any(dsterm == "Death") ~ dsstdtc),
    ventilation = case_when(
      any(invasive_proccur == "YES") ~ "Invasive",
      any(noninvasive_proccur == "YES") ~ "NonInvasive",
      # Commented this out as definitions of high-flow oxygen are different (2l/min v
      # 6l/min)
      any(oxygenhf_cmoccur == "1") ~ "Oxygen (high-flow)",
      any(extracorp_prtrt == "YES") ~ "ECMO",
      # Specify if patient has had none of the above treatments
      any(invasive_proccur == "NO") & any(noninvasive_proccur == "NO")
      & any(oxygenhf_cmoccur == "2") & any(extracorp_prtrt == "NO") ~ "None"
    )
  ) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    outcome_cocin = first(na.omit(dsterm)),
    vent = first(na.omit(ventilation))
  ) 

#########################################################
######## RISK FACTORS - CLOSE CONTACT SETTING ###########
#########################################################

rkclosecontact <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Close contact
    contact = first(na.omit(symptoms_epi_physical))
  )

#########################################################
############# RISK FACTORS - EXAMS/LAB ##################
#########################################################

rkexamlab <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    oxsat = first(na.omit(oxy_vsorres)),
    bloodgroup = first(na.omit(bloodgroup))
  )


#########################################################
########## UNDERLYING CHRONIC CONDITIONS ################
#########################################################

underlyingcc <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    anaemia = first(na.omit(aneamia_ceterm)),
    asthma = first(na.omit(asthma_mhyn)),
    cancer = first(na.omit(malignantneo_mhyn)),
    hypert = first(na.omit(hypertension_mhyn)),
    dementia = first(na.omit(dementia_mhyn)),
    diabetes = first(na.omit(diabetes_mhyn)),
    heartdis = first(na.omit(chrincard)),
    immuno   = first(na.omit(aidshiv_mhyn)),
    liverdis = first(na.omit(modliv)),
    obese = first(na.omit(obesity_mhyn)),
    rendis = first(na.omit(renal_mhyn)),
    rheumat = first(na.omit(rheumatologic_mhyn)),
    stroke = first(na.omit(stroke_ceterm))
  ) 

#########################################################
############# RISK FACTORS - MEDICATIONS ################
#########################################################

rkhospmedc <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    prone = first(na.omit(pronevent_prtrt)),
    plasma = first(na.omit(conv_plasma_cmyn))
  ) 

#########################################################
################ FINAL ISARIC DATASET ###################
#########################################################

## Merge all data to create final data set
cocin_admissions <- list(
  studyidentifiers, hospitalward, patientchar,
  caseseverity_ind, caseseverity_symptoms, rkclosecontact,
  rkexamlab, underlyingcc, rkhospmedc
) %>%
  reduce(merge, by = c("subjid","nhs_chi")) %>%
  rename(chi_number = nhs_chi)

# COCIN with CHI - remove duplicate admissions
cocin_chi <- cocin_admissions %>%
  filter(!is.na(chi_number)) %>%
  group_by(chi_number, admitdate_cocin) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  ungroup

# COCIN without CHI
cocin_nonchi <- cocin_admissions %>%
  filter(is.na(chi_number))

# Join data together
cocin_admissions <- bind_rows(cocin_chi, cocin_nonchi)

# Remove datasets no longer needed
rm(
  cocin, studyidentifiers, hospitalward, patientchar,
  caseseverity_ind, caseseverity_symptoms, rkclosecontact,
  rkclosecontact, rkexamlab, underlyingcc, rkhospmedc,
  cocin_chi, cocin_nonchi
)