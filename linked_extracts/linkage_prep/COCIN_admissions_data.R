source("extract-data/00_setup-environment.R")

# Create COCIN admissions

cocin <- read_rds(path(
  server_dir,
  str_glue("{today()}_cocin_extract_clean.rds")
))

#########################################################
#################### STUDY IDENTIFIERS ##################
#########################################################

studyidentifiers <- cocin %>%
  # Take the date of a positive test result
  mutate(swabdate = if_else(mborres == "Positive", mbdat, NA_Date_)) %>%
  group_by(subjid) %>%
  summarise(
    nhs_chi = first(na.omit(nhs_chi)), # Include CHI for linkage
    hospitalcode = first(na.omit(hospid)),
    dob_cocin = first(na.omit(agedat)),
    swabdate = min(na.omit(swabdate))
  )

#########################################################
################### HOSPITAL/WARD INFO ##################
#########################################################

hospitalward <- cocin %>%
  group_by(subjid) %>%
  mutate(
    # Discharge Date from Hospital
    dischargedate_cocin = case_when(any(dsterm %in% c(
      "Discharged alive", "Death",
      "Palliative discharge"
    )) ~ dsstdtc)
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
    age_y = case_when(age_estimateyearsu == "Years" ~
    first(na.omit(age_estimateyears))),
    age_m = case_when(age_estimateyearsu == "Months" ~
    first(na.omit(age_estimateyears)))
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
    hcw_cocin = first(na.omit(healthwork_erterm)),
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
      # Commented this out as definitions of high-flow
      # oxygen are different (2l/min v 6l/min)
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
    asthma = first(na.omit(asthma_mhyn)),
    cancer = first(na.omit(malignantneo_mhyn)),
    hypert = first(na.omit(hypertension_mhyn)),
    dementia = first(na.omit(dementia_mhyn)),
    diabetes = first(na.omit(diabetes_mhyn)),
    heartdis = first(na.omit(chrincard)),
    immuno = first(na.omit(aidshiv_mhyn)),
    liverdis = first(na.omit(modliv)),
    obese = first(na.omit(obesity_mhyn)),
    rendis = first(na.omit(renal_mhyn)),
    rheumat = first(na.omit(rheumatologic_mhyn))
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

##### The following are variables required for WP4 ######

#########################################################
########## WP4 - IN-HOSPITAL MEDICATIONS ################
#########################################################

# Mispellings of specific antibiotics
alt_azith <- c('azithromicin')
alt_ceftr <- c('ceftr9axone','ceftriazone')

# Antibiotics - Set to lower case and fix mispellings of specific antibiotics
cocin <- cocin %>%
  mutate(antibiotic_cmtrt  = tolower(antibiotic_cmtrt),
         antibiotic2_cmtrt = tolower(antibiotic2_cmtrt),
         antibiotic3_cmtrt = tolower(antibiotic3_cmtrt),
         antibiotic4_cmtrt = tolower(antibiotic4_cmtrt),
         antibiotic5_cmtrt = tolower(antibiotic5_cmtrt),
         antibiotic6_cmtrt = tolower(antibiotic6_cmtrt),
         antibiotic7_cmtrt = tolower(antibiotic7_cmtrt)) %>%
  mutate(antibiotic_cmtrt  = ifelse(antibiotic_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic_cmtrt),
         antibiotic2_cmtrt = ifelse(antibiotic2_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic2_cmtrt),
         antibiotic3_cmtrt = ifelse(antibiotic3_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic3_cmtrt),
         antibiotic4_cmtrt = ifelse(antibiotic4_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic4_cmtrt),
         antibiotic5_cmtrt = ifelse(antibiotic5_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic5_cmtrt),
         antibiotic6_cmtrt = ifelse(antibiotic6_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic6_cmtrt),
         antibiotic7_cmtrt = ifelse(antibiotic7_cmtrt %in% alt_azith, 
                                    'azithromycin', antibiotic7_cmtrt)) %>%
  mutate(antibiotic_cmtrt  = ifelse(antibiotic_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic_cmtrt),
         antibiotic2_cmtrt = ifelse(antibiotic2_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic2_cmtrt),
         antibiotic3_cmtrt = ifelse(antibiotic3_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic3_cmtrt),
         antibiotic4_cmtrt = ifelse(antibiotic4_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic4_cmtrt),
         antibiotic5_cmtrt = ifelse(antibiotic5_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic5_cmtrt),
         antibiotic6_cmtrt = ifelse(antibiotic6_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic6_cmtrt),
         antibiotic7_cmtrt = ifelse(antibiotic7_cmtrt %in% alt_ceftr, 
                                    'azithromycin', antibiotic7_cmtrt))


# Antibiotics
wp4_antibiotics <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Antibiotics (no dose info)
    # Whether patient was on antibiotics
    antibiot = first(na.omit(antibiotic_cmyn)),
    # Antibiotic type
    antibiotic_type1 = first(na.omit(antibiotic_cmtrt)),
    antibiotic_type2 = first(na.omit(antibiotic2_cmtrt)),
    antibiotic_type3 = first(na.omit(antibiotic3_cmtrt)),
    antibiotic_type4 = first(na.omit(antibiotic4_cmtrt)),
    antibiotic_type5 = first(na.omit(antibiotic5_cmtrt)),
    antibiotic_type6 = first(na.omit(antibiotic6_cmtrt)),
    antibiotic_type7 = first(na.omit(antibiotic7_cmtrt))
  ) %>%
  mutate(
    antibiotic_type1 = ifelse(is.na(antibiotic_type1), "", antibiotic_type1),
    antibiotic_type2 = ifelse(is.na(antibiotic_type2), "", antibiotic_type2),
    antibiotic_type3 = ifelse(is.na(antibiotic_type3), "", antibiotic_type3),
    antibiotic_type4 = ifelse(is.na(antibiotic_type4), "", antibiotic_type4),
    antibiotic_type5 = ifelse(is.na(antibiotic_type5), "", antibiotic_type5),
    antibiotic_type6 = ifelse(is.na(antibiotic_type6), "", antibiotic_type6),
    antibiotic_type7 = ifelse(is.na(antibiotic_type7), "", antibiotic_type7)
  ) %>%
  mutate(
    antibiot_type  = paste(antibiotic_type1, antibiotic_type2, antibiotic_type3,
                           antibiotic_type4, antibiotic_type5, antibiotic_type6,
                           antibiotic_type7, sep = " ")
  ) %>%
  mutate(
    antibiot_type2 = ifelse(str_detect(antibiot_type, "azithromycin"), 4,
                            ifelse(str_detect(antibiot_type, "ceftriaxone"), 7,
                                   ifelse(antibiotic_type1 != "", 8, NA_character_)))
  ) %>%
  select(subjid, nhs_chi, antibiot, antibiot_type, antibiot_type2)


# Antivirals
wp4_antiviral <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Antiviral - Yes/No
    antivir_med = first(na.omit(antiviral_cmyn)),
    # Antiviral type
    antiviral_cmtrt___1 = first(na.omit(antiviral_cmtrt___1)),
    antiviral_cmtrt___2 = first(na.omit(antiviral_cmtrt___2)),
    antiviral_cmtrt___3 = first(na.omit(antiviral_cmtrt___3)),
    antiviral_cmtrt___4 = first(na.omit(antiviral_cmtrt___4)),
    antiviral_cmtrt___5 = first(na.omit(antiviral_cmtrt___5)),
    antiviral_cmtrt___6 = first(na.omit(antiviral_cmtrt___6)),
    antiviral_cmtrt___7 = first(na.omit(antiviral_cmtrt___7)),
    antiviral_cmtrt___8 = first(na.omit(antiviral_cmtrt___8)),
    antiviral_cmtrt___9 = first(na.omit(antiviral_cmtrt___9))
  ) %>%
  mutate(
    antiviral_cmtrt___1 = ifelse(antiviral_cmtrt___1 == "Checked","Ribavirin",""),
    antiviral_cmtrt___1 = ifelse(is.na(antiviral_cmtrt___1), "", as.character(antiviral_cmtrt___1)),
    antiviral_cmtrt___2 = ifelse(antiviral_cmtrt___2 == "Checked","Lopinavir/Ritonvir",""),
    antiviral_cmtrt___2 = ifelse(is.na(antiviral_cmtrt___2), "", as.character(antiviral_cmtrt___2)),
    antiviral_cmtrt___3 = ifelse(antiviral_cmtrt___3 == "Checked","Interferon alpha",""),
    antiviral_cmtrt___3 = ifelse(is.na(antiviral_cmtrt___3), "", as.character(antiviral_cmtrt___3)),
    antiviral_cmtrt___4 = ifelse(antiviral_cmtrt___4 == "Checked","Interferon beta",""),
    antiviral_cmtrt___4 = ifelse(is.na(antiviral_cmtrt___4), "", as.character(antiviral_cmtrt___4)),
    antiviral_cmtrt___5 = ifelse(antiviral_cmtrt___5 == "Checked","Neuraminidase inhibitors",""),
    antiviral_cmtrt___5 = ifelse(is.na(antiviral_cmtrt___5), "", as.character(antiviral_cmtrt___5)),
    antiviral_cmtrt___6 = ifelse(antiviral_cmtrt___6 == "Checked","Other",""),
    antiviral_cmtrt___6 = ifelse(is.na(antiviral_cmtrt___6), "", as.character(antiviral_cmtrt___6)),
    antiviral_cmtrt___7 = ifelse(antiviral_cmtrt___7 == "1",
                                 "Chloroquine/Hydroxychloroquine",""),
    antiviral_cmtrt___7 = ifelse(is.na(antiviral_cmtrt___7), "", as.character(antiviral_cmtrt___7)),
    antiviral_cmtrt___8 = ifelse(antiviral_cmtrt___8 == "1","Remdesivir",""),
    antiviral_cmtrt___8 = ifelse(is.na(antiviral_cmtrt___8), "", as.character(antiviral_cmtrt___8)),
    antiviral_cmtrt___9 = ifelse(antiviral_cmtrt___9 == "1","IL6 inhibitor",""),
    antiviral_cmtrt___9 = ifelse(is.na(antiviral_cmtrt___9), "", as.character(antiviral_cmtrt___9))
  ) %>%
  mutate(
    antivirtype = paste(antiviral_cmtrt___1, antiviral_cmtrt___2, antiviral_cmtrt___3, 
                        antiviral_cmtrt___4, antiviral_cmtrt___5, antiviral_cmtrt___7, 
                        antiviral_cmtrt___8, antiviral_cmtrt___9, antiviral_cmtrt___6,
                        sep = " ")
  ) %>%
  select(subjid, nhs_chi, antivir_med, antivirtype)

# Hydroxycholorquine/Chloroquine


# Corticosteroids
wp4_cortico <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Corticosteroids
    # Yes/No
    cortico = first(na.omit(corticost_cmyn)),
    # Route
    corticost_type = first(na.omit(corticost_cmroute))
  )

# IL-6 blockers
# Corticosteroids
wp4_il6 <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # IL6 blockers
    il6 = first(na.omit(antiviral_cmtrt___9)),
    il6_type = first(na.omit(il6_cmtrt))
  )

# Combine


#########################################################
############## WP4 - FRAILTY SCORES #####################
#########################################################

wp4_frailty <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # AVPU score
    avpu = first(na.omit(avpu_vsorres)),
    # Clinical frailty score
    frailty_cfs = first(na.omit(clinical_frailty)),
    # Clinical Frailty Score? - Yes/No
    frailty_any = ifelse(!is.na(frailty_cfs), 1, 0),
    # frailty type
    frailty_type = ifelse(!is.na(frailty_cfs), 2, NA),
    # GCS score taken - yes/no
    gcs = first(na.omit(daily_gcs_lbyn)),
    # Total GCS score
    gcs_total = first(na.omit(daily_gcs_vsorres))
  )


#########################################################
############## WP4 - COMPLICATIONS ######################
#########################################################

wp4_complic <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # AVPU score
    vrialpneu_ceoccur       = first(na.omit(vrialpneu_ceoccur)),
    bactpneu_ceoccur        = first(na.omit(bactpneu_ceoccur)),
    ards_ceoccur            = first(na.omit(ards_ceoccur)),
    cryptogenic_ceterm      = first(na.omit(cryptogenic_ceterm)),
    pneumothorax_ceterm     = first(na.omit(pneumothorax_ceterm)),
    pleuraleff_ceterm       = first(na.omit(pleuraleff_ceterm)),
    bronchio_ceterm         = first(na.omit(bronchio_ceterm)),
    meningitis_ceterm       = first(na.omit(meningitis_ceterm)),
    seizure_ceterm          = first(na.omit(seizure_ceterm)),
    stroke_ceterm           = first(na.omit(stroke_ceterm)),
    neuro_comp              = first(na.omit(neuro_comp)),
    heartfailure_ceterm     = first(na.omit(heartfailure_ceterm)),
    endocarditis_aeterm     = first(na.omit(endocarditis_aeterm)),
    myocarditis_ceterm      = first(na.omit(myocarditis_ceterm)),
    cardiomyopathy_ceterm   = first(na.omit(cardiomyopathy_ceterm)),
    arrhythmia_ceterm       = first(na.omit(arrhythmia_ceterm)),
    ischaemia_ceterm        = first(na.omit(ischaemia_ceterm)),
    cardiacarrest_ceterm    = first(na.omit(cardiacarrest_ceterm)),
    bacteraemia_ceterm      = first(na.omit(bacteraemia_ceterm)),
    coagulo_ceterm          = first(na.omit(coagulo_ceterm)),
    aneamia_ceterm          = first(na.omit(aneamia_ceterm)),
    rhabdomyolsis_ceterm    = first(na.omit(rhabdomyolsis_ceterm)),
    renalinjury_ceterm      = first(na.omit(renalinjury_ceterm)),
    gastro_ceterm           = first(na.omit(gastro_ceterm)),
    pancreat_ceterm         = first(na.omit(pancreat_ceterm)),
    liverdysfunction_ceterm = first(na.omit(liverdysfunction_ceterm)),
    hyperglycemia_aeterm    = first(na.omit(hyperglycemia_aeterm)),
    hypoglycemia_ceterm     = first(na.omit(hypoglycemia_ceterm))
  ) %>%
  mutate(
    vrialpneu_ceoccur       = ifelse(vrialpneu_ceoccur != "YES" | is.na(vrialpneu_ceoccur), "", as.character("Viral_Pneumonia ")),
    bactpneu_ceoccur        = ifelse(bactpneu_ceoccur != "YES" | is.na(bactpneu_ceoccur), "", as.character(bactpneu_ceoccur)),
    ards_ceoccur            = ifelse(ards_ceoccur != "YES" | is.na(ards_ceoccur), "", as.character(ards_ceoccur)),
    cryptogenic_ceterm      = ifelse(cryptogenic_ceterm != "YES" | is.na(cryptogenic_ceterm), "", as.character("Cryptogenic_organizing_pneumonia ")),
    pneumothorax_ceterm     = ifelse(pneumothorax_ceterm != "YES" | is.na(pneumothorax_ceterm), "", as.character("Pneumothorax ")),
    pleuraleff_ceterm       = ifelse(pleuraleff_ceterm != "YES" | is.na(pleuraleff_ceterm), "", as.character("Pleural_effusion ")),
    bronchio_ceterm         = ifelse(bronchio_ceterm != "YES" | is.na(bronchio_ceterm), "", as.character(bronchio_ceterm)),
    meningitis_ceterm       = ifelse(meningitis_ceterm != "YES" | is.na(meningitis_ceterm), "", as.character("Meningitis/Encephalitis ")),
    seizure_ceterm          = ifelse(seizure_ceterm != "YES" | is.na(seizure_ceterm), "", as.character("Seizure ")),
    stroke_ceterm           = ifelse(stroke_ceterm != "YES" | is.na(stroke_ceterm), "", as.character("Stroke")),
    neuro_comp              = ifelse(neuro_comp != "1" | is.na(neuro_comp), "", as.character("Other_neurological_complication ")),
    heartfailure_ceterm     = ifelse(heartfailure_ceterm != "YES" | is.na(heartfailure_ceterm), "", as.character(heartfailure_ceterm)),
    endocarditis_aeterm     = ifelse(endocarditis_aeterm != "YES" | is.na(endocarditis_aeterm), "", as.character("Endocarditis/Myocarditis_Pericarditis ")),
    myocarditis_ceterm      = ifelse(myocarditis_ceterm != "YES" | is.na(myocarditis_ceterm), "", "Myocarditis/Pericarditis "),
    cardiomyopathy_ceterm   = ifelse(cardiomyopathy_ceterm != "YES" | is.na(cardiomyopathy_ceterm), "", "Cardiomyopathy "),
    arrhythmia_ceterm       = ifelse(arrhythmia_ceterm != "YES" | is.na(arrhythmia_ceterm), "", as.character("Cardiac_arrhythmia ")),
    ischaemia_ceterm        = ifelse(ischaemia_ceterm != "YES" | is.na(ischaemia_ceterm), "", as.character("Cardiac_ischaemia ")),
    cardiacarrest_ceterm    = ifelse(cardiacarrest_ceterm != "YES" | is.na(cardiacarrest_ceterm), "", as.character("Cardiac_arrest ")),
    bacteraemia_ceterm      = ifelse(bacteraemia_ceterm  != "YES" | is.na(bacteraemia_ceterm ), "", as.character("Bacteraemia ")),
    coagulo_ceterm          = ifelse(coagulo_ceterm != "YES" | is.na(coagulo_ceterm), "", as.character("Coagulation_disorder ")),
    aneamia_ceterm          = ifelse(aneamia_ceterm != "YES" | is.na(aneamia_ceterm), "", as.character("Aneamia ")),
    rhabdomyolsis_ceterm    = ifelse(rhabdomyolsis_ceterm != "YES" | is.na(rhabdomyolsis_ceterm), "", as.character("Rhabdomyolsis/Myositis ")),
    renalinjury_ceterm      = ifelse(renalinjury_ceterm != "YES" | is.na(renalinjury_ceterm), "", as.character(renalinjury_ceterm)),
    gastro_ceterm           = ifelse(gastro_ceterm != "YES" | is.na(gastro_ceterm), "", as.character("Gastrointestinal_hemorrhage ")),
    pancreat_ceterm         = ifelse(pancreat_ceterm != "YES" | is.na(pancreat_ceterm), "", as.character("Pancreatitis ")),
    liverdysfunction_ceterm = ifelse(liverdysfunction_ceterm != "YES" | is.na(liverdysfunction_ceterm), "", as.character("Liver_dysfunction ")),
    hyperglycemia_aeterm    = ifelse(hyperglycemia_aeterm != "YES" | is.na(hyperglycemia_aeterm), "", as.character("Hyperglycemia ")),
    hypoglycemia_ceterm     = ifelse(hypoglycemia_ceterm != "YES" | is.na(hypoglycemia_ceterm), "", as.character("hypoglycemia "))
  ) %>%
  mutate(
    other_flag = paste0(vrialpneu_ceoccur, cryptogenic_ceterm, pneumothorax_ceterm, 
                        pleuraleff_ceterm, meningitis_ceterm, seizure_ceterm,
                        stroke_ceterm, neuro_comp, endocarditis_aeterm, 
                        myocarditis_ceterm, cardiomyopathy_ceterm, arrhythmia_ceterm,
                        ischaemia_ceterm, cardiacarrest_ceterm, bacteraemia_ceterm,
                        coagulo_ceterm, aneamia_ceterm, rhabdomyolsis_ceterm,
                        gastro_ceterm, pancreat_ceterm, liverdysfunction_ceterm,
                        hyperglycemia_aeterm, hypoglycemia_ceterm),
    other_flag2 = ifelse(other_flag != "", "YES", ""),
    other_flag2 = ifelse(ards_ceoccur == "YES" | bronchio_ceterm == "YES" |
                           bactpneu_ceoccur == "YES" | renalinjury_ceterm == "YES" |
                           heartfailure_ceterm == "YES", "", as.character(other_flag2))
  ) %>%
  mutate(
    complic = ifelse(ards_ceoccur == "YES", 1, 
                     ifelse(bronchio_ceterm == "YES", 2, 
                            ifelse(bactpneu_ceoccur == "YES", 5, 
                                   ifelse(renalinjury_ceterm == "YES", 8, 
                                          ifelse(heartfailure_ceterm == "YES", 9,
                                                 ifelse(other_flag2 == "YES", 14, 0)))))),
    complic_sp = ifelse(complic == 14, other_flag, "")
  ) %>%
  select(subjid, nhs_chi, complic, complic_sp)



#########################################################
################ WP4 - LAB TESTS ########################
#########################################################

wp4_labtests <- cocin %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Influenza
    mbperf       = first(na.omit(mbperf)),
    influ_mbyn   = first(na.omit(influ_mbyn)),
    corna_mbcat  = first(na.omit(corna_mbcat)),
    corna_mbcaty = first(na.omit(corna_mbcaty)),
    rsv_mbcat    = first(na.omit(rsv_mbcat)),
    adeno_mbcat  = first(na.omit(adeno_mbcat)),
    other_mbyn   = first(na.omit(other_mbyn))
  ) %>%
  mutate(
    lab_fluany = ifelse(mbperf == "YES" & influ_mbyn == "YES - Confirmed", 1,
                        ifelse(mbperf == "YES" & influ_mbyn == "NO", 0, 
                               ifelse(mbperf == "NO", 2, 8))),
    lab_fluany = ifelse(is.na(lab_fluany), 8, lab_fluany),
    lab_mers   = ifelse(mbperf == "YES" & corna_mbcaty == "MERS-CoV" & corna_mbcat == "YES - Confirmed", 1,
                        ifelse(mbperf == "YES" & corna_mbcaty != "MERS-CoV", 0,
                               ifelse(mbperf == "NO", 2, 8))),
    lab_mers   = ifelse(is.na(lab_mers), 8, lab_mers),
    lab_othcov = ifelse(mbperf == "YES" & corna_mbcaty == "Other CoV" & corna_mbcat == "YES - Confirmed", 1,
                        ifelse(mbperf == "YES" & corna_mbcaty != "Other CoV", 0,
                               ifelse(mbperf == "NO", 2, 8))),
    lab_othcov = ifelse(is.na(lab_othcov), 8, lab_othcov),
    resp_virus = ifelse(rsv_mbcat == "YES - Confirmed", 1,
                        ifelse(adeno_mbcat == "YES - Confirmed" | 
                                 other_mbyn == "YES - Confirmed", 3, 8)),
    resp_virus = ifelse(is.na(resp_virus), 8, resp_virus)
  ) %>%
  select(subjid, nhs_chi, lab_fluany, lab_mers, lab_othcov, resp_virus)


#########################################################
############# WP4 - HOSPITAL TESTS ######################
#########################################################

wp4_hosptests <- cocin %>%
  mutate(
    daily_bun_lborres    = ifelse(daily_bun_lborresu == "mmol/L", daily_bun_lborres,
                                  ifelse(daily_bun_lborresu == "mg/dL", daily_bun_lborres * 0.357, NA)),
    daily_crp_lborres    = ifelse(daily_crp_lborresu == "1", daily_crp_lborres,
                                  ifelse(daily_crp_lborresu == "2", daily_crp_lborres * 10, 
                                         ifelse(daily_crp_lborresu == "3", daily_crp_lborres, NA))),
    daily_ferr_lborres   = ifelse(daily_ferr_lborresu == "1", daily_ferr_lborres,
                                  ifelse(daily_ferr_lborresu == "2", daily_ferr_lborres, NA)),
    daily_hba1c_lborres  = ifelse(daily_hba1c_lborresu == "1", daily_hba1c_lborres,
                                  ifelse(daily_hba1c_lborresu == "2", (daily_hba1c_lborres / 10.929) + 2.15, NA)),
    daily_bil_lborres    = ifelse(daily_bil_lborresu == "µmol/L", daily_bil_lborres * 0.585,
                                  ifelse(daily_bil_lborresu == "mg/dL", daily_bil_lborres, NA)),
    daily_lymp_lborres   = ifelse(daily_lymp_lborresu == "1", daily_lymp_lborres,
                                  ifelse(daily_lymp_lborresu == "2", daily_lymp_lborres*1000, 
                                         ifelse(daily_lymp_lborresu == "3", daily_lymp_lborres*1000, NA))),
    daily_neutro_lborres = ifelse(daily_neutro_lborresu == "1", daily_neutro_lborres,
                                  ifelse(daily_neutro_lborresu == "2", daily_neutro_lborres*1000, 
                                         ifelse(daily_neutro_lborresu == "3", daily_neutro_lborres*1000, NA))),
    daily_plt_lborres    = daily_plt_lborres*1000
  ) %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Hospital Tests at admission
    alt                   = first(na.omit(daily_alt_lborres)),
    ast                   = first(na.omit(daily_ast_lborres)),
    bloodurea             = first(na.omit(daily_bun_lborres)),
    bpdia                 = first(na.omit(admission_diabp_vsorres)),
    bpsys                 = first(na.omit(sysbp_vsorres)),
    cpk                   = first(na.omit(daily_cpk_lbyn_2)),
    crp                   = first(na.omit(daily_crp_lborres)),
    ferritin              = first(na.omit(daily_ferr_lborres)),
    hba1c                 = first(na.omit(daily_hba1c_lborres)),
    heartrate             = first(na.omit(hr_vsorres)),
    ldh                   = first(na.omit(daily_ldh_lborres)),
    lft_bili              = first(na.omit(daily_bil_lborres)),
    lymphoc               = first(na.omit(daily_lymp_lborres)),
    neutro                = first(na.omit(daily_neutro_lborres)),
    plat                  = first(na.omit(daily_plt_lborres)),
    resprate              = first(na.omit(rr_vsorres))
  ) %>%
  select(subjid, nhs_chi, alt, ast, bloodurea, bpdia, bpsys, cpk, crp, ferritin,
         hba1c, heartrate, ldh, lft_bili, lymphoc, neutro, plat, resprate)



#########################################################
######## WP4 - HOSPITAL TESTS (MOST PATHOLOGIC) #########
#########################################################

wp4_hosptests_mp <- cocin %>%
  mutate(
    daily_bun_lborres    = ifelse(daily_bun_lborresu == "mmol/L", daily_bun_lborres,
                                  ifelse(daily_bun_lborresu == "mg/dL", daily_bun_lborres * 0.357, NA)),
    daily_crp_lborres    = ifelse(daily_crp_lborresu == "1", daily_crp_lborres,
                                  ifelse(daily_crp_lborresu == "2", daily_crp_lborres * 10, 
                                         ifelse(daily_crp_lborresu == "3", daily_crp_lborres, NA))),
    daily_ferr_lborres   = ifelse(daily_ferr_lborresu == "1", daily_ferr_lborres,
                                  ifelse(daily_ferr_lborresu == "2", daily_ferr_lborres, NA)),
    daily_hba1c_lborres  = ifelse(daily_hba1c_lborresu == "1", daily_hba1c_lborres,
                                  ifelse(daily_hba1c_lborresu == "2", (daily_hba1c_lborres / 10.929) + 2.15, NA)),
    daily_bil_lborres    = ifelse(daily_bil_lborresu == "µmol/L", daily_bil_lborres * 0.585,
                                  ifelse(daily_bil_lborresu == "mg/dL", daily_bil_lborres, NA)),
    daily_lymp_lborres   = ifelse(daily_lymp_lborresu == "1", daily_lymp_lborres,
                                  ifelse(daily_lymp_lborresu == "2", daily_lymp_lborres*1000, 
                                         ifelse(daily_lymp_lborresu == "3", daily_lymp_lborres*1000, NA))),
    daily_neutro_lborres = ifelse(daily_neutro_lborresu == "1", daily_neutro_lborres,
                                  ifelse(daily_neutro_lborresu == "2", daily_neutro_lborres*1000, 
                                         ifelse(daily_neutro_lborresu == "3", daily_neutro_lborres*1000, NA))),
    daily_plt_lborres    = daily_plt_lborres*1000
  ) %>%
  group_by(subjid) %>%
  summarise(
    # Include CHI for linkage
    nhs_chi = first(na.omit(nhs_chi)),
    # Hospital Tests at admission
    alt_mp                   = max(daily_alt_lborres, na.rm=T),
    ast_mp                   = max(daily_ast_lborres, na.rm=T),
    bloodurea_mp             = max(daily_bun_lborres, na.rm=T),
    bpdia_mp                 = max(admission_diabp_vsorres, na.rm=T),
    bpsys_mp                 = min(sysbp_vsorres, na.rm=T),
    cpk_mp                   = max(daily_cpk_lbyn_2, na.rm=T),
    crp_mp                   = max(daily_crp_lborres, na.rm=T),
    ferritin_mp              = max(daily_ferr_lborres, na.rm=T),
    hba1c_mp                 = max(daily_hba1c_lborres, na.rm=T),
    heartrate_mp             = min(hr_vsorres, na.rm=T),
    ldh_mp                   = max(daily_ldh_lborres, na.rm=T),
    lft_bili_mp              = max(daily_bil_lborres, na.rm=T),
    lymphoc_mp               = min(daily_lymp_lborres, na.rm=T),
    neutro_mp                = min(daily_neutro_lborres, na.rm=T),
    plat_mp                  = min(daily_plt_lborres, na.rm=T),
    resprate_mp              = min(rr_vsorres, na.rm=T)
  ) %>%
  select(subjid, nhs_chi, alt_mp, ast_mp, bloodurea_mp, bpdia_mp, bpsys_mp, cpk_mp, 
         crp_mp, ferritin_mp, hba1c_mp, heartrate_mp, ldh_mp, lft_bili_mp, lymphoc_mp, 
         neutro_mp, plat_mp, resprate_mp)

wp4_hosptests_mp[wp4_hosptests_mp == -Inf] <- NA
wp4_hosptests_mp[wp4_hosptests_mp == Inf]  <- NA


#########################################################
################ FINAL ISARIC DATASET ###################
#########################################################

## Merge all data to create final data set
cocin_admissions <- list(
  studyidentifiers, hospitalward, patientchar,
  caseseverity_ind, caseseverity_symptoms, rkclosecontact,
  rkexamlab, underlyingcc, rkhospmedc,
  wp4_antibiotics, wp4_antiviral, wp4_complic, wp4_cortico, wp4_frailty,
  wp4_hosptests, wp4_hosptests_mp, wp4_il6, wp4_labtests
) %>%
  reduce(left_join, by = c("subjid", "nhs_chi")) %>%
  rename(chi_number = nhs_chi)


# COCIN with CHI - remove duplicate admissions
cocin_chi <- cocin_admissions %>%
  filter(!is.na(chi_number)) %>%
  group_by(chi_number, admitdate_cocin) %>%
  summarise_all(list(~ first(na.omit(.)))) %>%
  ungroup()

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
  cocin_chi, cocin_nonchi,
  wp4_antibiotics, wp4_antiviral, wp4_complic, wp4_cortico, wp4_frailty,
  wp4_hosptests, wp4_hosptests_mp, wp4_il6, wp4_labtests
)
