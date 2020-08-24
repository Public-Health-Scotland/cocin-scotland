source("extract-data/00_setup-environment.R")

# Recode into IMOVE format
#######################################
#### ID and Basic Demographic Info ####
#######################################

demographics <- data %>%
  
  mutate(
    # idcountry - country code
    idcountry = "SC",
    # hosp_id2 - unique number for each patient - use CHI number
    hosp_id2 = chi_number,
    # hospitalcode - hospital site
    hospitalcode = hospital_of_treatment_code,
    # consent - this is NA
    consent = NA,
    # sex - take from RAPID  
    sex = ifelse(sex == "Female", 0,
                 ifelse(sex == "Male", 1, 
                        ifelse(sex == "Unknown", 3, 8))),
    sex = ifelse(is.na(sex), 8, sex),
    # age_y - age in years
    age_y = as.integer(ifelse(age > 1, age, NA)),
    # age_m - age in months for those under two, can calculate from RAPID DOB
    age_m = as.integer(ifelse(age < 2, 
                   interval(dob, adm_date) %/% months(1), NA)),
    # height - N/A
    height = NA,
    # weight - N/A
    weight = NA
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, idcountry, hosp_id2, age_y, age_m,  sex, height, weight)%>%
  
  mutate_at(vars(sex), .funs = factor)

#################################
###### Hospital Information #####
#################################

hospinfo <- data %>%
  
  # If record marked as a reinfection, treat as a new admission rather than a readmission
  mutate(readmission = ifelse(reinfection == 1, 0, readmission)) %>%
  group_by(chi_number) %>%
  # Get the max number of readmissions per CHI
  mutate(no_of_readmissions = max(readmission)) %>%
  ungroup() %>%
  
  mutate(
    
    # admitdate - this comes from RAPID data
    admitdate = adm_date,
    # dischargedate - this comes from RAPID
    dischargedate = dis_date,
    # hospitalward - N/A
    hospitalward = NA,
    # hospitalward_oth - N/A
    hospitalward_oth = NA,
    # icu - from SICSAG
    icu = ifelse(icu == 'No', 0,
                 ifelse(icu == 'Yes', 1, 8)),
    # icuadmitdate - from SICSAG
    icuadmitdate = icuadmitdate,
    # icudisdate - from SICSAG
    icudisdate = icudisdate,
    # los_hosp
    los_hosp = as.integer(dischargedate - admitdate),
    # los_icu - from SICSAG
    los_icu = as.integer(los_icu),
    # multiple_hosp
    multiple_hosp = ifelse(no_of_readmissions >= 1, 1, 0),
    multiple_hosp = ifelse(is.na(multiple_hosp), 0, multiple_hosp),
    # multiple_episode
    multiple_episode = no_of_readmissions,
    multiple_episode = ifelse(is.na(multiple_episode), 0, multiple_episode),
    multiple_episode = as.integer(multiple_episode),
    # prevhosp  
    prevhosp = ifelse(prevhosp == 'No', 0,
                      ifelse(prevhosp == 'Yes', 1, 8))
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, admitdate, dischargedate, hospitalward, hospitalward_oth,
         icu, icuadmitdate, icudisdate, los_hosp, los_icu, multiple_hosp, 
         multiple_episode, prevhosp) %>%
  
  mutate_at(vars(hospitalward, icu, multiple_hosp, prevhosp), .funs = factor)


###############################
######### Patient Info ########
###############################

patientinfo <- data %>%
  
  mutate(
    # hcw - from COCIN only
    hcw = ifelse(hcw == "NO", 0,
                 ifelse(hcw == "YES", 1, 8)),
    hcw = ifelse(is.na(hcw), 8, hcw),
    # smoking - from COCIN only
    smoking = ifelse(smoking == "Never Smoked", 0,
                     ifelse(smoking == "Former Smoker", 1,
                            ifelse(smoking == "Yes", 2, 8))),
    smoking = ifelse(is.na(smoking), 8, smoking),
    # pregnant - from COCIN only
    pregnant = ifelse(pregnant == "NO", 0,
                      ifelse(pregnant == "YES", 1, 8)),
    pregnant = ifelse(is.na(pregnant), 8, pregnant),
    # trimester - from COCIN only
    trimester = ifelse(trimester < 13, 1,
                       ifelse(trimester > 12 & trimester < 27, 2,
                              ifelse(trimester > 26, 3, 8))),
    trimester = ifelse(is.na(trimester), 8, trimester),
    # postpartum - from COCIN only
    postpartum = ifelse(postpartum == "No", 0,
                        ifelse(postpartum == "Yes", 1, 8)),
    postpartum = ifelse(is.na(postpartum), 8, postpartum),
    # postcode
    postcode  = postcode,
    # residence - This is given as it is in RAPID data, needs recoded
    residence = admitted_transfer_from_type,
    # onsetdate - Fix wrong date issue
    onsetdate = as.character(onsetdate),
    onsetdate = ifelse(onsetdate == '2010-03-26', '2020-03-26', onsetdate),
    onsetdate = as.Date(onsetdate),
    # swabdate - from RAPID
    swabdate = test_date
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, hcw, smoking, pregnant,
         trimester, postpartum, postcode, residence, onsetdate, swabdate)%>%
  
  mutate_at(vars(-chi_number, -adm_date, -postcode, -onsetdate, -swabdate), .funs = factor)
  
####################################
##### CASE SEVERITY (SYMPTOMS) #####
####################################

symptoms <- data %>%
  
  # feverishness - N/A
  mutate(feverishness = NA,
         
         # fever - only from COCIN admissions
         fever        = ifelse(fever == "No", 0,
                               ifelse(fever == "Yes", 1, 8)),
         fever        = ifelse(is.na(fever), 8, fever),
         
         # malaise - only COCIN admissions
         malaise      = ifelse(malaise == "NO", 0,
                               ifelse(malaise == "YES", 1, 8)),
         malaise      = ifelse(is.na(malaise), 8, malaise),
         
         # headache - only COCIN admissions
         headache     = ifelse(headache == "NO", 0,
                               ifelse(headache == "YES", 1, 8)),
         headache     = ifelse(is.na(headache), 8, headache),
         
         # myalgia - only COCIN admissions
         myalgia      = ifelse(myalgia == "NO", 0,
                               ifelse(myalgia == "YES", 1, 8)),
         myalgia      = ifelse(is.na(myalgia), 8, myalgia),
         
         # sorethroat - only COCIN admissions
         sorethroat   = ifelse(sorethroat == "NO", 0,
                               ifelse(sorethroat == "YES", 1, 8)),
         sorethroat   = ifelse(is.na(sorethroat), 8, sorethroat),
         
         # cough - only COCIN admissions
         cough        = ifelse(cough == "NO", 0,
                               ifelse(cough == "YES", 1, 8)),
         cough        = ifelse(is.na(cough), 8, cough),
         
         # suddenonset - N/A
         suddenonset  = NA,
         
         # sob - only COCIN admissions
         sob          = ifelse(sob == "NO", 0,
                               ifelse(sob == "YES", 1, 8)),
         sob          = ifelse(is.na(sob), 8, sob),
         
         # general_deter - N/A
         general_deter     = NA,
         
         # vomit - only COCIN admissions (COCIN is both vomit/nausea)
         vomit        = ifelse(vomit == "NO", 0,
                               ifelse(vomit == "YES", 1, 8)),
         vomit        = ifelse(is.na(vomit), 8, vomit),
         
         # diarr - only COCIN admissions
         diarr        = ifelse(diarr == "NO", 0,
                               ifelse(diarr == "YES", 1, 8)),
         diarr        = ifelse(is.na(diarr), 8, diarr),
         
         # abdopain - only COCIN admissions
         abdopain     = ifelse(abdopain == "NO", 0,
                               ifelse(abdopain == "YES", 1, 8)),
         abdopain     = ifelse(is.na(abdopain), 8, abdopain),
         
         # aguesia - only COCIN admissions
         ageusia      = ifelse(ageusia == "2", 0,
                               ifelse(ageusia == "1", 1, 8)),
         ageusia      = ifelse(is.na(ageusia), 8, ageusia),
         
         # anosmia - only COCIN admissions
         anosmia      = ifelse(anosmia == "2", 0,
                               ifelse(anosmia == "1", 1, 8)),
         anosmia      = ifelse(is.na(anosmia), 8, anosmia),
         
         # chills - N/A
         chills     = NA,
         
         # tach - N/A
         tach     = NA,
         
         # general_deter - N/A
         coryza     = NA,
         
         # confusion - only COCIN admissions
         confusion   = ifelse(confusion == "NO", 0,
                              ifelse(confusion == "YES", 1, 8)),
         confusion   = ifelse(is.na(confusion), 8, confusion),
         
         # dizzy - N/A
         dizzy    = NA,
         
         # chest - only COCIN admissions
         chest    = ifelse(chest == "2", 0,
                           ifelse(chest == "1", 1, 8)),
         chest    = ifelse(is.na(chest), 8, chest),
         
         # palp - N/A
         palp    = NA,
         
         # nausea - N/A (nausea is included within vomit)
         nausea   = NA,
         
         # conjunct - only COCIN admissions
         conjunct = ifelse(conjunct == "NO", 0,
                           ifelse(conjunct == "YES", 1, 8)),
         conjunct    = ifelse(is.na(conjunct), 8, conjunct),
         
         # dermato - only COCIN admissions
         dermato  = ifelse(dermato == "NO", 0,
                           ifelse(dermato == "YES", 1, 8)),
         dermato    = ifelse(is.na(dermato), 8, dermato)
         
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, abdopain, ageusia, anosmia, chest, chills, confusion, 
         conjunct, coryza, cough, dermato, diarr, dizzy, fever, feverishness, general_deter, 
         headache, malaise, myalgia, nausea, palp, sob, sorethroat, suddenonset, 
         tach, vomit) %>%
  
  mutate_at(vars(-chi_number, -adm_date), .funs = factor)


##########################################
###### HOSPITAL TESTS/EXAMS ##############
##########################################

hospitaltests <- data %>%
  
  # ct_us_ecg - can't provide this
  mutate(
    abo = ifelse(bloodgroup == '1', '1',
                 ifelse(bloodgroup == '2', '2',
                        ifelse(bloodgroup == '4', '0',
                               ifelse(bloodgroup %in% c('3','9'), '8', '8')))),
    abo = ifelse(is.na(abo), '8', abo),
    
    bmi = NA,
    
    # ct_res - can't provide this
    ct_res = NA,
    
    ct_res_sp = NA,
    
    ct_us_ecg = NA,
    
    # cxr - can't provide this
    cxr = NA,
    
    cxroth_sp = NA,
    
    # cxr_sp - can't provide this
    examoth_sp = NA,
    
    # ecg_qt - can't provide this
    ecg_qt = NA,
    
    ox_nasal = NA,
    
    # oxsat - only COCIN admissions
    oxsat = as.integer(oxsat)
    
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, abo, bmi, ct_res, ct_res_sp,  ct_us_ecg, cxr, cxroth_sp, 
         ecg_qt, examoth_sp, ox_nasal, oxsat) %>%
  
  mutate_at(vars(abo, ct_res, ct_us_ecg, cxr, ecg_qt, ox_nasal), .funs = factor)


##########################################
##### IN-HOSPITAL MEDICATIONS ############
##########################################  

medications <- data %>%
  
  mutate(
    
    # nebu - N/A
    nebu = NA,
    
    # prone - COCIN only
    prone = ifelse(prone == "NO", 0,
                   ifelse(prone == "YES", 1, 8)),
    prone   = ifelse(is.na(prone), 8, prone),
    
    trialdrugs = NA,
    
    study_convpl = ifelse(plasma == '1', 2,
                          ifelse(plasma == '2', 1,
                                 ifelse(plasma == '3', 8, 8))),
    study_convpl = ifelse(is.na(study_convpl), 8, study_convpl),
    
    study_gm_csf = NA,
    
    study_oth = NA,
    
    study_oth_sp = NA,
    
    vent_new = ifelse(vent == "None", 0,
                      ifelse(vent == "ECMO", 1,
                             ifelse(vent == "Oxygen (high-flow)", 2,
                                    ifelse(vent == "NonInvasive", 3,
                                           ifelse(vent == "Invasive", 4, 8))))),
    
    vent_new = ifelse(is.na(vent_new), 8, vent_new),
    
    vent_sp = NA,
    
    vent_type = NA,
    
    venttype_sp = NA) %>%
  
  mutate(vent = vent_new) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, nebu, prone, trialdrugs, study_convpl, study_oth, 
         study_oth_sp, vent, vent_sp, vent_type, venttype_sp) %>%
  
  mutate_at(vars(-chi_number, -adm_date), .funs = factor)

##########################################
######## CONTACT INFORMATION #############
##########################################

contactinfo <- data %>%
  
  # closecont - can't provide this
  mutate(closecont = ifelse(contact == "NO", 0,
                            ifelse(contact == "YES", 1, 
                                   ifelse(contact == "Unknown", 8, NA))),
         closecont = ifelse(is.na(closecont), 8, closecont),
         
         closecont_type = NA,
         
         # closecont_sp - can't provide this
         closecont_sp = NA) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, closecont, closecont_type, closecont_sp) %>%
  
  mutate_at(vars(-chi_number, -adm_date), .funs = factor)


##########################################
####### CO-MORBID CONDITIONS #############
##########################################

conditions <- data %>%
  
  # anaemia - only COCIN 
  mutate(anaemia = ifelse(anaemia == "NO", 0,
                          ifelse(anaemia == "YES", 1, 8)),
         anaemia   = ifelse(is.na(anaemia), 8, anaemia),
         
         # asplenia - N/A
         asplenia = NA,
         
         # asthma - only COCIN 
         asthma = ifelse(asthma == "NO", 0,
                         ifelse(asthma == "YES", 1, 8)),
         asthma   = ifelse(is.na(asthma), 8, asthma),
         
         # cancer - only COCIN 
         cancer = ifelse(cancer == "NO", 0,
                         ifelse(cancer == "YES", 1, 8)),
         cancer   = ifelse(is.na(cancer), 8, cancer),
         
         # hypert - only COCIN 
         hypert = ifelse(hypert == "NO", 0,
                         ifelse(hypert == "YES", 1, 8)),
         hypert   = ifelse(is.na(hypert), 8, hypert),
         
         # dement - only COCIN 
         dement = ifelse(dementia == "NO", 0,
                         ifelse(dementia == "YES", 1, 8)),
         dement   = ifelse(is.na(dement), 8, dement),
         
         # diabetes - only COCIN 
         diabetes = ifelse(diabetes == "NO", 0,
                           ifelse(diabetes == "YES", 1, 8)),
         diabetes   = ifelse(is.na(diabetes), 8, diabetes),
         
         # heartdis - only COCIN 
         heartdis = ifelse(heartdis == "NO", 0,
                           ifelse(heartdis == "YES", 1, 8)),
         heartdis   = ifelse(is.na(heartdis), 8, heartdis),
         
         # immuno - only COCIN 
         immuno = ifelse(immuno == "NO", 0,
                         ifelse(immuno == "YES", 1, 8)),
         immuno   = ifelse(is.na(immuno), 8, immuno),
         
         # liverdis - only COCIN 
         liverdis = ifelse(liverdis == "NO", 0,
                           ifelse(liverdis == "YES", 1, 8)),
         liverdis   = ifelse(is.na(liverdis), 8, liverdis),
         
         # lungdis - N/A
         lungdis = NA,
         
         # neuromusc - N/A
         neuromusc = NA,
         
         # obese - only COCIN 
         obese = ifelse(obese == "NO", 0,
                        ifelse(obese == "YES", 1, 8)),
         obese   = ifelse(is.na(obese), 8, obese),
         
         # rendis - only COCIN 
         rendis = ifelse(rendis == "NO", 0,
                         ifelse(rendis == "YES", 1, 8)),
         rendis   = ifelse(is.na(rendis), 8, rendis),
         
         # rheumat - only COCIN 
         rheumat = ifelse(rheumat == "NO", 0,
                          ifelse(rheumat == "YES", 1, 8)),
         rheumat   = ifelse(is.na(rheumat), 8, rheumat),
         
         # Stroke - only COCIN 
         stroke = ifelse(stroke == "NO", 0,
                         ifelse(stroke == "YES", 1, 8)),
         stroke   = ifelse(is.na(stroke), 8, stroke),
         
         # tuberc - N/A
         tuberc = NA) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, anaemia, asplenia, asthma, cancer, dement, diabetes,
         heartdis,  hypert, immuno, liverdis, lungdis, neuromusc, obese,
         rendis, rheumat, stroke, tuberc) %>%
  
  mutate_at(vars(-chi_number, -adm_date), .funs = factor)



####################################
######## OUTCOMES AND DEATH ########
####################################

outcomes <- data %>%
  
  # outcome
  mutate(
    
    # outcome_pre = ifelse(discharge_type %in% c(40,41,42,43), 1,
    #                      ifelse(discharge_type %in% c(10,11,18,19,20,21,22,23,28,29), 2,
    #                             ifelse(discharge_type %in% c(12,13), 3, 8))),
    # outcome_pre = ifelse(is.na(outcome_pre), 8, outcome_pre),
    
    death_outcome = ifelse(!is.na(date_of_death) & date_of_death <= (dis_date + 1), 1, 0),
    
    # outcome = ifelse(outcome_pre == 1 | death_outcome == 1, 1, outcome_pre),
    # outcome = ifelse(is.na(outcome), 8, outcome), 
    
    outcome = ifelse(death_outcome == 1, 1, 2),
    outcome  = ifelse(is.na(outcome), 8, outcome), 
    
    deathdate = ifelse(outcome == '1', as.character(date_of_death), NA),
    deathdate = as.Date(deathdate),
    
    deathcause = ifelse(covid_death == "Yes", 1, 
                        ifelse(covid_death == "No", 2, 
                               ifelse(covid_death == "Unknown", 8, NA))),
    
    healthcare_contact = NA
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, outcome, deathcause, deathdate, healthcare_contact,
         date_of_death, dis_date) %>%
  
  mutate_at(vars(-chi_number, -adm_date, -deathdate), .funs = factor)


####################################
###### LAB TESTS AND RESULTS #######
####################################


labtests <- data %>%
  
  mutate(
    
    # lab_covtest - marked as Yes for everyone with a test date       
    lab_covtest = ifelse(!is.na(test_date), 1, 0),
    
    # lab_covtesttype - all PCR for those with test
    lab_covtesttype = ifelse(lab_covtest == 1, 1, NA),
    
    # lab_covtesttype_sp - mark as NA
    lab_covtesttype_sp = NA,
    
    # lab_covid - use result if a test was taken
    lab_covid = ifelse(lab_covtest == 1, result, NA),
    
    # covid_new - whether someone is confirmed, suspected, not etc.
    covid_new = ifelse(lab_covid == 1 | covid == "Lab comfirmed", 1,
                       ifelse(lab_covid != 1 & covid == "Clinical suspected", 4,
                              ifelse(lab_covid != 1 & covid == "Other coronavirus", 
                                     3, NA))),
    
    covid_new = ifelse(is.na(covid_new), 8, covid_new),
    
    # seq - can't provide this
    seq = NA,
    
    # genetic_group = can't provide this
    genetic_group = NA) %>%
  
  # rename variable
  mutate(covid = covid_new) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, lab_covid, lab_covtest, lab_covtesttype, lab_covtesttype_sp, 
         covid, genetic_group, seq) %>%
  
  mutate_at(vars(-chi_number, -adm_date), .funs = factor)


##########################################
############## FULL DATA #################
##########################################

# Combine all datasets
IMOVE_data <- list(demographics, hospinfo, patientinfo, symptoms, hospitaltests,
                   medications, contactinfo, conditions, outcomes, labtests) %>%
  reduce(merge, by = c("chi_number","adm_date")) %>%
  #remove adm_date variable as this is for joining only
  select(-adm_date, -chi_number)

# Remove datasets no longer required
rm(demographics, hospinfo, patientinfo, symptoms, hospitaltests,
   medications, contactinfo, conditions, outcomes, labtests)
