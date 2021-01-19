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
    # hospitalcode - hospital site
    hospitalcode = hospital_of_treatment_code,
    # consent - this is NA
    consent = NA,
    # consent_sp - NA
    consent_sp = NA,
    # sex - take from RAPID
    sex = case_when(
      sex == "Female" ~ 0L,
      sex == "Male" ~ 1L,
      sex == "Unknown" ~ 3L,
      TRUE ~ NA_integer_
    ),
    # age_y - age in years
    age_y = if_else(age >= 2, age, NA_integer_),
    # age_m - age in months for those under two, can calculate from RAPID DOB
    age_m = as.integer(if_else(age < 2,
                               as.integer(time_length(dob %--% adm_date, "months")),
                               NA_integer_
    )),
    # height - N/A
    height = NA,
    # weight - N/A
    weight = NA
  ) %>%
  # select only required variables for this section
  select(chi_number, adm_date, idcountry, hosp_id2, consent, consent_sp, 
         hospitalcode, age_y, age_m, sex, height, weight) %>%
  mutate(sex = factor(sex))

#################################
###### Hospital Information #####
#################################

hospinfo <- data %>%
  # If record marked as a reinfection, treat as a new admission rather than a readmission
  mutate(readmission = if_else(reinfection == 1, 0L, readmission)) %>%
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
    icu = case_when(
      icu == "No" ~ 0L,
      icu == "Yes" ~ 1L,
      TRUE ~ 8L
    ),
    # icuadmitdate - from SICSAG
    icuadmitdate = icuadmitdate,
    # icudisdate - from SICSAG
    icudisdate = icudisdate,
    # los_hosp
    los_hosp = los,
    # los_icu - from SICSAG
    los_icu = as.integer(los_icu),
    # multiple_hosp
    multiple_hosp = if_else(no_of_readmissions >= 1, 1, 0),
    multiple_hosp = if_else(is.na(multiple_hosp), 0, multiple_hosp),
    # multiple_episode
    multiple_episode = no_of_readmissions,
    multiple_episode = if_else(is.na(multiple_episode), 0L, multiple_episode),
    multiple_episode = as.integer(multiple_episode),
    # prevhosp
    prevhosp = case_when(
      prevhosp == "No" ~ 0L,
      prevhosp == "Yes" ~ 1L,
      TRUE ~ 8L
    )
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, admitdate, dischargedate, hospitalward, hospitalward_oth,
    icu, icuadmitdate, icudisdate, los_hosp, los_icu, 
    multiple_episode, multiple_hosp, prevhosp
  ) %>%
  mutate_at(vars(hospitalward, icu, multiple_hosp, prevhosp), list(factor))


###############################
######### Patient Info ########
###############################

patientinfo <- data %>%
  mutate(
    # hcw - from RAPID (via ECOSS) and COCIN
    hcw = case_when(
      hcw == 1 | hcw_cocin == "YES" ~ 1L,
      hcw == 0 | hcw_cocin == "NO" ~ 0L,
      TRUE ~ 8L
    ),
    # smoking - from COCIN only
    smoking = ifelse(smoking == "Never Smoked", 0,
                     ifelse(smoking == "Former Smoker", 1,
                            ifelse(smoking == "Yes", 2, 8)
                     )
    ),
    smoking = ifelse(is.na(smoking), 8, smoking),
    # pregnant - from COCIN only
    pregnant = ifelse(pregnant == "NO", 0,
                      ifelse(pregnant == "YES", 1, 8)
    ),
    pregnant = ifelse(is.na(pregnant), 8, pregnant),
    # trimester - from COCIN only
    trimester = ifelse(trimester < 13, 1,
                       ifelse(trimester > 12 & trimester < 27, 2,
                              ifelse(trimester > 26, 3, 8)
                       )
    ),
    trimester = ifelse(is.na(trimester), 8, trimester),
    # postpartum - from COCIN only
    postpartum = ifelse(postpartum == "No", 0,
                        ifelse(postpartum == "Yes", 1, 8)
    ),
    postpartum = ifelse(is.na(postpartum), 8, postpartum),
    # postcode
    postcode = postcode,
    # residence - This is given as it is in RAPID data, needs recoded
    residence = NA,
    # onsetdate - Fix wrong date issue (sinlge record)
    onsetdate = if_else(onsetdate == ymd("2010-03-26") & adm_date == ymd("2020-03-26"),
                        ymd("2020-03-26"),
                        onsetdate
    ),
    # swabdate - from RAPID
    swabdate = test_date
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, hcw, smoking, pregnant,
    trimester, postpartum, postcode, residence, onsetdate, swabdate
  ) %>%
  mutate_at(vars(-chi_number, -adm_date, -postcode, -onsetdate, -swabdate), list(factor))


####################################
#### SYMPTOMS - PRE-ADMISSION ######
####################################

wp4_symp_pre <- data %>%
  mutate(
    onsetdate_pre     = NA,
    abdopain_pre      = NA,
    ageusia_pre       = NA,
    anosmia_pre       = NA,
    chest_pre         = NA,
    chills_pre        = NA,
    confusion_pre     = NA,
    conjunct_pre      = NA,
    coryza_pre        = NA,
    cough_pre         = NA,
    dermato_pre       = NA,
    diarr_pre         = NA,
    dizzy_pre         = NA,
    fever_pre         = NA,
    feverish_pre      = NA,
    general_deter_pre = NA,
    headache_pre      = NA,
    malaise_pre       = NA,
    myalgia_pre       = NA,
    nausea_pre        = NA,
    palp_pre          = NA,
    sob_pre           = NA,
    sorethroat_pre    = NA,
    suddenonset_pre   = NA,
    tach_pre          = NA,
    vomit_pre         = NA
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, onsetdate_pre, abdopain_pre, ageusia_pre, anosmia_pre,
    chest_pre, chills_pre, confusion_pre, conjunct_pre, coryza_pre, cough_pre, 
    dermato_pre, diarr_pre, dizzy_pre, fever_pre, feverish_pre, general_deter_pre,
    headache_pre, malaise_pre, myalgia_pre, nausea_pre, palp_pre, sob_pre, 
    sorethroat_pre, suddenonset_pre, tach_pre, vomit_pre
  )



####################################
#### MEDICATIONS - PRE-ADMISSION ###
####################################

wp4_meds_pre <- data %>%
  mutate(
    ace_pre            = NA,
    antivir_pre        = NA,
    arb_pre            = NA,
    chemo_pre          = NA,
    chest_pre          = NA,
    chloroq_pre        = NA,
    corticost_pre      = NA,
    corticost_pre_type = NA,
    dmards_pre         = NA,
    gliclaz_pre        = NA,
    hydroxychloroq_pre = NA,
    il6_pre            = NA,
    il6pre_type        = NA,
    metform_pre        = NA,
    nsaid_pre          = NA,
    other1_pre_sp      = NA,
    other2_pre_sp      = NA,
    other3_pre_sp      = NA,
    psychotrop_pre     = NA,
    statin_pre         = NA,
    steroids_pre       = NA
  ) %>%
  # select only required variables for this section
  select(
    chi_number, adm_date, ace_pre, antivir_pre, arb_pre, chemo_pre, chloroq_pre,
    corticost_pre, corticost_pre_type, dmards_pre, gliclaz_pre, hydroxychloroq_pre,
    il6_pre, il6pre_type, metform_pre, nsaid_pre, other1_pre_sp, other2_pre_sp,
    other3_pre_sp, psychotrop_pre, statin_pre, steroids_pre
  )




####################################
##### CASE SEVERITY (SYMPTOMS) #####
####################################

symptoms <- data %>%
  
  mutate(
    
    # same onset - NA
    same_onsetdate = NA,
    
    # same_symptoms - NA
    same_symptoms = NA,
    
    # feverish - N/A
    feverish = NA,
    
    # fever - only from COCIN admissions
    fever = ifelse(fever == "No", 0,
                   ifelse(fever == "Yes", 1, 8)
    ),
    fever = ifelse(is.na(fever), 8, fever),
    
    # malaise - only COCIN admissions
    malaise = ifelse(malaise == "NO", 0,
                     ifelse(malaise == "YES", 1, 8)
    ),
    malaise = ifelse(is.na(malaise), 8, malaise),
    
    # headache - only COCIN admissions
    headache = ifelse(headache == "NO", 0,
                      ifelse(headache == "YES", 1, 8)
    ),
    headache = ifelse(is.na(headache), 8, headache),
    
    # myalgia - only COCIN admissions
    myalgia = ifelse(myalgia == "NO", 0,
                     ifelse(myalgia == "YES", 1, 8)
    ),
    myalgia = ifelse(is.na(myalgia), 8, myalgia),
    
    # sorethroat - only COCIN admissions
    sorethroat = ifelse(sorethroat == "NO", 0,
                        ifelse(sorethroat == "YES", 1, 8)
    ),
    sorethroat = ifelse(is.na(sorethroat), 8, sorethroat),
    
    # cough - only COCIN admissions
    cough = ifelse(cough == "NO", 0,
                   ifelse(cough == "YES", 1, 8)
    ),
    cough = ifelse(is.na(cough), 8, cough),
    
    # suddenonset - N/A
    suddenonset = NA,
    
    # sob - only COCIN admissions
    sob = ifelse(sob == "NO", 0,
                 ifelse(sob == "YES", 1, 8)
    ),
    sob = ifelse(is.na(sob), 8, sob),
    
    # general_deter - N/A
    general_deter = NA,
    
    # vomit - only COCIN admissions (COCIN is both vomit/nausea)
    vomit = ifelse(vomit == "NO", 0,
                   ifelse(vomit == "YES", 1, 8)
    ),
    vomit = ifelse(is.na(vomit), 8, vomit),
    
    # diarr - only COCIN admissions
    diarr = ifelse(diarr == "NO", 0,
                   ifelse(diarr == "YES", 1, 8)
    ),
    diarr = ifelse(is.na(diarr), 8, diarr),
    
    # abdopain - only COCIN admissions
    abdopain = ifelse(abdopain == "NO", 0,
                      ifelse(abdopain == "YES", 1, 8)
    ),
    abdopain = ifelse(is.na(abdopain), 8, abdopain),
    
    # aguesia - only COCIN admissions
    ageusia = ifelse(ageusia == "2", 0,
                     ifelse(ageusia == "1", 1, 8)
    ),
    ageusia = ifelse(is.na(ageusia), 8, ageusia),
    
    # anosmia - only COCIN admissions
    anosmia = ifelse(anosmia == "2", 0,
                     ifelse(anosmia == "1", 1, 8)
    ),
    anosmia = ifelse(is.na(anosmia), 8, anosmia),
    
    # chills - N/A
    chills = NA,
    
    # tach - N/A
    tach = NA,
    
    # general_deter - N/A
    coryza = NA,
    
    # confusion - only COCIN admissions
    confusion = ifelse(confusion == "NO", 0,
                       ifelse(confusion == "YES", 1, 8)
    ),
    confusion = ifelse(is.na(confusion), 8, confusion),
    
    # dizzy - N/A
    dizzy = NA,
    
    # chest - only COCIN admissions
    chest = ifelse(chest == "2", 0,
                   ifelse(chest == "1", 1, 8)
    ),
    chest = ifelse(is.na(chest), 8, chest),
    
    # palp - N/A
    palp = NA,
    
    # nausea - N/A (nausea is included within vomit)
    nausea = NA,
    
    # conjunct - only COCIN admissions
    conjunct = ifelse(conjunct == "NO", 0,
                      ifelse(conjunct == "YES", 1, 8)
    ),
    conjunct = ifelse(is.na(conjunct), 8, conjunct),
    
    # dermato - only COCIN admissions
    dermato = ifelse(dermato == "NO", 0,
                     ifelse(dermato == "YES", 1, 8)
    ),
    dermato = ifelse(is.na(dermato), 8, dermato)
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, abdopain, ageusia, anosmia, chest, chills, confusion,
    conjunct, coryza, cough, dermato, diarr, dizzy, fever, feverish, general_deter,
    headache, malaise, myalgia, nausea, palp, sob, sorethroat, suddenonset,
    tach, vomit
  ) %>%
  
  mutate_at(vars(-chi_number, -adm_date), list(factor))


##########################################
###### HOSPITAL TESTS/EXAMS ##############
##########################################

hospitaltests <- data %>%
  
  # ct_us_ecg - can't provide this
  mutate(
    abo = ifelse(bloodgroup == "1", "1",
                 ifelse(bloodgroup == "2", "2",
                        ifelse(bloodgroup == "4", "0",
                               ifelse(bloodgroup %in% c("3", "9"), "8", "8")
                        )
                 )
    ),
    abo = ifelse(is.na(abo), "8", abo),
    
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
    oxsat = as.integer(oxsat),
    
    alp                = NA,
    bnp                = NA,
    dimer              = NA,
    eosin              = NA,
    fibrin             = NA,
    hdl                = NA,
    ldl                = NA,
    lft_alb            = NA,
    lft_bili_conj      = NA,
    lft_ggt            = NA,
    lft_prothromb      = NA,
    lft_totprot        = NA,
    nt_probnp          = NA,
    trigly             = NA,
    trop               = NA,
    urea               = NA
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, abo, alp, alt, ast, bloodurea, bmi, bnp, bpdia, bpsys, 
    cpk, crp, ct_res, ct_res_sp, ct_us_ecg, cxr, cxroth_sp, dimer, ecg_qt, eosin, 
    examoth_sp, ferritin, fibrin, hba1c, hdl, heartrate, ldh, ldl, lft_alb,
    lft_bili, lft_bili_conj, lft_ggt, lft_prothromb, lft_totprot, lymphoc,
    neutro, nt_probnp, ox_nasal, oxsat, plat, resprate, trigly, trop, urea
  ) %>%
  
  mutate_at(vars(abo, ct_res, ct_us_ecg, cxr, ecg_qt, ox_nasal), list(factor))






##################################################
## WP4 - HOSPITAL EXAMS/TESTS - MOST PATHOLOGIC ##
##################################################

wp4_hosptest_mp <- data %>%
  mutate(
    alp_mp                = NA,
    bnp_mp                = NA,
    dimer_mp              = NA,
    eosin_mp              = NA,
    fibrin_mp             = NA,
    hdl_mp                = NA,
    ldl_mp                = NA,
    lft_alb_mp            = NA,
    lft_bili_conj_mp      = NA,
    lft_ggt_mp            = NA,
    lft_prothromb_mp      = NA,
    lft_totprot_mp        = NA,
    nt_probnp_mp          = NA,
    trigly_mp             = NA,
    trop_mp               = NA,
    urea_mp               = NA
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, alp_mp, alt_mp, ast_mp, bloodurea_mp, bnp_mp, bpdia_mp, 
    bpsys_mp, cpk_mp, crp_mp, dimer_mp, eosin_mp, ferritin_mp, fibrin_mp, hba1c_mp, 
    hdl_mp, heartrate_mp, ldh_mp, ldl_mp, lft_alb_mp, lft_bili_mp, lft_bili_conj_mp,
    lft_ggt_mp, lft_prothromb_mp, lft_totprot_mp, lymphoc_mp,
    neutro_mp, nt_probnp_mp, plat_mp, resprate_mp, trigly_mp, trop_mp, urea_mp
  ) %>%
  mutate_at(vars(-chi_number, -adm_date), list(factor))



##########################################
##### IN-HOSPITAL MEDICATIONS ############
##########################################

medications <- data %>%
  mutate(
    
    # nebu - N/A
    nebu = NA,
    
    # prone - COCIN only
    prone = ifelse(prone == "NO", 0,
                   ifelse(prone == "YES", 1, 8)
    ),
    prone = ifelse(is.na(prone), 8, prone),
    
    trialdrugs = NA,
    
    study_convpl = ifelse(plasma == "1", 2,
                          ifelse(plasma == "2", 1,
                                 ifelse(plasma == "3", 8, 8)
                          )
    ),
    study_convpl = ifelse(is.na(study_convpl), 8, study_convpl),
    
    study_gm_csf = NA,
    
    study_oth = NA,
    
    study_oth_sp = NA,
    
    vent_new = ifelse(vent == "None", 0,
                      ifelse(vent == "ECMO", 1,
                             ifelse(vent == "Oxygen (high-flow)", 2,
                                    ifelse(vent == "NonInvasive", 3,
                                           ifelse(vent == "Invasive", 4, 8)
                                    )
                             )
                      )
    ),
    
    vent_new = ifelse(is.na(vent_new), 8, vent_new),
    
    vent_sp = NA,
    
    vent_type = NA,
    
    venttype_sp = NA,
    
    antibiot              = ifelse(antibiot == "No", "0",
                                   ifelse(antibiot == "Yes", "1",
                                          ifelse(antibiot == "N/A", "8", NA))),
    antivir_med           = ifelse(antivir_med == "No", "0",
                                   ifelse(antivir_med == "Yes", "1",
                                          ifelse(antivir_med == "N/A", "8", NA))),
    antivir_type          = NA,
    chlor                 = NA,
    cortico               = ifelse(cortico == "No", "0",
                                   ifelse(cortico == "Yes", "1",
                                          ifelse(cortico == "N/A", "8", NA))),
    corticost_type        = ifelse(corticost_type == "Oral", "1",
                                   ifelse(corticost_type == "Intravenous", "2",
                                          ifelse(corticost_type == "Inhaled", "3", NA))),
    hydroxychl            = NA,
    il6_type              = ifelse(il6_type == "1", "Tocilizumab",
                                   ifelse(il6_type == "2", "Anakinra",
                                          ifelse(il6_type == "3", "drug X", 
                                                 ifelse(il6_type == "10", "Other IL6 inhibitor", NA)))),
    meds_oth1             = NA,
    meds_oth2             = NA,
    meds_oth3             = NA,
    sep_resus             = NA
    
  ) %>%
  mutate(vent = vent_new) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, antibiot, antibiot_type, antibiot_type2, antivir_med,
    antivir_type, antivirtype, chlor, cortico, corticost_type, hydroxychl,
    il6, il6_type, meds_oth1, meds_oth2, meds_oth3,
    nebu, prone, trialdrugs, study_convpl, study_oth,
    study_oth_sp, sep_resus, vent, vent_sp, vent_type, venttype_sp
  ) %>%
  mutate_at(vars(-chi_number, -adm_date), list(factor))



################################
##### WP4 - FRAILTY SCORES #####
################################

table(data$gcs_total, exclude = NULL)

wp4_frailty <- data %>%
  mutate(
    avpu              = ifelse(avpu == "Alert", "1",
                               ifelse(avpu == "Verbal", "2",
                                      ifelse(avpu == "Pain", "3", 
                                             ifelse(avpu == "Unresponsive", "4", NA)))),
    frailty_barthel   = NA,
    frailty_sp        = NA,
    gcs               = ifelse(gcs == "No", "0",
                               ifelse(gcs == "Yes", "1", NA)),
    gcs_motor         = NA,
    gcs_verbal        = NA,
    gcs_visual        = NA
  ) %>%
  # select only required variables for this section
  select(
    chi_number, adm_date, avpu, frailty_any, frailty_barthel, frailty_cfs, frailty_sp,
    frailty_type, gcs_motor, gcs_total, gcs_verbal, gcs_visual
  ) %>%
  mutate_at(vars(-chi_number, -adm_date), list(factor))


################################
###### WP4 - VACCINATION #######
################################

table(data$gcs_total, exclude = NULL)

wp4_vacc <- data %>%
  mutate(
    bcg_scar     = NA,
    bcg_vacc     = NA,
    bcg_vaccyear = NA,
    flu_vacc     = NA,
    flu_vaccdate = NA,
    pcv_vacc     = NA,
    pcv_vaccdate = NA,
    ppv_vacc     = NA,
    ppv_vaccdate = NA
  ) %>%
  # select only required variables for this section
  select(
    chi_number, adm_date, bcg_scar, bcg_vacc, bcg_vaccyear, flu_vacc, flu_vaccdate,
    pcv_vacc, pcv_vaccdate, ppv_vacc, ppv_vaccdate
  ) %>%
  mutate_at(vars(-chi_number, -adm_date), list(factor))





##########################################
######## CONTACT INFORMATION #############
##########################################

contactinfo <- data %>%
  
  # closecont - can't provide this
  mutate(
    closecont = ifelse(contact == "NO", 0,
                       ifelse(contact == "YES", 1,
                              ifelse(contact == "Unknown", 8, NA)
                       )
    ),
    closecont = ifelse(is.na(closecont), 8, closecont),
    
    closecont_type = NA,
    
    # closecont_sp - can't provide this
    closecont_sp = NA
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, closecont, closecont_type, closecont_sp) %>%
  
  mutate_at(vars(-chi_number, -adm_date), list(factor))


##########################################
####### CO-MORBID CONDITIONS #############
##########################################

conditions <- data %>%
  
  # anaemia - only COCIN
  mutate(
    # anaemia = ifelse(anaemia == "NO", 0,
    #   ifelse(anaemia == "YES", 1, 8)
    # ),
    # anaemia = ifelse(is.na(anaemia), 8, anaemia),
    anaemia = NA,
    
    # asplenia - N/A
    asplenia = NA,
    
    # asthma - only COCIN
    asthma = ifelse(asthma == "NO", 0,
                    ifelse(asthma == "YES", 1, 8)
    ),
    asthma = ifelse(is.na(asthma), 8, asthma),
    
    # cancer - only COCIN
    cancer = ifelse(cancer == "NO", 0,
                    ifelse(cancer == "YES", 1, 8)
    ),
    cancer = ifelse(is.na(cancer), 8, cancer),
    
    # hypert - only COCIN
    hypert = ifelse(hypert == "NO", 0,
                    ifelse(hypert == "YES", 1, 8)
    ),
    hypert = ifelse(is.na(hypert), 8, hypert),
    
    # dement - only COCIN
    dement = ifelse(dementia == "NO", 0,
                    ifelse(dementia == "YES", 1, 8)
    ),
    dement = ifelse(is.na(dement), 8, dement),
    
    # diabetes - only COCIN
    diabetes = ifelse(diabetes == "NO", 0,
                      ifelse(diabetes == "YES", 1, 8)
    ),
    diabetes = ifelse(is.na(diabetes), 8, diabetes),
    
    # heartdis - only COCIN
    heartdis = ifelse(heartdis == "NO", 0,
                      ifelse(heartdis == "YES", 1, 8)
    ),
    heartdis = ifelse(is.na(heartdis), 8, heartdis),
    
    # immuno - only COCIN
    immuno = ifelse(immuno == "NO", 0,
                    ifelse(immuno == "YES", 1, 8)
    ),
    immuno = ifelse(is.na(immuno), 8, immuno),
    
    # liverdis - only COCIN
    liverdis = ifelse(liverdis == "NO", 0,
                      ifelse(liverdis == "YES", 1, 8)
    ),
    liverdis = ifelse(is.na(liverdis), 8, liverdis),
    
    # lungdis - N/A
    lungdis = NA,
    
    # lungdis_sp - N/A
    lungdis_sp = NA,
    
    # neuromusc - N/A
    neuromusc = NA,
    
    # obese - only COCIN
    obese = ifelse(obese == "NO", 0,
                   ifelse(obese == "YES", 1, 8)
    ),
    obese = ifelse(is.na(obese), 8, obese),
    
    # rendis - only COCIN
    rendis = ifelse(rendis == "NO", 0,
                    ifelse(rendis == "YES", 1, 8)
    ),
    rendis = ifelse(is.na(rendis), 8, rendis),
    
    # rheumat - only COCIN
    rheumat = ifelse(rheumat == "NO", 0,
                     ifelse(rheumat == "YES", 1, 8)
    ),
    rheumat = ifelse(is.na(rheumat), 8, rheumat),
    
    # Stroke - only COCIN
    # stroke = ifelse(stroke == "NO", 0,
    #   ifelse(stroke == "YES", 1, 8)
    # ),
    # stroke = ifelse(is.na(stroke), 8, stroke),
    stroke = NA,
    
    # tuberc - N/A
    tuberc = NA
  ) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, anaemia, asplenia, asthma, cancer, dement, diabetes,
    heartdis, hypert, immuno, liverdis, lungdis, lungdis_sp, neuromusc, obese,
    rendis, rheumat, stroke, tuberc
  ) %>%
  
  mutate_at(vars(-chi_number, -adm_date), list(factor))



####################################
######## OUTCOMES AND DEATH ########
####################################

table(data$complic_sp, exclude = NULL)

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
    outcome = ifelse(is.na(outcome), 8, outcome),
    
    deathdate = ifelse(outcome == "1", as.character(date_of_death), NA),
    deathdate = as.Date(deathdate),
    
    deathcause = ifelse(covid_death == "Yes", 1,
                        ifelse(covid_death == "No", 2,
                               ifelse(covid_death == "Unknown", 8, NA)
                        )
    ),
    deathcause = ifelse(is.na(deathdate), NA, deathcause),
    
    healthcare_contact = NA
  ) %>%
  
  # select only required variables for this section
  select(chi_number, adm_date, complic, complic_sp, outcome, deathcause, 
         deathdate, healthcare_contact) %>%
  
  mutate_at(vars(-chi_number, -adm_date, -deathdate), list(factor))


####################################
###### LAB TESTS AND RESULTS #######
####################################

table(data$resp_virus, exclude = NULL)

labtests <- data %>%
  mutate(
    
    # lab_covtest - marked as Yes for everyone with a test date (from ECOSS)
    lab_covtest = ifelse(!is.na(test_date), 1, 0),
    
    # lab_covtesttype - all PCR for those with test
    lab_covtesttype = ifelse(lab_covtest == 1, 1, NA),
    
    # lab_covtesttype_sp - mark as NA
    lab_covtesttype_sp = NA,
    
    # lab_covid - use result if a test was taken
    lab_covid = ifelse(lab_covtest == 1, result, NA),
    
    # covid_new - whether someone is confirmed, suspected, not etc.
    # covid_new = ifelse(lab_covid == 1 | covid == "Lab comfirmed", 1,
    #                    ifelse(lab_covid != 1 & covid == "Clinical suspected", 4,
    #                           ifelse(lab_covid != 1 & covid == "Other coronavirus",
    #                                  3, NA))),
    
    covid_new = ifelse(lab_covid == 1 | covid == "Lab comfirmed", 1, 2),
    
    covid_new = ifelse(is.na(covid_new), 2, covid_new),
    
    # seq - can't provide this
    seq = NA,
    
    # genetic_group = can't provide this
    genetic_group = NA
  ) %>%
  
  # rename variable
  mutate(covid = covid_new) %>%
  
  # select only required variables for this section
  select(
    chi_number, adm_date, lab_covid, lab_covtest, lab_covtesttype, lab_covtesttype_sp,
    genetic_group, lab_fluany, lab_mers, lab_othcov, resp_virus, seq
  ) %>%
  mutate_at(vars(-chi_number, -adm_date), list(factor))


##########################################
############## FULL DATA #################
##########################################

# Combine all datasets
IMOVE_data <- list(
  demographics, hospinfo, patientinfo, wp4_symp_pre, wp4_meds_pre, 
  symptoms, hospitaltests, wp4_hosptest_mp, medications, wp4_frailty, 
  wp4_vacc, contactinfo, conditions, outcomes, labtests
) %>%
  map(arrange, chi_number, adm_date) %>%
  # remove adm_date variable as this is for joining only
  map(select, c(-adm_date, -chi_number)) %>%
  bind_cols()

# Remove datasets no longer required
rm(
  demographics, hospinfo, patientinfo, wp4_symp_pre, wp4_meds_pre, symptoms, 
  hospitaltests, wp4_hosptest_mp, medications, wp4_frailty, wp4_vacc, 
  contactinfo, conditions, outcomes, labtests
)

# Assign each hospital code a generic number
hospnums <- IMOVE_data %>%
  group_by(hospitalcode) %>%
  summarise(total = n()) %>%
  mutate(hospnum = row_number()) %>%
  select(hospnum, hospitalcode)

# Commented out as we don't need this file, hospnum doesn't need to be the same
# for each hospital for each submission
# save out for reference
# write_rds(hospnums, path(here("output", str_glue("Hospital_Numbers_{today()}.rds"))),
#           compress = "gz"
# )

# Add numbers into IMOVE data and drop Hospital code
IMOVE_data <- IMOVE_data %>%
  left_join(hospnums, by = "hospitalcode") %>%
  select(-hospitalcode) %>%
  select(idcountry, hosp_id2, hospnum, everything())

# remove
rm(hospnums)