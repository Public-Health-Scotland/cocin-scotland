

genomics_hospdata <- data %>%
   mutate(ethnic_group = ifelse(substr(ethnicity_code,1,1) == '1',
                                     'White',
                         ifelse(substr(ethnicity_code,1,1) == '2',
                                     'Mixed or multiple ethnic groups',
                         ifelse(substr(ethnicity_code,1,1) == '3',
                                     'Asian, Asian Scottish or Asian British',
                         ifelse(substr(ethnicity_code,1,1) == '4',
                                     'African',
                         ifelse(substr(ethnicity_code,1,1) == '5',
                                     'Caribbean or Black',
                         ifelse(substr(ethnicity_code,1,1) == '6',
                                     'Other ethnic group',
                         ifelse(ethnicity_code == '98',
                                     'Refused/Not provided by patient',
                         ifelse(ethnicity_code == '99',
                                     'Not Known', NA))))))))) %>%


rename(CHI = chi_number,
       First_name = forename,
       Surname = surname,
       Postcode = postcode,
       HB_Residence = health_board_of_residence,
       HB_Treatment = health_board_of_treatment,
       Sex = sex,
       DOB = dob,
       Ethnicity = ethnicity,
       Ethnicity_Group = ethnic_group,
       AdmitHosp = adm_date,
       Admit_ICU = icu,
       AdmitUnit = icuadmitdate,
       DiscDate  = icudisdate,
       DateDiscHosp = dis_date,
       Date.Death = date_of_death,
       SpecimenDate = test_date,
       ECOSSID = ecossid,
       Death_Cause = covid_death) %>%
  
  mutate(Admit_Hosp = "Yes",
         
         anaemia = ifelse(anaemia == "NO", 0,
                          ifelse(anaemia == "YES", 1, 8)),
         anaemia   = ifelse(is.na(anaemia), 8, anaemia),
         
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
         hypert = ifelse(hypert == "2", 0,
                         ifelse(hypert == "1", 1, 8)),
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
         
         lungdis = NA,
         
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
         
         tuberc = NA,
         
         Death_Cause = ifelse(Death_Cause == "Yes", 1, 
                              ifelse(Death_Cause == "No", 2, 
                                     ifelse(Death_Cause == "Unknown", 8, NA)))) %>%
  
  select(CHI, ECOSSID, First_name, Surname, Sex, Postcode, HB_Residence, HB_Treatment, 
         DOB, Ethnicity, Ethnicity_Group, Admit_Hosp, 
         AdmitHosp, DateDiscHosp, Admit_ICU, AdmitUnit, DiscDate, covidICUorHDU,
         SpecimenDate, Date.Death, Death_Cause, anaemia, asplenia, asthma, cancer, 
         dement, diabetes, heartdis, hypert, immuno, liverdis, lungdis, neuromusc, 
         obese, rendis, rheumat, stroke, tuberc)


