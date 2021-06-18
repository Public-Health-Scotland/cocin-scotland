source("extract-data/00_setup-environment.R")

#######################         
### COCIN CHECKS ######
#######################

# Create months vector for ordering data correctly
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
            "Nov", "Dec")
months_2020 <- paste0(months, "-2020")
months_2021 <- paste0(months, "-2021")
months_2022 <- paste0(months, "-2022")

months <- c(months_2020, months_2021, months_2022)

# Total admissions, cocin admissions, and perc by month
data_cocin <- data %>%
  mutate(cocin = ifelse(!is.na(subjid), 1, 0)) %>%
  mutate(year    = year(adm_date),
         month   = month(adm_date, label = T)) %>%
  mutate(adm_month = paste0(month, '-', year)) %>%
  group_by(adm_month) %>%
  summarise(total_admissions = n(),
            cocin_admissions = sum(cocin)) %>%
  mutate(perc = round_half_up((cocin_admissions/total_admissions)*100, 1)) %>%
  mutate(adm_month = factor(adm_month, levels = months)) %>%
  arrange(adm_month)

# Total records
total_admissions <- nrow(data)

# Cocin admissions
cocin_admissions <- data %>%
  filter(!is.na(subjid)) %>%
  nrow()

# Percentage cocin admissions
cocin_perc <- round_half_up((cocin_admissions/total_admissions)*100,1)

# Cocin - Bullet 1
cocin_bullet1 <- paste0(cocin_admissions, " of ", total_admissions, " (",
                        cocin_perc, "%) admissions have been matched to COCIN.\n")

###############################
###### COCIN ADM DATES ########
###############################

# Does COCIN admission date match RAPID
cocin_check1 <- data %>%
  filter(admitdate_cocin == adm_date) %>%
  nrow()

# Are COCIN admission date within 1 day of RAPID
cocin_check2 <- data %>%
  filter(admitdate_cocin != adm_date) %>%
  filter(admitdate_cocin %in% c(adm_date-1, adm_date+1)) %>%
  nrow()

# Cocin - Bullet 2
cocin_bullet2 <- paste0("Of ", cocin_admissions, " COCIN admissions, there were ", 
                        cocin_check1, " records where the COCIN and RAPID admission dates",
                        " matched exactly while ", cocin_check2, " had admission dates ",
                        "within one day.\n")


###############################
###### COCIN DIS DATES ########
###############################

# Does COCIN discharge date match RAPID
cocin_check3 <- data %>%
  filter(dischargedate_cocin == dis_date) %>%
  nrow()

# Are COCIN discharge date within 1 day of RAPID
cocin_check4 <- data %>%
  filter(dischargedate_cocin != dis_date) %>%
  filter(admitdate_cocin %in% c(dis_date-1, dis_date+1)) %>%
  nrow()

cocin_check5 <- data %>%
  filter(dischargedate_cocin != dis_date) %>%
  filter(!admitdate_cocin %in% c(dis_date-1, dis_date+1)) %>%
  nrow()

cocin_check6 <- data %>%
  filter(!is.na(dischargedate_cocin) & is.na(dis_date)) %>%
  nrow()

cocin_check7 <- data %>%
  filter(!is.na(subjid) & is.na(dischargedate_cocin) & !is.na(dis_date)) %>%
  nrow()

cocin_check8 <- data %>%
  filter(!is.na(subjid) & is.na(dischargedate_cocin) & is.na(dis_date)) %>%
  nrow()

# Cocin - Bullet 3
cocin_bullet3 <- paste0("Of ", cocin_admissions, " COCIN admissions, there were ", 
                        cocin_check3, " records where the COCIN and RAPID discharge dates",
                        " matched exactly, ", cocin_check4, " had discharge dates ",
                        "within one day, and ", cocin_check5, " had mismatching discharge",
                        " dates greater than one day. ", cocin_check6, " had a missing ",
                        "RAPID discharge date, ", cocin_check7, " had a missing ",
                        "COCIN discharge date, while ", cocin_check8, 
                        " records had both a ",
                        "missing RAPID and COCIN discharge date.\n")

###############################
###### COCIN HOSP CODE ########
###############################

cocin_check9 <- data %>%
  filter(!is.na(subjid) & hospital_of_treatment_code == hospitalcode) %>%
  nrow()

cocin_check10 <- data %>%
  filter(!is.na(subjid) & hospital_of_treatment_code != hospitalcode) %>%
  nrow()

cocin_bullet4 <- paste0("Of ", cocin_admissions, " COCIN admissions, ", cocin_check9,
                        " had matching hospital codes to RAPID.\n")

###############################
########## COCIN DOB ##########
###############################

cocin_check11 <- data %>%
  filter(!is.na(subjid) & dob == dob_cocin) %>%
  nrow()

cocin_check12 <- data %>%
  filter(!is.na(subjid) & dob != dob_cocin) %>%
  nrow()

cocin_check13 <- data %>%
  filter(!is.na(subjid) & is.na(dob_cocin)) %>%
  nrow()

cocin_bullet5 <- paste0("Of ", cocin_admissions, " COCIN admissions, ", cocin_check11,
                        " had matching DOBs to RAPID. ", cocin_check12, " had mismatching",
                        " DOBs, while ", cocin_check13, " had missing COCIN DOB.\n")


###############################
########## COCIN SEX ##########
###############################

cocin_check14 <- data %>%
  filter(!is.na(subjid) & sex == sex_cocin) %>%
  nrow()

cocin_check15 <- data %>%
  filter(!is.na(subjid) & sex != sex_cocin) %>%
  nrow()

cocin_check16 <- data %>%
  filter(!is.na(subjid) & is.na(sex_cocin)) %>%
  nrow()

cocin_bullet6 <- paste0("Of ", cocin_admissions, " COCIN admissions, ", cocin_check14,
                        " had matching sex to RAPID. ", cocin_check15, " had mismatching",
                        " sex.\n")

# save as .rds
write_csv(data_cocin, here("output", str_glue("COCIN_info_{today()}.csv")))

# Save out info on cleaned data
cat(cocin_bullet1,  file = str_glue("output/COCIN_checks_{today()}.txt"))
cat(cocin_bullet2,  file = str_glue("output/COCIN_checks_{today()}.txt"), append = TRUE)
cat(cocin_bullet3,  file = str_glue("output/COCIN_checks_{today()}.txt"), append = TRUE)
cat(cocin_bullet4,  file = str_glue("output/COCIN_checks_{today()}.txt"), append = TRUE)
cat(cocin_bullet5,  file = str_glue("output/COCIN_checks_{today()}.txt"), append = TRUE)
cat(cocin_bullet6,  file = str_glue("output/COCIN_checks_{today()}.txt"), append = TRUE)


# Copy files to server
cocin_info_local_1 <- path(here("output", str_glue("COCIN_info_{today()}.csv")))

# .rds file
if (file_exists(cocin_info_local_1)) {
  file_copy(
    path = cocin_info_local_1,
    new_path = path(server_dir_info, str_glue("COCIN_info_{today()}.csv"))
  )
}


cocin_checks_local_1 <- path(here("output", str_glue("COCIN_checks_{today()}.txt")))

# .txt file
if (file_exists(cocin_checks_local_1)) {
  file_copy(
    path = cocin_checks_local_1,
    new_path = path(server_dir_info, str_glue("COCIN_checks_{today()}.txt"))
  )
}
