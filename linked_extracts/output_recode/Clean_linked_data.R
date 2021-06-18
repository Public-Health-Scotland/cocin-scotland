source("extract-data/00_setup-environment.R")

######################
### FEB ADMISSIONS ###
######################

# Number of Feb admissions
feb_admissions <- data %>%
  filter(adm_date < "2020-03-01") %>%
  nrow()

# Bullet 1
bullet1 <- paste0("There were ", feb_admissions, " February admissions removed.\n")

# Censor Feb admissions
data <- data %>%
  filter(adm_date >= "2020-03-01")

# remove
rm(feb_admissions)

######################
### ICU ADMISSIONS ###
######################

# check those with icu admission/discharge dates have ICU stay ticked
data_icu_check <- data %>%
  filter(!is.na(icuadmitdate) | !is.na(icudisdate)) %>%
  filter(icu != "Yes") %>%
  nrow()

# Bullet 2
bullet2 <- paste0("There were ", data_icu_check, " records that had an ICU admission/discharge date ",
                  "but was nor marked as having an ICU admission.\n")

# check those with icu admission/discharge dates have ICU stay ticked
data <- data %>%
  mutate(icu = ifelse((!is.na(icuadmitdate) | !is.na(icudisdate)) & icu != "Yes", "Yes", icu))

# remove
rm(data_icu_check)
         
         
#######################         
### DISCHARGE DATES ###
#######################

# Check dis date is not before adm date
data_discheck <- data %>%
  filter(dis_date < adm_date) %>%
  nrow()

# Bullet 3
bullet3 <- paste0("There were ", data_discheck, " records that had a discharge date prior to the ",
                  "admission date.\n")

# Remove discharge dates which are prior to admission date
data <- data %>%
  mutate(dis_date = ifelse(dis_date < adm_date, NA, as.character(dis_date))) %>%
  mutate(dis_date = as.Date(dis_date))


#######################         
##### TEST DATES ######
#######################

# Check test date is not more than 21 days before adm date
data_testcheck1 <- data %>%
  filter(test_date < (adm_date-21)) %>%
  nrow()

# Bullet 4
bullet4 <- paste0("There were ", data_testcheck1, " records that had a test date more than 21 days ",
                  "before the admission date.\n")

# Remove test dates which are prior to 21 days before admission date
data <- data %>%
  mutate(test_date = ifelse(test_date < (adm_date-21), NA, as.character(test_date))) %>%
  mutate(test_date = as.Date(test_date))

# Check dis date is not before adm date
data_testcheck2 <- data %>%
  filter(!is.na(dis_date) & test_date > dis_date) %>%
  nrow()

# Bullet 5
bullet5 <- paste0("There were ", data_testcheck2, " records that had a test date after the ",
                  "discharge date.\n")

# Remove test dates which are after discharge date
data <- data %>%
  mutate(test_date = ifelse(!is.na(dis_date) & test_date > dis_date, NA, as.character(test_date))) %>%
  mutate(test_date = as.Date(test_date))



#######################         
#### ONSET DATES ######
#######################

# Check onset date is not before 14 days prior to adm date
data_onsetcheck1 <- data %>%
  filter(onsetdate < (adm_date-14)) %>%
  nrow()

# Bullet 4
bullet6 <- paste0("There were ", data_onsetcheck1, " records that had an onset date more than 14 days ",
                  "before the admission date.\n")

# Remove onset dates which are prior to 14 days before admission date
data <- data %>%
  mutate(onsetdate = ifelse(onsetdate < (adm_date-14), NA, as.character(onsetdate))) %>%
  mutate(onsetdate = as.Date(onsetdate))

# Check onset date is not after admission date
data_onsetcheck2 <- data %>%
  filter(!is.na(dis_date) & onsetdate > dis_date) %>%
  nrow()

# Bullet 7
bullet7 <- paste0("There were ", data_onsetcheck2, " records that had an onset date after the ",
                  "discharge date.\n")

# Remove onset dates which are after discharge date
data <- data %>%
  mutate(onsetdate = ifelse(!is.na(dis_date) & onsetdate > dis_date, NA, as.character(onsetdate))) %>%
  mutate(onsetdate = as.Date(onsetdate))

#######################         
#### DEATH DATES ######
#######################

# Check death date is not before admission
data_deathcheck1 <- data %>%
  filter(date_of_death < adm_date) %>%
  nrow()

# Bullet 8
bullet8 <- paste0("There were ", data_deathcheck1, " records that had a death date before ",
                  "the admission date.\n")

# Remove records where death occurs before admission
data <- data %>%
  filter(is.na(date_of_death) | date_of_death >= adm_date)


# Check death date is not before admission
data_deathcheck2 <- data %>%
  filter(!is.na(date_of_death) & !is.na(dis_date) & date_of_death < dis_date) %>%
  nrow()

# Bullet 9
bullet9 <- paste0("There were ", data_deathcheck2, " records that had a death date before ",
                  "the discharge date.\n")

# Remove records where death occurs before admission
data <- data %>%
  mutate(dis_date = ifelse(!is.na(date_of_death) & !is.na(dis_date) & date_of_death < dis_date,
                           as.character(date_of_death), as.character(dis_date))) %>%
  mutate(dis_date = as.Date(dis_date))



# Check death date is not before admission
data_deathcheck3 <- data %>%
  filter(!is.na(date_of_death) & !is.na(icuadmitdate) & date_of_death < icuadmitdate) %>%
  nrow()

# Bullet 10
bullet10 <- paste0("There were ", data_deathcheck3, " records that had a death date before ",
                  "the ICU admission date.\n")

# Remove ICU dates records where death occurs before ICU admission
data <- data %>%
  mutate(icuadmitdate = ifelse(!is.na(date_of_death) & !is.na(icuadmitdate) & date_of_death < icuadmitdate,
                           NA, as.character(icuadmitdate)),
         icudisdate = ifelse(!is.na(date_of_death) & !is.na(icuadmitdate) & date_of_death < icuadmitdate,
                               NA, as.character(icudisdate))) %>%
  mutate(icuadmitdate = as.Date(icuadmitdate),
         icudisdate = as.Date(icudisdate))


# Check death date is not before admission
data_deathcheck4 <- data %>%
  filter(!is.na(date_of_death) & !is.na(icudisdate) & date_of_death < icudisdate) %>%
  nrow()

# Bullet 11
bullet11 <- paste0("There were ", data_deathcheck4, " records that had a death date before ",
                   "the ICU discharge date.\n")

# Remove ICU dates records where death occurs before ICU admission
data <- data %>%
  mutate(icudisdate = ifelse(!is.na(date_of_death) & !is.na(icuadmitdate) & 
                             !is.na(icudisdate) & date_of_death < icudisdate,
                             as.character(date_of_death), as.character(icudisdate))) %>%
  mutate(icudisdate = as.Date(icudisdate))

# Check death date is not before admission
data_deathcheck5 <- data %>%
  filter(!is.na(date_of_death) & !is.na(test_date) & date_of_death < test_date) %>%
  nrow()

# Bullet 12
bullet12 <- paste0("There were ", data_deathcheck5, " records that had a death date before ",
                   "the test date.\n")

# Remove ICU dates records where death occurs before ICU admission
data <- data %>%
  mutate(test_date = ifelse(!is.na(date_of_death) & !is.na(test_date) & 
                             date_of_death < test_date,
                             NA, as.character(test_date))) %>%
  mutate(test_date = as.Date(test_date))

# Check death date is not before admission
data_deathcheck6 <- data %>%
  filter(!is.na(date_of_death) & !is.na(onsetdate) & date_of_death < onsetdate) %>%
  nrow()

# Bullet 13
bullet13 <- paste0("There were ", data_deathcheck6, " records that had a death date before ",
                   "the onset date.\n")

# Remove ICU dates records where death occurs before ICU admission
data <- data %>%
  mutate(onsetdate = ifelse(!is.na(date_of_death) & !is.na(onsetdate) & 
                              date_of_death < onsetdate,
                            NA, as.character(onsetdate))) %>%
  mutate(onsetdate = as.Date(onsetdate))

#######################         
##### ICU DATES #######
#######################

# Check ICU discharge date is not before ICU admission date
data_icucheck1 <- data %>%
  filter(icudisdate < icuadmitdate) %>%
  nrow()

# Bullet 14
bullet14 <- paste0("There were ", data_icucheck1, " records that had an ICU discharge ",
                  "date before the ICU admission date.\n")

# Remove records where death occurs before admission
data <- data %>%
  mutate(icudisdate = ifelse(icudisdate < icuadmitdate, NA, as.character(icudisdate))) %>%
  mutate(icudisdate = as.Date(icudisdate))


# Check ICU admission date is not before hospital admission date
data_icucheck2 <- data %>%
  filter(icuadmitdate < adm_date) %>%
  nrow()

# Bullet 15
bullet15 <- paste0("There were ", data_icucheck2, " records that had an ICU admission ",
                   "date before the hospital admission date.\n")

# If ICU admission is before hospital admission, set to hosp adm date
data <- data %>%
  mutate(icuadmitdate = ifelse(icuadmitdate < adm_date, as.character(adm_date), 
                             as.character(icuadmitdate))) %>%
  mutate(icuadmitdate = as.Date(icuadmitdate))

# Check ICU admission date is not before hospital admission date
data_icucheck3 <- data %>%
  filter(icudisdate > dis_date) %>%
  nrow()

# Bullet 16
bullet16 <- paste0("There were ", data_icucheck3, " records that had an ICU discharge ",
                   "date after the hospital discharge date.\n")

# If ICU discharge is after hospital discharge, set to hosp dis date
data <- data %>%
  mutate(icudisdate = ifelse(icudisdate > dis_date, as.character(dis_date), 
                               as.character(icudisdate))) %>%
  mutate(icuadmitdate = as.Date(icuadmitdate))


# Save out info

# save as .rds
write_rds(data, here("output", str_glue("Linked_Dataset_clean_{today()}.rds")), compress = "gz")

# Save out info on cleaned data
cat(bullet1,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"))
cat(bullet2,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet3,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet4,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet5,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet6,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet7,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet8,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet9,  file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet10, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet11, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet12, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet13, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet14, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet15, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)
cat(bullet16, file = str_glue("output/Cleaned_linked_data_info_{today()}.txt"), append = TRUE)


# Copy files to server
clean_data_local_1 <- path(here("output", str_glue("Linked_Dataset_clean_{today()}.rds")))

# .rds file
if (file_exists(clean_data_local_1)) {
  file_copy(
    path = clean_data_local_1,
    new_path = path(server_dir_final, str_glue("Linked_Dataset_clean_{today()}.rds"))
  )
}


clean_data_local_2 <- path(here("output", str_glue("Cleaned_linked_data_info_{today()}.txt")))

# .txt file
if (file_exists(clean_data_local_2)) {
  file_copy(
    path = clean_data_local_2,
    new_path = path(server_dir_info, str_glue("Cleaned_linked_data_info_{today()}.txt"))
  )
}



