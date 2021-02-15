# Pseudonymise data

# Once IG is in place, the below commented-out code can be used to pseudonymise CHI.
# Script currently drops CHI and postcode and randomises rows.

# IMOVE_data_anon <- anonymise_chi(
#   data = IMOVE_data,
#   chi = hosp_id2,
#   lookup_file = "CHI_lookup_salted.rds",
#   key = rstudioapi::askForPassword("Enter (secret) key for hashing  or press cancel to not use one")
# )
# 
# # APPEND ANON ID WITH MARKER FOR NUMBER OF ADMISSION IN DATA
# IMOVE_data_anon <- IMOVE_data_anon %>%
#   mutate(anon_id = str_glue("{anon_id}-{multiple_episode}")) %>%
#   # order variables
#   select(idcountry, anon_id, everything()) %>%
#   # Remove Postcode variable
#   select(-postcode)
# 
# # No IG for pseudonomised data so dropping anon_id
# IMOVE_data_anon <- IMOVE_data_anon %>%
#   arrange(anon_id) %>%
#   # Taking out row number for anonymisation reasons
# #  mutate(row_num = row_number(), .before = everything()) %>%
#   select(-anon_id)

# No IG so drop hosp_id2 (chi number) and postcode
IMOVE_data_anon <- IMOVE_data %>%
  select(-hosp_id2, -postcode)

# Randomise rows for anonymisation reasons
rows <- sample(nrow(IMOVE_data_anon))
IMOVE_data_anon <- IMOVE_data_anon[rows, ]

# Removing NA onsetdate
# IMOVE_data_anon <- IMOVE_data_anon %>%
#   filter(!is.na(onsetdate))

# save as .rds, .dta, and .csv files - currently we only provide .csv
write_rds(IMOVE_data_anon, here("output", str_glue("IMOVE_data_{today()}_Anon.rds")), compress = "gz")
haven::write_dta(IMOVE_data_anon, here("output", str_glue("IMOVE_data_{today()}_Anon.dta")), version = 15)
write_csv(IMOVE_data_anon, here("output", str_glue("IMOVE_data_{today()}_Anon.csv")))

# Copy files to server
imove_anon_local_1 <- path(here("output", str_glue("IMOVE_data_{today()}_Anon.rds")))

# .rds file
if (file_exists(imove_anon_local_1)) {
  file_copy(
    path = imove_anon_local_1,
    new_path = path(server_dir_final, str_glue("IMOVE_data_{today()}_Anon.rds"))
  )
}

# .dta file
imove_anon_local_2 <- path(here("output", str_glue("IMOVE_data_{today()}_Anon.dta")))

if (file_exists(imove_anon_local_2)) {
  file_copy(
    path = imove_anon_local_2,
    new_path = path(server_dir_final, str_glue("IMOVE_data_{today()}_Anon.dta"))
  )
}

# .csv file
imove_anon_local_3 <- path(here("output", str_glue("IMOVE_data_{today()}_Anon.csv")))

if (file_exists(imove_anon_local_3)) {
  file_copy(
    path = imove_anon_local_3,
    new_path = path(server_dir_final, str_glue("IMOVE_data_{today()}_Anon.csv"))
  )
}
