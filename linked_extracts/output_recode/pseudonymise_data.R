# Pseudonymise data

IMOVE_data_anon <- anonymise_chi(
  data = IMOVE_data,
  chi = hosp_id2,
  lookup_file = "CHI_lookup_salted.rds",
  key = rstudioapi::askForPassword("Enter (secret) key for hashing  or press cancel to not use one")
)

# APPEND ANON ID WITH MARKER FOR NUMBER OF ADMISSION IN DATA
IMOVE_data_anon <- IMOVE_data_anon %>%
  mutate(anon_id = str_glue("{anon_id}-{multiple_episode}")) %>%
  # order variables
  select(idcountry, anon_id, everything()) %>%
  # Remove Postcode variable
  select(-postcode)

# No IG for pseudonomised data so dropping anon_id
IMOVE_data_anon <- IMOVE_data_anon %>%
  arrange(anon_id) %>%
  mutate(row_num = row_number(), .before = everything()) %>%
  select(-anon_id)

# Removing NA onsetdate
# IMOVE_data_anon <- IMOVE_data_anon %>%
#   filter(!is.na(onsetdate))

write_rds(IMOVE_data_anon, here("data", str_glue("IMOVE_data_{today()}_Anon.rds")), compress = "gz")
haven::write_dta(IMOVE_data_anon, here("data", str_glue("IMOVE_data_{today()}_Anon.dta")), version = 15)
write_csv(IMOVE_data_anon, here("data", str_glue("IMOVE_data_{today()}_Anon.csv")))

test <- read_rds(here("data", str_glue("IMOVE_data_{today()}_Anon.rds")))
