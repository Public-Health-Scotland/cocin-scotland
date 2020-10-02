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

# Removing NA onsetdate
# IMOVE_data_anon <- IMOVE_data_anon %>%
#   filter(!is.na(onsetdate)) 

write_rds(IMOVE_data_anon, here("data", str_glue("IMOVE_data_{today()}_Anon.rds")), compress = "gz")
write_csv(IMOVE_data_anon, here("data", str_glue("IMOVE_data_{today()}_Anon.csv")))

test <- read_rds(here("data", str_glue("IMOVE_data_{today()}_Anon.rds")))
