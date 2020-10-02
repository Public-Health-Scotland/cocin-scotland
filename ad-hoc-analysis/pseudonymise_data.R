# Pseudonymise data

IMOVE_data_anon <- anonymise_chi(IMOVE_data, hosp_id2, lookup_file = "CHI_lookup.rds")

table(test$multiple_episode, exclude = NULL)
table(test$multiple_hosp, exclude = NULL)
table(rapid_data$readmission, exclude = NULL)

test <- IMOVE_data %>%
  filter(!is.na(onsetdate))

# APPEND ANON ID WITH MARKER FOR NUMBER OF ADMISSION IN DATA
IMOVE_data_anon <- IMOVE_data_anon %>%
  # group by ID
  group_by(anon_id) %>%
  # Sort by admission and discharge dates
  arrange(admitdate, dischargedate) %>%
  # Get row number and max row number
  mutate(row = row_number(),
         max_row = max(row)) %>%
  # Ungroup
  ungroup() %>%
  # If ID has more than one row in data, append the row number
  mutate(id_append = ifelse(max_row > 1, as.character(row), ""),
         anon_id = paste0(anon_id, id_append)) %>%
  # remove variables not required
  select(-row, -max_row, -id_append) %>%
  # order variables
  select(idcountry, anon_id, everything()) %>%
  # Remove Postcode variable
  select(-postcode)

# Removing NA onsetdate
IMOVE_data_anon <- IMOVE_data_anon %>%
  filter(!is.na(onsetdate)) 

write_rds(IMOVE_data_anon, "data/IMOVE_data_27-08-20_Anon.rds")
write_csv(IMOVE_data_anon, "data/IMOVE_data_27-08-20_Anon.csv")

test <- read_rds("data/IMOVE_data_27-08-20_Anon.rds")
