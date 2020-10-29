source("extract-data/00_setup-environment.R")

# Read PIS data 
pis_extract <- read_csv(path(server_dir, "2020-10-29_PIS_extract.csv.gz"), skip = 3) %>% 
  clean_names() %>% 
  mutate(disp_date = dmy(disp_date))

# Have one perscription per line
# Flag drugs of interest
pis_extract_clean <- pis_extract %>% 
  mutate(ace_pre = NA_character_,
         antivir_pre = NA_character_,
         arb_pre = NA_character_,
         chemo_pre = NA_character_,
         chloroq_pre = NA_character_,
         corticost_pre = NA_character_,
         corticost_pre_type = NA_character_,
         dmards_pre = NA_character_,
         gliclaz_pre = NA_character_,
         hydroxychloroq_pre = str_detect(pi_bnf_root_drug_description, regex("Hydroxychloroquine", ignore_case = TRUE)),
         il6_pre = NA_character_,
         il6pre_type = NA_character_,
         metform_pre = str_detect(pi_bnf_root_drug_description, regex("Metformin", ignore_case = TRUE)),
         nsaid_pre = NA_character_,
         other1_pre_sp = NA_character_,
         other2_pre_sp = NA_character_,
         other3_pre_sp = NA_character_,
         psychotrop_pre = NA_character_,
         statin_pre = NA_character_,
         steroids_pre = NA_character_) %>% count(hydroxychloroq_pre, metform_pre)

#TODO  
# Drop any lines which haven't been flagged
# Nest the data by CHI 
# Match the nested data to full dataset
# compare admit dates to disp_date (nested - use map in mutate) to produce a logical
# change to Y/N/DK factor


  
