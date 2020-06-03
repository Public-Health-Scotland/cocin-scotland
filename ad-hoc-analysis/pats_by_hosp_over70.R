
# Run setup environment for other packages and functions
source(here::here("extract-data", "00_setup-environment.R"))

# Read in latest cleaned data extract
scot_data <-
  read_rds(
    here("data", 
         paste0(latest_extract_date(),
                "_scot-data-clean.rds") 
    )
  )

output <-
  scot_data %>%
  group_by(subjid) %>%
  mutate(over70 = first(na.omit(age)) >= 70,
         adm_30apr = first(na.omit(hostdat) >= dmy(30042020))
  ) %>%
  mutate(
    status = 
      case_when(
        over70 & adm_30apr ~ "over70_after30apr",
        over70 & !adm_30apr ~ "over70_before30apr",
        TRUE ~ "other"
      )) %>% 
  group_by(hb_name, redcap_data_access_group, hospid, location_name, status) %>% 
  summarise(n = n_distinct(subjid)) %>%
  ungroup() %>%
  pivot_wider(names_from = status, values_from = n, values_fill = list(n = 0)) %>%
  mutate(total_pats = reduce(select_if(., is.integer), `+`))

# Save output file
write_csv(
  output,
  here("output", 
       paste0(latest_extract_date(),
              "_hosp_over70.csv"))
)


### END OF SCRIPT ###