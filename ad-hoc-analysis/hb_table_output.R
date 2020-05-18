library(gt)
hb_summary_ex_travelled %>% 
  gt() %>% 
  tab_header(title = "Summary of NHS Scotland Health Boards",
             subtitle = str_glue("Excludes {n_travelled} who travelled in the 14 days prior to admission",
                                 n_travelled = length(travelled))) %>% 
  cols_align(
    align = "right"
  ) %>% 
  cols_align(
    align = "left",
    columns = vars(HB2019Name)
  ) %>% 
  cols_label(
    HB2019Name = "NHS Health Board",
    total_cases = "Number of cases",
    n_hosptials = "Number of hospitals",
    confirmed_cases = "Number of confirmed",
    suspected_cases = "Number of suspected",
    unknown_cases = "Number where status is unknown",
    deaths = "Number of deaths"
  ) 
