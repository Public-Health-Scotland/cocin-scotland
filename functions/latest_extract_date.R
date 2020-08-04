# function to take latest extract from data folder

latest_extract_date <- function() {
  latest_date <- list.files(here("data"),
    pattern = "^\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}_"
  ) %>%
    if_else(stringr::str_detect(., regex("scot-data", ignore_case = TRUE)), ., NA_character_) %>%
    stringr::str_sub(1, 16) %>%
    max(na.rm = T)
  
  return(dplyr::if_else(is.na(latest_date), "1900-01-01_00-00", latest_date))
}


### END OF SCRIPT ###
