# function to take latest extract from data folder

latest_extract_date <- function() {
  list.files(here("data"),
    pattern = "^\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}_"
  ) %>%
    str_sub(1, 16) %>%
    max()
    if_else(stringr::str_detect(., regex("scot-data", ignore_case = TRUE)), ., NA_character_) %>%
}


### END OF SCRIPT ###
