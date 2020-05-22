# function to take latest extract from data folder

latest_extract_date <- function(){
  
  list.files(here("data")) %>%
    str_sub(1, 16) %>%
    ymd_hm() %>%
    max(na.rm = TRUE) %>%
    format("%Y-%m-%d_%H-%M")
  
}


### END OF SCRIPT ###