library(fs)
library(dplyr)
dir_info("data", regexp = "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}_scot") %>%
  as_tibble() %>%
  select(path, birth_time) %>%
  filter(birth_time < max(date(birth_time))) %>%
  pull(path) %>%
  file_delete()
