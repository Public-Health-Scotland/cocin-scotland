latest_server_data <- function(dataset = NULL) {
  if (is.null(dataset)) stop("Need to specify a dataset e.g. cocin")

  list.files(server_dir,
    pattern = "^\\d{4}-\\d{2}-\\d{2}.+?\\.rds$"
  ) %>%
    if_else(str_detect(., regex(dataset, ignore_case = TRUE)), ., NA_character_) %>%
    str_sub(1, 10) %>%
    max(na.rm = T)
}
