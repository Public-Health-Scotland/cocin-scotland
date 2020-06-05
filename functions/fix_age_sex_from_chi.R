sex_from_chi <- function(chi) {
  sex <- if_else((str_sub(chi, 9, 9) %>%
    as.integer() %>%
    mod(2)) == 0, "Female", "Male") %>%
    factor(levels = c("Male", "Female", "Not specified"))
}

dob_from_chi <- function(chi) {
  date1 <- dmy(str_replace(chi, "^(\\d{4})(\\d{2})\\d{4}$", "\\119\\2"))
  date2 <- dmy(str_replace(chi, "^(\\d{4})(\\d{2})\\d{4}$", "\\120\\2"))

  return(case_when(
    date2 >= today() ~ date1,
    time_length(date1 %--% today(), unit = "years") > 110 ~ date2,
    TRUE ~ NA_Date_
  ))
}

fix_age_sex_from_chi <- function(data) {
  valid_chis <- data %>%
    select(subjid, nhs_chi, agedat, age, sex, hostdat, daily_dsstdat, cestdat, dsstdat) %>%
    group_by(subjid) %>%
    summarise_all(~ first(na.omit(.))) %>%
    filter(phsmethods::chi_pad(nhs_chi) %>% phsmethods::chi_check() == "Valid CHI")

  missing_age <- valid_chis %>% filter(is.na(age))
  missing_sex <- valid_chis %>% filter(is.na(sex))

  fixed_age <- missing_age %>%
    filter(is.na(agedat)) %>%
    mutate(
      agedat = dob_from_chi(nhs_chi),

      # Date at which to calculate age
      anydat = coalesce(hostdat, daily_dsstdat, cestdat, dsstdat),

      # Calculate age using DOB and anydat derived above
      age = interval(agedat, anydat) %>%
        as.period() %>%
        year() %>%
        as.double()
    ) %>%
    select(subjid, agedat, age)

  fixed_sex <- missing_sex %>%
    mutate(sex = sex_from_chi(sex)) %>%
    select(subjid, sex)

  fixed_data <- reduce(list(data, fixed_sex, fixed_age),
    left_join,
    by = "subjid",
    suffix = c("", "_fix")
  ) %>%
    mutate(
      age = if_else(is.na(age_fix), age, age_fix),
      agedat = if_else(is.na(agedat_fix), agedat, agedat_fix),
      sex = if_else(is.na(sex_fix), sex, sex_fix)
    )

  return(fixed_data)
}
