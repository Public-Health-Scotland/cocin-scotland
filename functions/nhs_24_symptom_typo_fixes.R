# Find typo list
# medical_terms <- c("DIARRHEA", "ABSECESS", "ABSECESSES")
#
# all_typos <- hunspell(nhs_24_clean$symptoms_free, dict = dictionary("en_GB", add_words = medical_terms)) %>%
#   unlist() %>%
#   as_tibble() %>%
#   count(value, sort = T)

# Fix typos

fun.fix.diarrhea <- function(data, symptom_text) {
  typo_regex <- "\\b(DI(ARRHOEA|ARROHEA|AHORREA|ARRHEO|RRHOEA|ARRHEAA|AARR\\b))"
  correct_spelling <- "DIARRHEA"

  data <- data %>%
    # mutate(new = sub(typo_regex, correct_spelling, {{symptom_text}}))
    mutate(across({{ symptom_text }}, ~ gsub(typo_regex, correct_spelling, .x)))

  return(data)
}

fun.fix.cough <- function(data, symptom_text) {
  typo_regex <- "\\b(CO(GH|GUH|GH|UGH|UG|FF(ING)\\b)(ED)?(ING)?)"
  correct_spelling <- "COUGH\\3\\4\\5"

  data <- data %>%
    # mutate(new = sub(typo_regex, correct_spelling, {{symptom_text}}))
    mutate(across({{ symptom_text }}, ~ gsub(typo_regex, correct_spelling, .x)))

  return(data)
}

fun.fix.times <- function(data, symptom_text) {
  typo_regex <- "(\\d)?(\\s)?(HR(S?))\\b"
  correct_spelling <- "\\1 HOUR\\4"

  data <- data %>%
    # mutate(new = sub("\\b(HRS?)\\b", "HOURS", {{symptom_text}}))
    mutate(across({{ symptom_text }}, ~ gsub(typo_regex, correct_spelling, .x)))

  typo_regex <- "(\\d)?(\\s)?(WK(S?))\\b"
  correct_spelling <- "\\1 WEEK\\4"

  data <- data %>%
    mutate(across({{ symptom_text }}, ~ gsub(typo_regex, correct_spelling, .x)))

  return(data)
}

fun.fix.abscess <- function(data, symptom_text) {
  typo_regex <- "\\b(AB[CS]{1,2}ESS?(ES)?)\\b"
  correct_spelling <- "ABSECESS\\2"

  data <- data %>%
    mutate(across({{ symptom_text }}, ~ gsub(typo_regex, correct_spelling, .x)))

  return(data)
}

# typo_test <- nhs_24_clean %>%
#   select(symptoms_free) %>%
#   sample_n(10000)
# 
# typo_test %>%
#   mutate(typos_before = hunspell(symptoms_free, dict = dictionary("en_GB", add_words = medical_terms)) %>%
#     map_int(~ sum(!is.na(.x)))) %>%
#   fun.fix.abscess(symptoms_free) %>%
#   fun.fix.diarrhea(symptoms_free) %>%
#   fun.fix.times(symptoms_free) %>%
#   mutate(typos_after = hunspell(symptoms_free, dict = dictionary("en_GB", add_words = medical_terms)) %>%
#     map_int(~ sum(!is.na(.x)))) %>%
#   summarise(across(contains("typos"), sum))
# 
# 
# missed_times <- nhs_24_clean %>%
#   select(symptoms_free) %>%
#   fun.fix.times(symptoms_free) %>%
#   mutate(time_error = hunspell(symptoms_free, dict = dictionary("en_GB", add_words = medical_terms)) %>%
#     map_int(~ sum(.x %in% c("HR", "HRS", "WK", "WKS")))) %>%
#   filter(time_error > 1)
# 
# 
# all_typos_new <- hunspell(nhs_24_clean %>%
#   fun.fix.abscess(symptoms_free) %>%
#   fun.fix.diarrhea(symptoms_free) %>%
#   fun.fix.times(symptoms_free) %>%
#   pull(symptoms_free), dict = dictionary("en_GB", add_words = medical_terms)) %>%
#   unlist() %>%
#   as_tibble() %>%
#  count(value, sort = T)
