## Scottish Data Analysis

# Run data extract or load in from stored extract
# source("code/00 - Get Scottish data.R")
# source("code/01 - Prep data.R")
# scot_data <- read_rds("...")

# Some extra packages
library(ggplot2)
library(openxlsx)
library(gt)
library(ggplot2)
library(lubridate)


# Fixes -------------------------------------------------------------------
# Change the age groupings
scot_data <- scot_data %>%
  mutate(
    age.factor = case_when(
      age < 17 ~ "<17",
      age < 30 ~ "17-29",
      age < 40 ~ "30-39",
      age < 50 ~ "40-49",
      age < 60 ~ "50-59",
      age < 70 ~ "60-69",
      age < 80 ~ "70-79",
      is.na(age) ~ NA_character_,
      TRUE ~ "80+"
    )
  )


# Set-up ------------------------------------------------------------------

# Create lists of subjids who meet criteria
corna_confirmed <- scot_data %>%
  filter(corna_mbcat == "YES - Confirmed") %>%
  distinct(subjid) %>%
  pull(subjid)

corna_suspected <- scot_data %>%
  filter(corna_mbcat == "YES - Probable") %>%
  distinct(subjid) %>%
  pull(subjid)

corna_unknown <- scot_data %>%
  distinct(subjid) %>%
  filter(!subjid %in% c(corna_confirmed, corna_suspected)) %>%
  pull(subjid)

travelled <- scot_data %>%
  filter(travel_erterm == "Yes") %>%
  distinct(subjid) %>%
  pull(subjid)

died <- scot_data %>%
  filter(dsterm == "Death") %>%
  distinct(subjid) %>%
  pull(subjid)

male <- scot_data %>%
  filter(sex == "Male") %>%
  distinct(subjid) %>%
  pull(subjid)

female <- scot_data %>%
  filter(sex == "Female") %>%
  distinct(subjid) %>%
  pull(subjid)

# Breakdown of gender, age and death
# Used for population pyramid


pop_data <- scot_data %>% 
  group_by(subjid) %>% 
  filter(!subjid %in% travelled) %>% 
  mutate(died = if_else(subjid %in% died, 1, 0)) %>% 
  summarise(age = first(na.omit(age.factor)),
            sex = first(na.omit(sex)),
            ethnicity = first(na.omit(ethnicity)),
            pregnancy = first(na.omit(pregyn_rptestcd)),
            admission = first(na.omit(cestdat)),
            outcome = first(na.omit(dsterm))) %>%
  mutate(died = case_when(outcome == "Death" ~ "Yes", TRUE ~ "No") %>% factor(levels = c("No", "Yes")))


# Summaries ---------------------------------------------------------------

## Scotland Summary
scot_summary <- scot_data %>%
  distinct(subjid) %>%
  summarise(
    total_cases = n(),
    confirmed_cases = sum(subjid %in% corna_confirmed),
    suspected_cases = sum(subjid %in% corna_suspected),
    unknown_cases = sum(subjid %in% corna_unknown),
    travelled_14 = sum(subjid %in% travelled),
    deaths = sum(subjid %in% died)
  )

## Health Board Summary
hb_summary <- scot_data %>%
  group_by(HB2019Name) %>%
  distinct(subjid, .keep_all = T) %>%
  summarise(
    total_cases = n(),
    n_hosptials = n_distinct(hospid),
    confirmed_cases = sum(subjid %in% corna_confirmed),
    suspected_cases = sum(subjid %in% corna_suspected),
    unknown_cases = sum(subjid %in% corna_unknown),
    travelled_14 = sum(subjid %in% travelled),
    deaths = sum(subjid %in% died)
  )

## Excluded patients who have travelled 14 days previously

hb_summary_ex_travelled <- scot_data %>%
  group_by(HB2019Name) %>%
  filter(!subjid %in% travelled & !(is.na(HB2019Name))) %>%
  distinct(subjid, .keep_all = T) %>%
  summarise(
    total_cases = n(),
    n_hosptials = n_distinct(hospid),
    confirmed_cases = sum(subjid %in% corna_confirmed),
    suspected_cases = sum(subjid %in% corna_suspected),
    unknown_cases = sum(subjid %in% corna_unknown),
    deaths = sum(subjid %in% died)
  )

## Age Summary

age_summary <- pop_data %>% 
  group_by(age) %>% 
  summarise(total_cases = n()) %>%
  mutate(percent= paste(round(total_cases/sum(total_cases)*100,1),"%"))


## Ethnicity analysis 

ethnicity_data <- pop_data %>%
  group_by(ethnicity) %>%
  filter(!subjid %in% travelled)  %>% 
  summarise(total_cases = n()) %>%
  mutate(percent= paste(round(total_cases/sum(total_cases)*100,1),"%"))

## Admission date analysis 
## Analysing by onset date of first/earliest symptom 
## Week 1 - 23/03/2020 

admission_data <- pop_data %>% 
  filter(admission <= today())  %>% 
  mutate(admissionweek = 
           if_else(admission <"2020-03-30", "Week 1", if_else(admission < "2020-04-06", "Week 2",
                                                              if_else(admission < "2020-04-13", "Week 3",  if_else(admission < "2020-04-20", "Week 4",if_else(admission < "2020-04-27", "Week 5", if_else(admission < "2020-05-04", "Week 6","NA" ))))))) %>% 
  group_by(admissionweek) %>% 
  summarise(total_cases = n()) %>%
  mutate(percent= paste(round(total_cases/sum(total_cases)*100,1),"%"))

## Pregnancy Analysis 

pregnancy_data <- pop_data %>%
  group_by(pregnancy) %>%
  summarise(total_cases = n()) 



# write.xlsx(
#   x = list(
#     "Scotland Summary" = scot_data,
#     "Health Boards" = hb_summary,
#     "Health Boards ex travelled" = hb_summary_ex_travelled, "Age Summary ex travelled" = age_summary_ex_travelled,
#     "Ethnicity Summary ex travelled" = ethnicity_ex_travelled
#   ),
#   file = "initial stats - 27-04-2020.xlsx"
# )


# HB summary table --------------------------------------------------------
hb_summary_ex_travelled %>%
  gt() %>%
  tab_header(
    title = "Summary of NHS Scotland Health Boards",
    subtitle = str_glue("Excludes {n_travelled} who travelled in the 14 days prior to admission",
      n_travelled = length(travelled)
    )
  ) %>%
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


# Plots -------------------------------------------------------------------

# Population pyramid
ggplot(pop_data, aes(x = age, fill = sex, alpha = died)) +
  geom_bar(data = filter(pop_data, sex == "Female")) +
  geom_bar(data = filter(pop_data, sex == "Male"), aes(y = ..count.. * (-1))) +
  scale_fill_discrete("Sex at birth") +
  scale_alpha_ordinal("Died", range = c(0.3, 1)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylim(-100, 100) +
  xlab("Age") +
  ylab("Count of subjects") +
  ggtitle("Population pyramid of ISARIC subjects from Scottish hospitals",
    subtitle = str_glue("Excludes {n_travelled} who travelled in the 14 days prior to admission",
      n_travelled = length(travelled)
    )
  )
