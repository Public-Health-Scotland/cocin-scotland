## Scottish Data Analysis 

library(openxlsx)
##scot_data <- read_rds("scottish_data_prepped.rds.gz")


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


## Outputs

## Scotland Summary 
scot_summary <- scot_data %>% 
  distinct(subjid) %>% 
  summarise(total_cases = n(),
            confirmed_cases = sum(subjid %in% corna_confirmed),
            suspected_cases = sum(subjid %in% corna_suspected),
            unknown_cases = sum(subjid %in% corna_unknown),
            travelled_14 = sum(subjid %in% travelled),
            deaths = sum(subjid %in% died))  

## Health Board Summary 
hb_summary <- scot_data %>% 
  group_by(HB2019Name) %>% 
  distinct(subjid, .keep_all = T) %>% 
  summarise(total_cases = n(),
            n_hosptials = n_distinct(hospid),
            confirmed_cases = sum(subjid %in% corna_confirmed),
            suspected_cases = sum(subjid %in% corna_suspected),
            unknown_cases = sum(subjid %in% corna_unknown),
            travelled_14 = sum(subjid %in% travelled),
            deaths = sum(subjid %in% died))

## Excluded patients who have travelled 14 days previously 

hb_summary_ex_travelled <- scot_data %>% 
  group_by(HB2019Name) %>% 
  filter(!subjid %in% travelled & !(is.na(HB2019Name))) %>% 
  distinct(subjid, .keep_all = T) %>% 
  summarise(total_cases = n(),
            n_hosptials = n_distinct(hospid),
            confirmed_cases = sum(subjid %in% corna_confirmed),
            suspected_cases = sum(subjid %in% corna_suspected),
            unknown_cases = sum(subjid %in% corna_unknown),
            deaths = sum(subjid %in% died))

## Addition of age summary and ethnicity using top line data 

## Age Summary 

age_summary_ex_travelled <- topline %>% 
  group_by(age.factor) %>% 
  filter(!subjid %in% travelled) %>% 
  ##distinct(subjid, .keep_all = T) %>% 
  summarise(total_cases = n(),
            confirmed_cases = sum(subjid %in% corna_confirmed),
            suspected_cases = sum(subjid %in% corna_suspected),
            unknown_cases = sum(subjid %in% corna_unknown),
            deaths = sum(subjid %in% died))

## Ethnicity 

ethnicity_ex_travelled <- topline %>%
  group_by(ethnicity) %>%
  filter(!subjid %in% travelled)  %>% 
  ##distinct(subjid, .keep_all = T) %>% 
  summarise(total_cases = n(),
            confirmed_cases = sum(subjid %in% corna_confirmed),
            suspected_cases = sum(subjid %in% corna_suspected),
            unknown_cases = sum(subjid %in% corna_unknown),
            deaths = sum(subjid %in% died))

## Admission date analysis 
## Analysing by onset date of first/earliest symptom 
## Week 1 - 23/03/2020 

admission_data <- topline %>% 
  filter(!is.na(cestdat) & year(cestdat) == 2020 & cestdat <= today())  %>% 
  mutate(admissionweek = 
           if_else(cestdat <"2020-03-30", "Week 1", if_else(cestdat < "2020-04-06", "Week 2",
                  if_else(cestdat < "2020-04-13", "Week 3",  if_else(cestdat < "2020-04-20", "Week 4",if_else(cestdat < "2020-04-27", "Week 5", "Error" )))))) %>% 
  group_by(admissionweek,hospid) %>% 
  summarise(total_cases = n(),
            age = first(age.factor),
            sex = first(sex))


##write.xlsx(
  ##x = list("Scotland Summary" = scot_data,
    ##   "Health Boards" = hb_summary,
      ## "Health Boards ex travelled" = hb_summary_ex_travelled, "Age Summary ex travelled" = age_summary_ex_travelled,
       ##"Ethnicity Summary ex travelled" = ethnicity_ex_travelled),
  ##file = "initial stats - 27-04-2020.xlsx"
)

# Plots
library(ggplot2)

## Breakdown of geder, age and death 
pop_data <- scot_data %>% 
  filter(!is.na(age.factor), !is.na(sex)) %>% 
  group_by(subjid) %>% 
  mutate(died = if_else(subjid %in% died, 1, 0)) %>% 
  summarise(age = first(age.factor),
            sex = first(sex),
            died = max(died)) %>% 
  mutate(died = as_factor(died))

# Population pyramid
ggplot(pop_data, aes(x = age, colour = sex, alpha = died)) +
  geom_bar(data = filter(pop_data, sex == "Female")) +
  geom_bar(data = filter(pop_data, sex == "Male"), aes(y = ..count..*(-1))) +
  coord_flip() +
  theme_minimal()







