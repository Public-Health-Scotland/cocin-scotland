### 0 - Setup environment ----
source("extract-data/00_setup-environment.R")

scot_data <- str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date()) %>%
  read_rds()

topline <- scot_data %>%
  select(subjid, age, cestdat, fever_ceoccur_v2:bleed_ceoccur_v2, -ageusia_ceoccur_v2) %>%
  rename(
    `Fever` = fever_ceoccur_v2,
    `Cough` = cough_ceoccur_v2,
    `Cough (sputum)` = coughsput_ceoccur_v2,
    `Cough (blood)` = coughhb_ceoccur_v2,
    `Sore throat` = sorethroat_ceoccur_v2,
    `Runny nose` = runnynose_ceoccur_v2,
    `Ear pain` = earpain_ceoccur_v2,
    `Wheeze` = wheeze_ceoccur_v2,
    `Chest pain` = chestpain_ceoccur_v2,
    `Muscle ache` = myalgia_ceoccur_v2,
    `Joint pain` = jointpain_ceoccur_v2,
    `Fatigue` = fatigue_ceoccur_v2,
    `Shortness of breath` = shortbreath_ceoccur_v2,
    `Lower chest wall indrawing` = lowerchest_ceoccur_v2,
    `Headache` = headache_ceoccur_v2,
    `Confusion` = confusion_ceoccur_v2,
    `Seizures` = seizures_cecoccur_v2,
    `Abdominal pain` = abdopain_ceoccur_v2,
    `Nausa/vomiting` = vomit_ceoccur_v2,
    `Diarrhoea` = diarrhoea_ceoccur_v2,
    `Conjunctivitis` = conjunct_ceoccur_v2,
    `Skin rash` = rash_ceoccur_v2,
    `Skin ulcers` = skinulcers_ceoccur_v2,
    `Lymphadenopathy` = lymp_ceoccur_v2,
    `Bleeding (Haemorrhage)` = bleed_ceoccur_v2
  ) %>%
  group_by(subjid) %>%
  summarise_all(~ first(na.omit(.))) %>%
  filter(age >= 70, !is.na(cestdat)) %>%
  mutate(admission = if_else(cestdat < "2020-04-30",
    "Before 30th April",
    "On or after 30th April"
  ) %>%
    as_factor())


symptom_data <- topline %>%
  pivot_longer(
    cols = `Fever`:`Bleeding (Haemorrhage)`,
    names_to = "Symptom",
    values_to = "Status",
    values_drop_na = TRUE
  ) %>%
  mutate(Status = recode(Status, "YES" = "Yes", "NO" = "No")) %>%
  group_by(admission, Symptom) %>%
  count(Status)

# Quick Graph
symptom_data %>%
  ggplot(aes(x = Symptom, y = n, fill = Status)) +
  geom_col(position = "fill") +
  theme_minimal() +
  ylab("Proportion") +
  scale_fill_brewer() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid("admission")

# Quick Table
symptom_data %>%
  group_by(admission, Symptom) %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  group_by(admission, Symptom, Status) %>%
  mutate(prop = n / total) %>%
  ungroup() %>%
  pivot_wider(
    names_from = admission,
    values_from = c(n, total, prop),
    values_fill = 0
  ) %>%
  filter(`prop_On or after 30th April` != 0) %>%
  rowwise() %>%
  mutate(significant = if_else(prop.test(
    x = c(`n_Before 30th April`, `n_On or after 30th April`),
    n = c(`total_Before 30th April`, `total_On or after 30th April`)
  )$p.value < 0.5,
  "Yes",
  "No"
  )) %>%
  mutate(direction = if_else(significant == "Yes",
    if_else(`prop_Before 30th April` > `prop_On or after 30th April`,
      "decrease",
      "increase"
    ),
    NA_character_
  )) %>%
  select(-starts_with("total")) %>%
  write_csv(str_glue("output/{date}_over70_symptom_comparison.csv",
    date = latest_extract_date()
  ))
