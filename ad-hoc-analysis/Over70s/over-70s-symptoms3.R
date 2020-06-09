# setup -------------------------------------------------------------------

source("extract-data/00_setup-environment.R")

# Read data ---------------------------------------------------------------

if (date(latest_extract_date()) < (today() - days(3))) {
  message("local extract is older than RAPID-ECOSS file")
  source("extract-data/01_get-scottish-data.R")
  source("extract-data/02_clean-data.R")
}

scot_data <- here("data", str_glue("{date}_scot-data-clean.rds", date = latest_extract_date())) %>%
  read_rds()


# prep data ---------------------------------------------------------------

topline <- scot_data %>%
  select(
    subjid,
    age,
    hostdat,
    dsstdat,
    corna_mbcat,
    fever_ceoccur_v2:bleed_ceoccur_v2,
    -ageusia_ceoccur_v2,
    ageusia_ceoccur_v2,
    anosmia_ceoccur_v2
  ) %>% 
  rename(
    "Fever" = fever_ceoccur_v2,
    "Cough" = cough_ceoccur_v2,
    "Cough (sputum)" = coughsput_ceoccur_v2,
    "Cough (blood)" = coughhb_ceoccur_v2,
    "Sore throat" = sorethroat_ceoccur_v2,
    "Runny nose" = runnynose_ceoccur_v2,
    "Ear pain" = earpain_ceoccur_v2,
    "Wheeze" = wheeze_ceoccur_v2,
    "Chest pain" = chestpain_ceoccur_v2,
    "Muscle ache" = myalgia_ceoccur_v2,
    "Joint pain" = jointpain_ceoccur_v2,
    "Fatigue" = fatigue_ceoccur_v2,
    "Shortness of breath" = shortbreath_ceoccur_v2,
    "Lower chest wall in-drawing" = lowerchest_ceoccur_v2,
    "Headache" = headache_ceoccur_v2,
    "Confusion" = confusion_ceoccur_v2,
    "Seizures" = seizures_cecoccur_v2,
    "Abdominal pain" = abdopain_ceoccur_v2,
    "Nausea/vomiting" = vomit_ceoccur_v2,
    "Diarrhoea" = diarrhoea_ceoccur_v2,
    "Conjunctivitis" = conjunct_ceoccur_v2,
    "Skin rash" = rash_ceoccur_v2,
    "Skin ulcers" = skinulcers_ceoccur_v2,
    "Lymphadenopathy" = lymp_ceoccur_v2,
    "Bleeding (Haemorrhage)" = bleed_ceoccur_v2,
    "Ageusia" = ageusia_ceoccur_v2,
    "Anosmia" = anosmia_ceoccur_v2
  ) %>%
  group_by(subjid) %>%
  summarise_all(~ first(na.omit(.))) %>%
  filter(age >= 70, !is.na(hostdat)) %>%
  mutate(admission = if_else(hostdat < ymd("2020-04-30"),
    "Before 30th April",
    "On or after 30th April"
  ) %>%
    as_factor()) %>%
  mutate_at(
    vars(Fever:Anosmia),
    ~ factor(.) %>%
      fct_relabel(~ c("YES", "NO", "Unknown")) %>%
      fct_explicit_na(na_level = "Unknown")
  ) %>% 
  left_join(
    filter_at(., vars(Fever:Anosmia), all_vars(. != "YES")) %>%
    mutate("Asymptomatic (inc Unknown)" = "YES") %>%
    select(subjid, "Asymptomatic (inc Unknown)"),
  by = "subjid"
  ) %>%
  left_join(
    filter_at(., vars(Fever:Anosmia), all_vars(. == "NO")) %>%
      mutate("Asymptomatic (all No)" = "YES") %>%
      select(subjid, "Asymptomatic (all No)"),
    by = "subjid"
  ) %>%
  mutate_at(
    vars("Asymptomatic (inc Unknown)", "Asymptomatic (all No)"),
    ~ recode(., .missing = "NO") %>% 
      factor(levels = c("YES", "NO"))
  )

n_before <- topline %>% filter(admission == "Before 30th April") %>% nrow()
n_after <- topline %>% filter(admission == "On or after 30th April") %>% nrow()

n_before_pos <- topline %>% filter(admission == "Before 30th April") %>% filter(corna_mbcat == "YES - Confirmed") %>% nrow()
n_after_pos <- topline %>% filter(admission == "On or after 30th April") %>% filter(corna_mbcat == "YES - Confirmed") %>% nrow()

n_before_unknown <- topline %>% filter(admission == "Before 30th April") %>% filter_at(vars(Fever:Anosmia), all_vars(. == "Unknown")) %>% nrow()
n_after_unknown <- topline %>% filter(admission == "On or after 30th April") %>% filter_at(vars(Fever:Anosmia), all_vars(. == "Unknown")) %>% nrow()


earliest_recruitment <- topline %>% filter(dsstdat >= dmy(01012020)) %>% pull(dsstdat) %>% min()
latest_recruitment <- topline %>% pull(dsstdat) %>% max(na.rm = TRUE)

ggplot(topline, aes(x = hostdat)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = ymd("2020-04-30")) +
  theme_minimal()

ggplot(topline %>% 
         select(dsstdat, Ageusia, Anosmia) %>% 
         pivot_longer(cols = c(Ageusia, Anosmia),
                      names_to = "Symptom", 
                      values_to = "Status") %>% 
         filter(Status != "Unknown"), 
       aes(x = dsstdat,
           fill = Symptom)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = ymd("2020-04-30")) +
  theme_minimal()

ggplot(topline %>% filter(hostdat >= dmy(01012020)), aes(x = hostdat)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = ymd("2020-04-30")) +
  theme_minimal()


symptom_data <- topline %>%
  pivot_longer(
    cols = c(Fever:Anosmia, "Asymptomatic (inc Unknown)", "Asymptomatic (all No)"),
    names_to = "Symptom",
    values_to = "Status",
    values_drop_na = TRUE
  ) %>% 
  mutate(Status = recode(Status, "YES" = "Yes", "NO" = "No")) %>% 
  group_by(admission, Symptom) %>%
  count(Status) %>%
  ungroup()

# Fix for Anosmia
if ((symptom_data %>% filter(admission == "Before 30th April", Symptom == "Anosmia", Status == "Yes") %>% nrow()) == 0) {
  symptom_data <- symptom_data %>% 
    add_row(admission = "Before 30th April", Symptom = "Anosmia", Status = "Yes", n = 0)
}

symp_data_levels_order <- symptom_data %>%
  filter(
    Status == "Yes",
    admission == "Before 30th April"
  ) %>%
  mutate(order = if_else(Symptom %in% c("Asymptomatic (inc Unknown)", "Asymptomatic (all No)"),
    1, 0
  )) %>%
  arrange(order, desc(n)) %>%
  pull(Symptom)


cluster_data <- topline %>%
  left_join(
    filter_at(., vars("Ear pain", "Confusion", "Seizures", "Skin rash", "Skin ulcers", "Conjunctivitis", "Bleeding (Haemorrhage)", "Headache"), any_vars(. == "YES")) %>%
      mutate(Neurocutaneous = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Neurocutaneous)
  ) %>% 
  left_join(
    filter_at(., vars("Fatigue", "Muscle ache", "Lymphadenopathy", "Fever", "Joint pain"), any_vars(. == "YES")) %>%
      mutate(Generalised = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Generalised)
  ) %>% 
  left_join(
    filter_at(., vars("Diarrhoea", "Nausea/vomiting", "Abdominal pain"), any_vars(. == "YES")) %>%
      mutate(Gastrointestinal = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Gastrointestinal)
  ) %>%
  left_join(
    filter_at(., vars("Cough", "Cough (blood)", "Cough (sputum)", "Wheeze", "Shortness of breath", "Sore throat", "Chest pain", "Lower chest wall in-drawing", "Runny nose"), any_vars(. == "YES")) %>%
      mutate(Respiratory = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Respiratory)
  ) %>%
  left_join(
    filter_at(., vars("Cough", "Cough (blood)", "Cough (sputum)", "Fever", "Ageusia", "Anosmia"), any_vars(. == "YES")) %>%
      mutate(Key = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Key)
  ) %>% 
  select(subjid, admission, "Asymptomatic (inc Unknown)":Key) %>%
  mutate_at(
    vars(-subjid, -admission),
    ~ replace_na(., "NO")
  ) %>%
  pivot_longer(
    cols = c(-subjid, -admission),
    names_to = "Cluster",
    values_to = "Status",
    values_drop_na = TRUE
  ) %>%
  mutate(Status = recode(Status, "YES" = "Yes", "NO" = "No")) %>%
  group_by(admission, Cluster) %>%
  count(Status) %>%
  ungroup()


cluster_data_levels_order <-
  cluster_data %>%
  filter(
    Status == "Yes",
    admission == "Before 30th April"
  ) %>%
  mutate(order = if_else(Cluster %in% c("Asymptomatic (inc Unknown)", "Asymptomatic (all No)"),
                         1, 0)
         ) %>%
  arrange(order, desc(n)) %>%
  pull(Cluster)




# Tables ------------------------------------------------------------------

tbl_summary <- scot_data %>%
  filter(!is.na(hostdat)) %>%
  group_by(subjid) %>%
  mutate(
    over70 = first(na.omit(age)) >= 70,
    adm_30apr = first(na.omit(hostdat) >= dmy(30042020))
  ) %>%
  mutate(
    status =
      case_when(
        over70 & adm_30apr ~ "over70_after30apr",
        over70 & !adm_30apr ~ "over70_before30apr",
        TRUE ~ "other_patients"
      )
  ) %>%
  group_by(hb_name, status) %>%
  summarise(n = n_distinct(subjid)) %>%
  ungroup() %>%
  pivot_wider(names_from = status, values_from = n, values_fill = 0) %>%
  mutate(total_patients = rowSums(select_if(., is.numeric))) %>%
  bind_rows((.) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(hb_name = "Scotland")) %>%
  rename(
    "Health Board of treatment" = hb_name,
    "70+ Before April 30" = "over70_before30apr",
    "70+ After April 30" = "over70_after30apr",
    "All other patients" = other_patients,
    "Total patients" = total_patients
  )

tbl_all_symptoms <- symptom_data %>%
  mutate(Status = recode(Status, "Unknown" = "No")) %>%
  group_by(admission, Symptom, Status) %>%
  summarise(n = sum(n)) %>% 
  mutate(total = if_else(admission == "Before 30th April", n_before, n_after)) %>% 
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
  mutate(Significant = if_else(prop.test(
    x = c(`n_Before 30th April`, `n_On or after 30th April`),
    n = c(n_before, n_after)
  )$p.value < 0.5,
  "Yes",
  "No"
  )) %>%
  mutate(Direction = if_else(Significant == "Yes",
    if_else(`prop_Before 30th April` > `prop_On or after 30th April`,
      "Decrease",
      "Increase"
    ),
    NA_character_
  )) %>%
  filter(Status == "Yes") %>% 
  select(-Status) %>% 
  arrange(desc(Significant), desc(`n_Before 30th April`))

tbl_all_symptoms_ex_unknown <- symptom_data %>%
  filter(Status != "Unknown") %>% 
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
  mutate(Significant = if_else(prop.test(
    x = c(`n_Before 30th April`, `n_On or after 30th April`),
    n = c(n_before, n_after)
  )$p.value < 0.5,
  "Yes",
  "No"
  )) %>%
  mutate(Direction = if_else(Significant == "Yes",
                             if_else(`prop_Before 30th April` > `prop_On or after 30th April`,
                                     "Decrease",
                                     "Increase"
                             ),
                             NA_character_
  )) %>%
  filter(Status == "Yes") %>% 
  select(-Status) %>% 
  arrange(desc(Significant), desc(`n_Before 30th April`))

tbl_clusters <- cluster_data %>%
  mutate(total = if_else(admission == "Before 30th April", n_before, n_after)) %>% 
  group_by(admission, Cluster, Status) %>%
  mutate(prop = n / total) %>% 
  ungroup() %>%
  pivot_wider(
    names_from = admission,
    values_from = c(n, total, prop),
    values_fill = 0
  ) %>%
  filter(`prop_On or after 30th April` != 0) %>%
  rowwise() %>%
  mutate(Significant = if_else(prop.test(
    x = c(`n_Before 30th April`, `n_On or after 30th April`),
    n = c(n_before, n_after)
  )$p.value < 0.5,
  "Yes",
  "No"
  )) %>%
  mutate(Direction = if_else(Significant == "Yes",
                             if_else(`prop_Before 30th April` > `prop_On or after 30th April`,
                                     "Decrease",
                                     "Increase"
                             ),
                             NA_character_
  )) %>%
  filter(Status == "Yes") %>% 
  select(-Status) %>% 
  arrange(desc(Significant), desc(`n_Before 30th April`))


# Plots -------------------------------------------------------------------

plt_Significant_symptoms <- symptom_data %>%
  # Status a factor ordered by levels == Yes value
  mutate(
    Symptom = factor(Symptom, symp_data_levels_order),
    Status = factor(Status, c("Yes", "Unknown", "No")),
  ) %>% 
  filter(Symptom %in% (tbl_all_symptoms %>% filter(Significant == "Yes") %>% pull(Symptom))) %>% 
  ggplot(aes(x = admission, y = n, fill = fct_rev(Status))) +
  geom_col(position = "fill") +
  theme_minimal() +
  theme(legend.position = "top") +
  ylab("Proportion") +
  scale_x_discrete("Admission period") +
  scale_fill_brewer("") +
  scale_y_continuous("Patients with symptom (%)", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~Symptom, labeller = labeller(Symptom = label_wrap_gen(10)))

plt_clusters <- cluster_data %>%
  mutate(Cluster = factor(Cluster, cluster_data_levels_order)) %>% 
  ggplot(aes(x = admission, y = n, fill = fct_rev(Status))) +
  geom_col(position = "fill") +
  theme_minimal() +
  theme(legend.position = "top") +
  ylab("Proportion") +
  scale_x_discrete("Admission period") +
  scale_fill_manual("", values = (c("#deebf7", "#3182bd"))) +
  scale_y_continuous("Patients with any symptom \nin cluster (%)", labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~Cluster, labeller = labeller(Cluster = label_wrap_gen(10)))



# Output ------------------------------------------------------------------
library(writexl)

tbl_overview <- tibble(
  Info = c("Extract Date", "Earliest admission", "Latest admission", "Admission 1st Quartile", "Admission 3rd Quartile"),
  Value = c(date(latest_extract_date()), min(topline$hostdat), max(topline$hostdat), summary(topline$hostdat)[2], summary(topline$hostdat)[5])
)

tables <- list(
  "Notes" = tbl_overview,
  "Summary" = tbl_summary,
  "All symptoms" = tbl_all_symptoms,
  "All symptoms (ex unknown)" = tbl_all_symptoms_ex_unknown,
  "Symptom clusters" = tbl_clusters
)

# Tidy up table output to percentags
tables <- tables %>% 
  map(~ mutate_at(.x, vars(starts_with("prop_")), ~ scales::percent(.)) %>% 
        rename_at(vars(starts_with("n_")), ~ str_replace(., "^n_(.+?)$", "Yes \\1")) %>% 
        rename_at(vars(starts_with("total_")), ~ str_replace(., "^total_(.+?)$", "Total \\1")) %>% 
        rename_at(vars(starts_with("prop_")), ~ str_replace(., "^prop_(.+?)$", "Percentage \\1")) %>% 
        rename_at(vars(matches("Significant")), ~ "Significant Change?") %>% 
        rename_at(vars(matches("Direction")), ~ "Direction of change (if significant)")
      )

write_xlsx(x = tables,
           path = str_glue("output/{date}_over70_symptoms.xlsx", 
                           date = date(latest_extract_date())))

ggsave(str_glue("output/{date}_over70_Significant_symptoms.png",
                date = date(latest_extract_date())),
       plt_Significant_symptoms,
       width = 12,
       height = 9)

ggsave(str_glue("output/{date}_over70_clusters.png",
                date = date(latest_extract_date())),
       plt_clusters,
       width = 9,
       height = 9)

       