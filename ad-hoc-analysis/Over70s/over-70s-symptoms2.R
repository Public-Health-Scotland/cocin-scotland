### 0 - Setup environment ----
source("extract-data/00_setup-environment.R")

scot_data <- str_glue("data/{date}_scot-data-clean.rds", date = latest_extract_date()) %>%
  read_rds()

topline <- scot_data %>%
  select(subjid, age, cestdat, fever_ceoccur_v2:bleed_ceoccur_v2, -ageusia_ceoccur_v2) %>%
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
    "Lower chest wall indrawing" = lowerchest_ceoccur_v2,
    "Headache" = headache_ceoccur_v2,
    "Confusion" = confusion_ceoccur_v2,
    "Seizures" = seizures_cecoccur_v2,
    "Abdominal pain" = abdopain_ceoccur_v2,
    "Nausa/vomiting" = vomit_ceoccur_v2,
    "Diarrhoea" = diarrhoea_ceoccur_v2,
    "Conjunctivitis" = conjunct_ceoccur_v2,
    "Skin rash" = rash_ceoccur_v2,
    "Skin ulcers" = skinulcers_ceoccur_v2,
    "Lymphadenopathy" = lymp_ceoccur_v2,
    "Bleeding (Haemorrhage)" = bleed_ceoccur_v2
  ) %>%
  group_by(subjid) %>%
  summarise_all(~ first(na.omit(.))) %>%
  filter(age >= 70, !is.na(cestdat)) %>%
  mutate(admission = if_else(cestdat < "2020-04-30",
    "Before 30th April",
    "On or after 30th April"
  ) %>%
    as_factor()) %>%
  left_join(scot_data %>%
    filter_at(vars(fever_ceoccur_v2:bleed_ceoccur_v2), any_vars(. == "YES")) %>%
    mutate(any_symptoms = "YES") %>%
    select(subjid, any_symptoms),
  by = "subjid"
  ) %>%
  mutate(any_symptoms = recode(any_symptoms, .missing = "NO")) %>%
  rename("Any Symptoms" = any_symptoms)


symptom_data <- topline %>%
  pivot_longer(
    cols = c(Fever:"Bleeding (Haemorrhage)", "Any Symptoms"),
    names_to = "Symptom",
    values_to = "Status",
    values_drop_na = TRUE
  ) %>%
  mutate(Status = recode(Status, "YES" = "Yes", "NO" = "No")) %>%
  group_by(admission, Symptom) %>%
  count(Status) %>%
  ungroup()

symp_data_levels_order <-
  symptom_data %>%
  filter(
    Status == "Yes",
    admission == "Before 30th April"
  ) %>%
  arrange(desc(n)) %>%
  pull(Symptom)


# Quick Graph
sympt_bar_chart <- symptom_data %>%
  mutate(Symptom = factor(Symptom, symp_data_levels_order)) %>%
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

# UoE symptom plot
uoe_sympt_plot <- symptom_data %>%
  # Status a factor ordered by levels == Yes value
  mutate(
    Symptom = factor(Symptom, symp_data_levels_order),
    Status = factor(Status, c("Yes", "Unknown", "No"))
  ) %>%
  ggplot(aes(x = fct_rev(Symptom), y = n, fill = fct_rev(Status))) +
  geom_col(position = "fill") +
  xlab("") +
  scale_y_continuous("Proportion of patients with symptom (%)", labels = scales::percent) +
  scale_fill_manual("", values = c("dark grey", "light grey", "black")) +
  # scale_fill_brewer("", palette = "Paired", breaks = rev, direction = -1) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_grid(cols = vars(admission))

# UoE corr plot
symp_corr_before <- topline %>%
  filter(str_detect(admission, "Before")) %>%
  select(-subjid, -age, -cestdat, -admission) %>%
  mutate_all(as_factor) %>%
  mutate_all(fct_recode, NULL = "Unknown") %>%
  mutate_all(as.numeric) %>%
  cor(use = "pairwise.complete.obs") %>%
  .[, colSums(is.na(.)) != nrow(.)] %>%
  .[rowSums(is.na(.)) != ncol(.), ]

symp_corr_after <- topline %>%
  filter(str_detect(admission, "On")) %>%
  select(-subjid, -age, -cestdat, -admission) %>%
  mutate_all(as_factor) %>%
  mutate_all(fct_recode, NULL = "Unknown") %>%
  mutate_all(as.numeric) %>%
  cor(use = "pairwise.complete.obs") %>%
  .[, colSums(is.na(.)) != nrow(.)] %>%
  .[rowSums(is.na(.)) != ncol(.), ]
symp_corr_after[is.na(symp_corr_after)] <- 0

library(ggcorrplot)
before_cor_plot <- symp_corr_before %>%
  ggcorrplot(
    hc.order = TRUE,
    outline.color = "white",
    colors = c("#6D9EC1", "white", "#E46726")
  ) +
  ggtitle("Before 30th April")

after_cor_plot <- symp_corr_after %>%
  ggcorrplot(
    hc.order = TRUE,
    outline.color = "white",
    colors = c("#6D9EC1", "white", "#E46726")
  ) +
  ggtitle("On or after 30th April")

library(gridExtra)
ggsave("output/over70_symptom_plots.pdf", marrangeGrob(
  grobs = list(
    sympt_bar_chart,
    uoe_sympt_plot,
    before_cor_plot,
    after_cor_plot
  ),
  nrow = 1,
  ncol = 1
),
width = 10, height = 10
)
