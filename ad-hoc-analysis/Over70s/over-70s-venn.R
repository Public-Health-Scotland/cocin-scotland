venn_data <- topline %>%
  select(-age, -hostdat) %>%
  left_join(
    filter_at(., vars("Ear pain", "Confusion", "Seizures", "Skin rash", "Skin ulcers", "Conjunctivitis", "Bleeding (Haemorrhage)", "Headache"), any_vars(. == "YES")) %>%
      mutate(Neurocutaneous = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Neurocutaneous)
  ) %>%
  left_join(
    filter_at(., vars("Fatigue", "Muscle ache", "Lymphadenopathy", "Fever"), any_vars(. == "YES")) %>%
      mutate(Generalised = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Generalised)
  ) %>%
  left_join(
    filter_at(., vars("Diarrhoea", "Nausa/vomiting", "Abdominal pain", "Joint pain"), any_vars(. == "YES")) %>%
      mutate(Gastrointestinal = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Gastrointestinal)
  ) %>%
  left_join(
    filter_at(., vars("Cough", "Cough (blood)", "Cough (sputum)", "Wheeze", "Shortness of breath", "Sore throat", "Chest pain", "Lower chest wall indrawing", "Runny nose"), any_vars(. == "YES")) %>%
      mutate(Respiratory = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Respiratory)
  ) %>%
  left_join(
    filter_at(., vars("Cough", "Cough (blood)", "Cough (sputum)", "Fever", "Ageusia", "Anosmia"), any_vars(. == "YES")) %>%
      mutate(Key = "YES" %>% factor(levels = c("YES", "NO"))) %>%
      select(subjid, Key)
  )

fever_group_before <- venn_data %>%
  filter(Fever == "YES") %>%
  filter(admission == "Before 30th April") %>%
  pull(subjid)

respiratory_group_before <- venn_data %>%
  filter(Respiratory == "YES") %>%
  filter(admission == "Before 30th April") %>%
  pull(subjid)

gastro_group_before <- venn_data %>%
  filter(Gastrointestinal == "YES") %>%
  filter(admission == "Before 30th April") %>%
  pull(subjid)


fever_group_after <- venn_data %>%
  filter(Fever == "YES") %>%
  filter(admission == "On or after 30th April") %>%
  pull(subjid)

respiratory_group_after <- venn_data %>%
  filter(Respiratory == "YES") %>%
  filter(admission == "On or after 30th April") %>%
  pull(subjid)

gastro_group_after <- venn_data %>%
  filter(Gastrointestinal == "YES") %>%
  filter(admission == "On or after 30th April") %>%
  pull(subjid)

library(ggVennDiagram)

venn_before <- ggVennDiagram(list(
  "Gastrointestinal \nCluster" = gastro_group_before,
  Fever = fever_group_before,
  "Respiratory \nCluster" = respiratory_group_before
), n.sides = 10000
) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, before April 30th (N = {n_before})"))

venn_after <- ggVennDiagram(list(
  "Gastrointestinal \nCluster" = gastro_group_after,
  Fever = fever_group_after,
  "Respiratory \nCluster" = respiratory_group_after
), n.sides = 10000
) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, after April 30th (N = {n_after})"))


