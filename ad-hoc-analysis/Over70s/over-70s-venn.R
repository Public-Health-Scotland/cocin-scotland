venn_data <- topline %>%
  select(-age, -hostdat, -dsstdat, -corna_mbcat) %>%
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
  )

before_lists <- venn_data %>%
  filter(admission == "Before 30th April") %>%
  select(-admission, -"No symptoms presenting (inc Unknown)", -"No symptoms presenting (all No)") %>%
  pivot_longer(cols = c(Fever:Key), names_to = "Group", values_to = "Status") %>%
  filter(Status == "YES") %>%
  pivot_wider(names_from = "Group", values_from = "subjid") %>%
  select(-Status)

after_lists <- venn_data %>%
  filter(admission == "On or after 30th April 2020") %>%
  select(-admission, -"No symptoms presenting (inc Unknown)", -"No symptoms presenting (all No)") %>%
  pivot_longer(cols = c(Fever:Key), names_to = "Group", values_to = "Status") %>%
  filter(Status == "YES") %>%
  pivot_wider(names_from = "Group", values_from = "subjid") %>%
  select(-Status)


library(ggVennDiagram)

# Gastrointestinal
venn_gastro_before <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(before_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(before_lists$Key),
  "Gastrointestinal \nCluster" = unlist(before_lists$Gastrointestinal)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, before 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(before_lists$Respiratory), 
                                     unlist(before_lists$Key), 
                                     unlist(before_lists$Gastrointestinal)) %>% 
                     unique() %>% 
                     length()))

venn_gastro_after <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(after_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(after_lists$Key),
  "Gastrointestinal \nCluster" = unlist(after_lists$Gastrointestinal)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, after 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(after_lists$Respiratory), 
                                     unlist(after_lists$Key), 
                                     unlist(after_lists$Gastrointestinal)) %>% 
                     unique() %>% 
                     length()))

# Neurocutaneous
venn_neuro_before <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(before_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(before_lists$Key),
  "Neurocutaneous \nCluster" = unlist(before_lists$Neurocutaneous)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, before 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(before_lists$Respiratory), 
                                     unlist(before_lists$Key), 
                                     unlist(before_lists$Neurocutaneous)) %>% 
                     unique() %>% 
                     length()))

venn_neuro_after <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(after_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(after_lists$Key),
  "Neurocutaneous \nCluster" = unlist(after_lists$Neurocutaneous)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, after 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(after_lists$Respiratory), 
                                     unlist(after_lists$Key), 
                                     unlist(after_lists$Neurocutaneous)) %>% 
                     unique() %>% 
                     length()))

# Generalised
venn_general_before <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(before_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(before_lists$Key),
  "Generalised \nCluster" = unlist(before_lists$Generalised)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, before 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(before_lists$Respiratory), 
                                     unlist(before_lists$Key), 
                                     unlist(before_lists$Generalised)) %>% 
                     unique() %>% 
                     length()))

venn_general_after <- ggVennDiagram(list(
  "Respiratory \nCluster" = unlist(after_lists$Respiratory),
  "Key Symptoms \nCluster" = unlist(after_lists$Key),
  "Generalised \nCluster" = unlist(after_lists$Generalised)
), n.sides = 10000) +
  scale_fill_viridis_c(direction = -1, option = "cividis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Patients") +
  ggtitle(str_glue("Over 70s, after 30 April 2020 (N = {combined_size})",
                   combined_size = c(unlist(after_lists$Respiratory), 
                                     unlist(after_lists$Key), 
                                     unlist(after_lists$Generalised)) %>% 
                     unique() %>% 
                     length()))

