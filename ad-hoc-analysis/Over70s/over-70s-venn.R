venn <- topline %>%
  left_join((.) %>% 
              filter_at(vars("Ear pain", "Confusion", "Seizures", "Skin rash", "Skin ulcers", "Conjunctivitis", "Bleeding (Haemorrhage)", "Headache"), any_vars(. == "YES")) %>% 
              mutate(Neurocutaneous = "YES") %>% 
              select(subjid, Neurocutaneous)
  ) %>% 
  left_join((.) %>% 
              filter_at(vars("Fatigue", "Muscle ache", "Lymphadenopathy", "Fever"), any_vars(. == "YES")) %>% 
              mutate(Generalised = "YES") %>% 
              select(subjid, Generalised)
  ) %>% 
  left_join((.) %>% 
              filter_at(vars("Diarrhoea", "Nausa/vomiting", "Abdominal pain", "Joint pain"), any_vars(. == "YES")) %>% 
              mutate(Gastrointestinal = "YES") %>% 
              select(subjid, Gastrointestinal)
  ) %>% 
  left_join((.) %>% 
              filter_at(vars("Cough", "Cough (blood)", "Cough (sputum)", "Wheeze", "Shortness of breath", "Sore throat", "Chest pain", "Lower chest wall indrawing", "Runny nose"), any_vars(. == "YES")) %>% 
              mutate(Respiratory = "YES") %>% 
              select(subjid, Respiratory)
  ) %>% 
  left_join((.) %>% 
              filter_at(vars("Cough", "Cough (blood)", "Cough (sputum)", "Fever", "Ageusia", "Anosmia"), any_vars(. == "YES")) %>% 
              mutate(Key = "YES") %>% 
              select(subjid, Key)
  ) %>% 
  select(-age, -hostdat)

fever_group_before <- venn %>% 
  filter(Fever == "YES") %>% 
  filter(admission == "Before 30th April") %>% 
  pull(subjid)
respiratory_group_before <- venn %>% filter(Respiratory == "YES") %>% 
  filter(admission == "Before 30th April") %>% 
  pull(subjid)
gastro_group_before <- venn %>% filter(Gastrointestinal == "YES")%>% 
  filter(admission == "Before 30th April") %>% 
  pull(subjid)


fever_group_after <- venn %>% 
  filter(Fever == "YES") %>% 
  filter(admission == "On or after 30th April") %>% 
  pull(subjid)
respiratory_group_after <- venn %>% filter(Respiratory == "YES") %>% 
  filter(admission == "On or after 30th April") %>% 
  pull(subjid)
gastro_group_after <- venn %>% filter(Gastrointestinal == "YES")%>% 
  filter(admission == "On or after 30th April") %>% 
  pull(subjid)

venn_before <- ggVennDiagram(list("Gastrointestinal \nCluster"= gastro_group_before,
              Fever = fever_group_before,
  "Respiratory \nCluster" = respiratory_group_before)
                   ) +
  scale_fill_continuous() +
  ggtitle("Over 70s, Before April 30th")

venn_after <- ggVennDiagram(list("Gastrointestinal \nCluster"= gastro_group_after,
                   Fever = fever_group_after,
                   "Respiratory \nCluster" = respiratory_group_after)
) +
  scale_fill_continuous() +
  ggtitle("Over 70s, After April 30th")

gridExtra::grid.arrange(venn_before, venn_after, ncol = 2)
