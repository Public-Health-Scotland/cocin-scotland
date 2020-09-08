source("extract-data/00_setup-environment.R")

latest_data <- dir_info(path(server_dir, "..", "final_data")) %>% 
  filter(birth_time == max(birth_time, na.rm = TRUE)) %>% 
  pull(path)

message(str_glue("Using file {basename(latest_data)}"))

data <- read_rds(latest_data) %>%
  as_tibble()


sari_data <- data %>% filter(covid == 1) %>% 
  filter_at(vars(fever, cough), any_vars(. != 8) ) %>% 
  mutate(who_sari = if_else(fever == 1 & cough == 1, TRUE, FALSE),
         who_sari_adapted = if_else(fever == 1 | cough == 1, TRUE, FALSE)) %>% 
  mutate(sari_def = case_when(who_sari ~ "WHO",
                              who_sari_adapted ~ "WHO-adapted",
                              TRUE ~ "None") %>% 
           as_factor) 


sari_definition_count <- count(sari_data, who_sari, who_sari_adapted)

pct_sari_match <- sari_data %>% 
  summarise(n = n(),
            n_sari = sum(who_sari),
            n_sari_adapted = sum(who_sari_adapted),
            pct_sari = n_sari / n * 100,
            pct_sari_adapted = n_sari_adapted / n * 100)

sari_data %>%
  ggplot(aes(x = admitdate, colour = sari_def)) +
  geom_freqpoly(binwidth = 7) +
  scale_color_discrete("SARI definition") +
  theme_minimal()


sari_data %>% 
  #filter(who_sari_adapted == FALSE) %>% 
  select(sari_def, admitdate, onsetdate, abdopain:vomit, -cough, -fever) %>% 
  pivot_longer(cols = c(abdopain:vomit),
               names_to = "symptom",
               values_to = "symptom_val") %>% 
  filter(symptom_val %in% c(1, 0)) %>% 
  ggplot(aes(x = symptom, fill = sari_def)) +
  scale_fill_discrete("SARI definition") +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


sari_data %>% 
  filter(sari_def %in% c("None", "WHO")) %>% 
  select(sari_def, admitdate, onsetdate, abdopain:vomit, -cough, -fever) %>% 
  pivot_longer(cols = c(abdopain:vomit),
               names_to = "symptom",
               values_to = "symptom_val") %>% 
  filter(symptom_val %in% c(1, 0)) %>% 
  ggplot(aes(x = admitdate, colour = symptom, linetype = sari_def)) +
  geom_freqpoly(binwidth = 7) +
  scale_color_discrete("Other symptoms") +
  theme_minimal()
