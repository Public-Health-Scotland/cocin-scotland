source("extract-data/00_setup-environment.R")

latest_data <- dir_info(path(server_dir, "..", "final_data")) %>%
  filter(birth_time == max(birth_time, na.rm = TRUE)) %>%
  pull(path)

message(str_glue("Using file {basename(latest_data)}"))

data <- read_rds(latest_data) %>%
  as_tibble()


sari_data <- data %>%
  filter(covid == 1) %>%
  filter_at(vars(fever, cough, malaise, headache, myalgia, confusion, dizzy, sorethroat, sob), any_vars(. != 8)) %>%
  mutate(
    who_sari = if_else(fever == 1 & cough == 1, TRUE, FALSE),
    who_sari_adapted = if_else(fever == 1 | cough == 1, TRUE, FALSE),
    imove_sari = if_else((fever == 1| malaise == 1| headache == 1| myalgia == 1| confusion == 1| dizzy == 1) &
                           (cough == 1| sorethroat == 1| sob == 1), TRUE, FALSE)
  ) %>%
  mutate(sari_def = case_when(
    who_sari ~ "WHO",
    who_sari_adapted ~ "WHO-adapted",
    imove_sari ~ "IMOVE",
    TRUE ~ "None"
  ) %>%
    as_factor())


sari_definition_count <- count(sari_data, who_sari, who_sari_adapted, imove_sari)

pct_sari_match <- sari_data %>%
  summarise(
    n = n(),
    n_sari = sum(who_sari),
    n_sari_adapted = sum(who_sari_adapted),
    n_imove = sum(imove_sari, na.rm = TRUE),
    pct_sari = n_sari / n * 100,
    pct_sari_adapted = n_sari_adapted / n * 100,
    pct_imove = n_imove / n * 100
  )

sari_data %>%
  ggplot(aes(x = admitdate, colour = sari_def)) +
  geom_freqpoly(binwidth = 7) +
  scale_color_discrete("SARI definition") +
  theme_minimal()


sari_data %>%
  # filter(who_sari_adapted == FALSE) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit, -cough, -fever) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(y = symptom, fill = sari_def)) +
  scale_fill_discrete("SARI definition") +
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top")

sari_data %>%
  # filter(who_sari_adapted == FALSE) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(y = symptom, fill = symptom_val)) +
  scale_fill_discrete("SARI definition") +
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap("sari_def")

sari_data %>% 
  group_by(age_y) %>% 
  mutate(none = if_else(who_sari | who_sari_adapted | imove_sari, FALSE, TRUE)) %>% 
  summarise(across(c(who_sari, who_sari_adapted, imove_sari, none), sum, na.rm = TRUE)) %>% 
  ggplot(aes(x = age_y)) +
  geom_line(aes(y = who_sari), colour = "red") +
  geom_line(aes(y = who_sari_adapted), colour = "red", linetype = "dashed") +
  geom_line(aes(y = imove_sari), colour = "blue") +
  geom_line(aes(y = none), colour = "black")

sari_data %>% 
  group_by(age_y) %>% 
  summarise(across(c(abdopain:vomit), ~sum(.x == 1, na.rm = TRUE))) %>% 
  pivot_longer(cols = c(abdopain:vomit),
               names_to = "symptoms",
               values_to = "count") %>% 
  ggplot(aes(x = age_y)) +
  geom_line(aes(y = count, colour = symptoms))


sari_data %>% 
  group_by(age_y, sex) %>% 
  summarise(across(c(abdopain:vomit), ~sum(.x == 1, na.rm = TRUE))) %>% 
  pivot_longer(cols = c(abdopain:vomit),
               names_to = "symptoms",
               values_to = "count") %>% 
  ggplot(aes(x = age_y, colour = sex)) +
  geom_line(aes(y = count)) +
  facet_wrap("symptoms")
  

sari_data %>%
  filter(who_sari_adapted == FALSE & who_sari == FALSE & imove_sari == FALSE) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(y = symptom, fill = symptom_val)) +
  scale_fill_discrete("Has symptom") +
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("No definition match")

sari_data %>%
  filter(who_sari == TRUE) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(y = symptom, fill = symptom_val)) +
  scale_fill_discrete("Has symptom") +
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("WHO Definition")

sari_data %>%
  filter(imove_sari == TRUE) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(y = symptom, fill = symptom_val)) +
  scale_fill_discrete("Has symptom") +
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "top") +
  ggtitle("IMOVE Definition")


sari_data %>%
  filter(sari_def %in% c("WHO", "IMOVE")) %>%
  select(sari_def, admitdate, onsetdate, abdopain:vomit, -cough, -fever) %>%
  pivot_longer(
    cols = c(abdopain:vomit),
    names_to = "symptom",
    values_to = "symptom_val"
  ) %>%
  filter(symptom_val %in% c(1, 0)) %>%
  ggplot(aes(x = admitdate, colour = symptom, linetype = sari_def)) +
  geom_freqpoly(binwidth = 7) +
  scale_color_discrete("Other symptoms") +
  scale_linetype_discrete("") +
theme_minimal()
