# Plots
library(ggplot2)

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

## Breakdown of gender, age and death
pop_data <- scot_data %>%
  filter(!(subjid %in% travelled)) %>%
  group_by(subjid) %>%
  summarise(
    age = first(na.omit(age.factor)),
    sex = first(na.omit(sex)),
    ethnicity = first(na.omit(ethnicity)),
    outcome = first(na.omit(dsterm))
  ) %>%
  mutate(died = case_when(outcome == "Death" ~ "Yes", TRUE ~ "No") %>% factor(levels = c("No", "Yes")))


# Population pyramid
ggplot(pop_data, aes(x = age, fill = sex, alpha = died)) +
  geom_bar(data = filter(pop_data, sex == "Female")) +
  geom_bar(data = filter(pop_data, sex == "Male"), aes(y = ..count.. * (-1))) +
  scale_fill_discrete("Sex at birth") +
  scale_alpha_ordinal("Died", range = c(0.3, 1)) +
  coord_flip() +
  theme_minimal() +
  ylim(-100, 100) +
  xlab("Age") +
  ylab("Count of subjects") +
  ggtitle("Population pyramid of ISARIC subjects from Scottish hospitals",
    subtitle = str_glue("Excludes {n_travelled} who travelled in the 14 days prior to admission",
      n_travelled = length(travelled)
    )
  )


