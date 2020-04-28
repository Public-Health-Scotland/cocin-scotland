# Plots
library(ggplot2)

## Breakdown of gender, age and death 
pop_data <- scot_data %>% 
  group_by(subjid) %>% 
  summarise(age = first(na.omit(age.factor)),
            sex = first(na.omit(sex)),
            ethnicity = first(na.omit(ethnicity)),
            outcome = first(na.omit(dsterm))) %>% 
  mutate(died = case_when(outcome == "Death" ~ "Yes", TRUE ~ "No") %>% factor(levels = c("No", "Yes"))) 


# Population pyramid
ggplot(pop_data, aes(x = age, fill = sex, alpha = died)) +
  geom_bar(data = filter(pop_data, sex == "Female")) +
  geom_bar(data = filter(pop_data, sex == "Male"), aes(y = ..count..*(-1))) +
  scale_fill_discrete("Sex at birth") +
  scale_alpha_ordinal("Died", range = c(0.3, 1)) +
  coord_flip() +
  theme_minimal()



