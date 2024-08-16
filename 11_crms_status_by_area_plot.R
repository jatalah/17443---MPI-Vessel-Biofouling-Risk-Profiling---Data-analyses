library(tidyverse)
rm(list = ls())

dat <- read_csv('cleaned_data/caw21_biofouling_data_clean.csv') 
labs <- dat %>% distinct(vessel_code) %>% mutate(vessel_no = as.character(1:50)) %>% deframe()

# summarise CRMS status data by vessel-area combos----
dd <-
  dat %>%
  mutate(area_crms = rowSums(across(
    green_algae_50mm_length:other_organism_45
  ))) %>%
  group_by(vessel_code, type, area, subarea) %>%
  summarise(
    mean_area_crms = mean(area_crms),
    area_crms_binary = if_else(mean_area_crms > 0, 1, 0),
    area_crms_cat = if_else(mean_area_crms > 0, "Fail", "Pass"),
    .groups = 'drop'
  ) %>%
  group_by(vessel_code, type) %>%
  mutate(sum_fail = sum(area_crms_binary),
         fail_prop = sum_fail / n()) %>%
  ungroup() %>%
  mutate(vessel_no = fct_relabel(vessel_code, ~ labs))

# plot average failure rate by area--------
dd %>%
  ggplot(aes(
    x = fct_reorder(subarea, mean_area_crms), 
    y = fct_reorder(vessel_no, fail_prop),
    fill = mean_area_crms
  )) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank()) +
  labs(y = 'Vessel number', x = 'Area') +
  scale_fill_viridis_c(option = 'A', trans = 'sqrt', name = "Average\nfailure") +
  facet_wrap( ~ type, scales = 'free')

# plot failure rate by vessel-area--------
crms_area_plot <- 
dd %>%
  ggplot(aes(
    x = fct_reorder(area,mean_area_crms), 
    y = fct_reorder(vessel_no, fail_prop),
    fill = factor(area_crms_cat)
  )) +
  geom_tile(alpha =.9) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank()) +
  labs(y = 'Vessel number', x = 'Area') +
  scale_fill_discrete(name = "CRMS") +
  facet_wrap( ~ type, scales = 'free')

crms_area_plot


## Plot failure by CRMS variables ---------
vars <- read_csv('cleaned_data/crms_variable_names.csv')

d <-
  read_csv('cleaned_data/caw21_biofouling_data_clean.csv') %>% 
  pivot_longer(green_algae_50mm_length:other_organism_45) %>% 
  group_by(vessel_code, type, name) %>%
  summarise(value = mean (value)) %>% 
  left_join(vars, by = c('name' = 'value'))

crms_vars_plot <- 
d %>%
  ggplot(aes(
    y = name_long                            ,
    x = factor(as.numeric(factor(vessel_code))),
    fill = value
  )) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank()) +
  labs(y = NULL, x = 'Vessels') +
  scale_fill_viridis_c(option = 'A', name = "Average\nfailure") +
  facet_grid(area~ type, scales = 'free')

crms_vars_plot

ggsave(crms_vars_plot,
       filename = 'figures/crms_vars_plot.png',
       dpi = 600,
       width = 11, height = 5)

###Plot percentage failuyre by vessel ---
dd2 <-
  read_csv('cleaned_data/caw21_biofouling_data_clean.csv', show_col_types = FALSE) %>%
  mutate(area_crms = rowSums(across(
    green_algae_50mm_length:other_organism_45
  ))) %>% 
  group_by(vessel_code, subarea) %>%
  summarise(
    sum_subarea_crms = sum(area_crms),
    n = n(),
    subarea_crms_binary = if_else(sum_subarea_crms > 0, 1, 0),
    subarea_crms_cat = if_else(subarea_crms_binary > 0, "Fail", "Pass"),
    .groups = 'drop'
  ) %>% 
  group_by(vessel_code) %>%
  mutate(sum_fail = sum(subarea_crms_binary),
         fail_prop = sum_fail/n()) %>% 
  ungroup()


ggplot(dd2) +
  geom_bar(aes(
    x = fct_reorder(vessel_code, fail_prop, .desc = T),
    fill = fct_rev(subarea_crms_cat)
  ), 
  position = "fill", alpha = .8) +
  theme_minimal() +
  labs(x= 'Vessels', y = "Percentage") +
  scale_fill_discrete(name = "CRMS", direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_blank(), panel.grid = element_blank())

ggsave(plot = last_plot(),
       device = 'png',
       filename = 'figures/barplot_crms_proportion.png',
       width = 4,
       height = 3,
       bg = 'white')


dd2 %>%
  group_by(subarea) %>% 
  summarise(m = mean(subarea_crms_binary/n)) %>% 
  arrange(-m)
