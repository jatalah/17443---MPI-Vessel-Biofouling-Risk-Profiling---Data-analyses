#' ---
#' title: "MPI Vessel Biofouling Risk Profiling - questionnaire data exploration"
#' output: 
#'     html_document: 
#'       code_folding: hide
#' author: Javier Atalah <br>  <br>  <javier.atalah@cawthron.org.nz>
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' params: 
#'     output_dir: "../data_reports"
#' ---
#' <img src = "figures/caw.webp">
#' <br>
#' <br>


#+ knitr::opts_chunk$set(fig.width=8, fig.height=8, echo=FALSE, warning=FALSE, message=FALSE)

#+ warning=FALSE, message=FALSE, fig.width = 8
library(tidyverse)
library(janitor)
library(ggpubr)
library(knitr)
library(broom)
library(clipr)
theme_set(theme_minimal())
rm(list = ls())
filter <- dplyr::filter

# Read clean data --------
q <- read_csv('cleaned_data/questionnaire_data_2021_clean.csv')

# CRMS compliance status ------
q %>%
  count(crms) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

# CRMS compliance status by vessel type ------
q %>%
  group_by(Type) %>%
  count(crms) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

ggplot(q) +
  geom_bar(
    aes(x = Type, fill = crms),
    alpha = 0.6,
    color = 'gray20',
    position = 'fill'
  ) +
  labs(x = NULL, y = 'Percentage of vessels') +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = .20,
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

# Vessel particulars ------
range(q$`2 Date of arrival`) %>%  kable(caption = 'Arrival time range')

tabyl(q,`1 Port of arrival in New Zealand`) %>% kable()

tabyl(q,Time_in_NZ) %>% kable()

tabyl(q,Type) %>% kable(caption = 'Vessel type')

tabyl(q,Port_of_registration) %>% kable(caption = 'Port of rego')

tabyl(q, `14 Is this vessel chartered?`, Type) %>% kable(caption = 'Chartered vessel')

# 1. Vessel descriptors -------

## histograms of vessel characteristics -----
histo_desc_vars <- 
q %>% 
  pivot_longer(cols = Tonnage_DWT:Max_speed) %>% 
  ggplot(aes(value, fill = Type)) +
  geom_histogram(bins = 10, alpha = .6) +
  facet_wrap(~name, scales = 'free') +
  scale_x_continuous(label=scales::comma) +
  labs(x = NULL) +
  scale_fill_viridis_d(option = 'A', end = .8)

histo_desc_vars

## 1.1 Box-plots of vessel characteristics -----
labels_des <-
  as_labeller(
    c(
      Tonnage_DWT = "Tonnage (DWT)"  ,
      Max_draught = "Max. draught (m)",
      LOA = "L.O.A. (m)" ,
      Beam = "Beam (m)",
      Av_speed = "Av. speed (kt)",
      Max_speed = "Max. speed (kt)"
    )
  )


boxplot_desc_vars <- 
q %>% 
  pivot_longer(cols = Tonnage_DWT:Max_speed) %>% 
  ggplot(aes(Type, value, fill = crms)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~name, scales = 'free_y', labeller = labels_des) +
  scale_fill_discrete(name = "CRMS") +
  scale_y_continuous(label=scales::comma) +
  labs(x = NULL, y = NULL)

boxplot_desc_vars

ggsave(
  boxplot_desc_vars,
  device = "png",
  filename = 'figures/boxplots_vessel_descriptors.png',
  height = 3.5,
  width = 7,
  dpi = 300
)

## summary tables of vessel characteristics -----
q %>%
  pivot_longer(cols = Tonnage_DWT:Max_speed) %>% 
  group_by(name) %>% 
  summarise(across(value, list(
      median = median,
      min = min,
      max = max
    ), na.rm = T
  )) %>% 
  kable(digits = 1)


## summary tables of vessel characteristics by type -----
q %>%
  pivot_longer(cols = Tonnage_DWT:Max_speed) %>% 
  group_by(name, Type) %>% 
  summarise_at(
    "value",
    list(
      median = median,
      min = min,
      max = max
    ),
    na.rm = T
  ) %>% 
  kable(digits = 1)


# ANOVAs vessel characteristics-----
q %>%
  pivot_longer(cols = Tonnage_DWT:Max_speed) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(anovas = map(data, ~tidy(anova(lm(sqrt(value)~Type*crms, data = .x))))) %>% 
  select(anovas) %>% 
  unnest(anovas) %>%
  rename(F = "statistic") %>% 
  kable(digits = 2)

# 2. Vessel maintenance history ----

## 2.1 Box-plots of maintenance continuous variables ----
q %>%
  pivot_longer(cols = Time_since_survey:Time_since_OWM) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~name, scales = 'free') +
  scale_x_sqrt()

boxplot_maintenance_history <- 
  q %>%
  pivot_longer(cols = Time_since_survey:Time_since_OWM) %>% 
  mutate(name = str_replace_all(name, "_", " ")) %>% 
  ggplot(aes(Type, value, fill = crms)) +
  geom_boxplot(alpha = .5) +
  facet_wrap(~name, scales = 'free') +
  scale_y_continuous(label=scales::comma) +
  labs(x = NULL, y = "Days") 

boxplot_maintenance_history

ggsave(
  boxplot_maintenance_history,
  device = "png",
  filename = 'figures/boxplot_maintenance_history.png',
  height = 2.5,
  width = 8,
  dpi = 300
)

q %>%
  pivot_longer(cols = Time_since_survey:Time_since_OWM) %>%
  group_by(name, crms) %>%
  summarise_at(
    "value",
    list(
      n = ~ n(),
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      # Q1 = ~quantile(., probs = 0.25,na.rm = T),
      # Q3 = ~quantile(., probs = 0.75,na.rm = T),
      se = ~ sd / sqrt(n),
      ci = ~ se * 1.96
    ),
    na.rm = T
  ) %>%
  kable(digits = 2)

q %>%
  pivot_longer(cols = Time_since_survey:Time_since_OWM) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(anovas = map(data, ~tidy(anova(lm(value~Type * crms, data = .x))))) %>% 
  dplyr::select(anovas) %>% 
  unnest(anovas) %>%
  rename(F = "statistic") %>% 
  kable(digits = 2)

# Time since OWM vs Time since AF-----------
q %>% 
  ggplot(aes(Time_since_OWM,Time_since_AF, label = `5 Vessel registered name`, shape = Type, color  = crms)) +
  geom_point(size = 3, alpha = .5, position = position_jitter()) +
  # ggrepel::geom_text_repel(size = 2) +
  geom_abline(lty = 3) +
  labs(x = 'Time since OWM (days)', y = 'Time since AF (days)') +
  ylim(0,2000) +
  annotate(geom = 'text', x = 500, y = 1750, label = 'r = 0.91')

ggsave(
  last_plot(),
  device = "png",
  filename = 'figures/time_since_OWM_and_AF_scatterplot.png',
  height = 3,
  width = 4,
  dpi = 300
)

## 2.2 AF_Applied by vessel type -----
af_applied_plot <- 
q %>%
  mutate(AF_applied = fct_recode(AF_applied, `All areas` = "All hull and niche areas")) %>% 
  ggplot() +
  geom_bar(aes(x = Type, fill = AF_applied),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'AF applied') +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap( ~ crms)
af_applied_plot

### 2.2.1 Contingency tables and Fisher's test ----
q %>%
  group_by(crms) %>%
  count(AF_applied) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

tabyl(q, AF_applied, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)


## 2.3 Hull protection ----
hull_protection_plot <-
  ggplot(q) +
  geom_bar(aes(x = Type, fill = `Hull protection`),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = "Hull protection") +
  scale_fill_viridis_d(option = 'D',
                       end = .8,
                       begin = 0, 
                       name = NULL) +
  facet_wrap( ~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

hull_protection_plot

tabyl(q, `Hull protection`, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

tabyl(q, `Hull protection`)

## 2.4 MGSP --------
mgps_plot <-
  ggplot(q) +
  geom_bar(aes(x = Type, fill = fct_relevel(MGSP, "Yes", "No")),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'MGPS') +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name = NULL
  ) +
  facet_wrap( ~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 


mgps_plot

tabyl(q, MGSP, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(crms) %>%
  count(MGSP)

tabyl(q, crms, MGSP)

## 2.5 Intake treatment -----
intake_treat_plot <-
  q %>% 
  drop_na(Intake_treatment) %>% 
  ggplot() +
  geom_bar(aes(x = Type, fill = fct_rev(Intake_treatment)),
           alpha = 0.6,
           color = 'gray20',
           position= 'fill') +
  labs(x = NULL, y = NULL, subtitle = "Intake treatment") +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name = NULL
  ) +
  facet_wrap( ~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

intake_treat_plot

tabyl(q, Intake_treatment, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(Type, crms) %>%
  count(Intake_treatment) %>% 
  kable()

tabyl(q, Intake_treatment)

## 2.6 Regular_intake_treatment-----------
regular_intake_trea_plot <-
  q %>%
  drop_na(Regular_intake_treatment) %>% 
  ggplot() +
  geom_bar(
    aes(x = Type, fill = Regular_intake_treatment),
    alpha = 0.6,
    color = 'gray20',
    position = 'fill'
  ) +
  labs(x = NULL, y = NULL, subtitle =  "Intake treatment frequency") +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name = NULL
  ) +
  facet_wrap( ~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

regular_intake_trea_plot

tabyl(q, Regular_intake_treatment, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)


q %>%
  group_by(Type, crms) %>%
  count(Regular_intake_treatment)

tabyl(q,Regular_intake_treatment)

## 2.7 Cleaning_since_AF barplot------
cleaning_since_AF_plot <-
  ggplot(q) +
  geom_bar(aes(x = Type, fill = Cleaning_since_AF),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'Cleaning since AF') +
  scale_fill_viridis_d(
    option = 'D',
    end = .5,
    begin = 0.2,
    name = NULL
  ) +
  facet_wrap(~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

cleaning_since_AF_plot

tabyl(q, Cleaning_since_AF, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(Type, crms) %>%
  count(Cleaning_since_AF) 

tabyl(q, Cleaning_since_AF)

## 2.8 Hull works since AF ------
hull_works_since_af_plot <-

  ggplot(q) +
  geom_bar(aes(x = Type, fill = Hull_works_since_AF),
           alpha = 0.8,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'Hull works since AF') +
  scale_fill_viridis_d(
    option = 'D',
    end = .5,
    begin = 0.2,
    name = NULL
  ) +
  facet_wrap( ~ crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

hull_works_since_af_plot

## Cleaning_since_AF table ------
tabyl(q, Hull_works_since_AF, crms) %>% 
  kable(digits = 2)

tabyl(q, Hull_works_since_AF, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(Type, crms) %>%
  count(Hull_works_since_AF) 

### tables of other maintenance questions------
tabyl(q,
  `44 Were any deficiencies detected during the last classification survey relating to hull or coating integrity?`, crms
) %>%
  kable(digits = 2)

tabyl(q, `45 If answered YES to the previous question, add details on what deficiencies have been detected`, crms) %>% 
  kable(digits = 2)


## 2.9 Multi AF -------
multi_af_plot <-
  q %>% 
  drop_na(Multi_AF) %>% 
  ggplot() +
  geom_bar(aes(x = Type, fill = Multi_AF),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'Multiple AF') +
  scale_fill_viridis_d(
    option = 'D',
    end = .5,
    begin = .2,
    name = NULL
  ) +
  facet_wrap(~crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

multi_af_plot

## Cleaning_since_AF table ------
tabyl(q, Multi_AF, crms) %>% 
  kable(digits = 2)

tabyl(q, Hull_works_since_AF, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(Type, crms) %>%
  count(Multi_AF) 

### Save maintenance history plots together --------
maintenance_history_plots1 <- 
  ggarrange(
  af_applied_plot,
  multi_af_plot,
  hull_protection_plot,
  mgps_plot,
  labels = 'auto',legend = 'bottom'
  
)

ggsave(
  maintenance_history_plots1,
  device = "png",
  filename = 'figures/maintenance_history_plots1.png',
  height = 5,
  width = 9,
  dpi = 300
)

maintenance_history_plots2 <- 
ggarrange(
  intake_treat_plot,
  regular_intake_trea_plot,
  cleaning_since_AF_plot,
  hull_works_since_af_plot,
  labels = 'auto',
  legend = 'bottom'
)
maintenance_history_plots2

ggsave(
  maintenance_history_plots2,
  device = "png",
  filename = 'figures/maintenance_history_plots2.png',
  height = 6,
  width = 9.5,
  dpi = 300
)

# 3. Voyage history -----------

## 3.1 Set route ----
set_route_plot <-
  ggplot(q) +
  geom_bar(aes(x = Type, fill = Set_route),
           alpha = 0.6,
           color = 'gray20',
           position = 'fill') +
  labs(x = NULL, y = NULL, subtitle = 'Route type') +
  scale_fill_viridis_d(
    option = 'D',
    end = .5,
    begin = .2,
    name = NULL
  ) +
  facet_wrap(~crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

set_route_plot

q %>%
  group_by(Type, crms) %>%
  count(Set_route) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

q %>%
  group_by(crms) %>%
  count(Set_route) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

tabyl(q, Set_route, crms, show_na = F) %>%
  fisher.test(alternative = "two.sided") %>%
  glance() %>%
  kable(digits = 2)

## 3.2 No. of ports since OWM -------
ports_since_owm_plot <- 
  q %>% 
  mutate(ports_since_owm = fct_relevel(
    `47 How many different ports has this vessel visited since its most recent out-of-water maintenance?`,
    "0",
    "1 - 5",
    '6 - 10',
    "11 - 20",
    "21 - 50",
    ">50"
  )) %>% 
  ggplot() +
  geom_bar(
    aes(x = Type, fill = ports_since_owm),
    alpha = 0.6,
    color = 'gray20',
    position = 'fill'
  ) +
  labs(x = NULL, y = NULL, subtitle = "No. of ports visited since OWM") +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name = NULL
  ) +
  facet_wrap(~crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

ports_since_owm_plot

tabyl(
  q,
  `47 How many different ports has this vessel visited since its most recent out-of-water maintenance?`, crms
) %>% 
  kable()

tabyl(
  q,
  `47 How many different ports has this vessel visited since its most recent out-of-water maintenance?`, crms
) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)

q %>%
  group_by(Type, crms) %>%
  count(ports_since_owm) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

q %>%
  group_by(crms) %>%
  count(ports_since_owm) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

## 3.3 No. of countries since OWM -------
countries_since_owm_plot <- 
q %>%
  mutate(
    countries_since_owm = fct_relevel(
      `48 How many different countries has this vessel visited since its most recent out-of-water maintenance?`,
      "0",
      "1 - 5",
      '6 - 10',
      "11 - 20",
      "21 - 50",
      ">50"
    )
  ) %>%
  drop_na(countries_since_owm) %>% 
  ggplot() +
  geom_bar(
    aes(x = Type, fill = countries_since_owm),
    alpha = 0.6,
    color = 'gray20',
    position = 'fill'
  ) +
  labs(x = NULL, y = NULL, subtitle =  "No. of countires visited since OWM") +
  scale_fill_viridis_d(
    option = 'D',
    end = .8,
    begin = 0,
    name =NULL
  ) +
  facet_wrap(~crms) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

countries_since_owm_plot

tabyl(
  q,
  `48 How many different countries has this vessel visited since its most recent out-of-water maintenance?`,
  crms
) %>% kable(digits = 2)

q %>%
  group_by(crms) %>%
  count(`48 How many different countries has this vessel visited since its most recent out-of-water maintenance?`) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()


tabyl(q, `48 How many different countries has this vessel visited since its most recent out-of-water maintenance?`, crms, show_na = F) %>% 
  fisher.test(alternative="two.sided") %>% 
  glance() %>% 
  kable(digits = 2)


q %>%
  group_by(crms) %>%
  count(`48 How many different countries has this vessel visited since its most recent out-of-water maintenance?`) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

### Save voyage history barplots together --------
voyage_history_barplots <- 
  ggarrange(
    set_route_plot,
    ports_since_owm_plot,
    countries_since_owm_plot,
    labels = 'auto',
    legend = 'right', 
    ncol = 1
    
  )
voyage_history_barplots

ggsave(
  voyage_history_barplots,
  device = "png",
  filename = 'figures/voyage_history_barplots.png',
  height = 7,
  width = 5,
  dpi = 300
)


## 3.4 Box-plots No. of ports in the last 12 mo,laid-up_more_10_d and max_LU_12m-------
boxplot_port_no <- 
q %>%
  pivot_longer(
    cols = c(
      Ports_12m:Max_LU_12m,
      time_since_idle,
      time_since_last_port,
      time_in_last_port
    )
  ) %>%
  mutate(name = str_to_sentence(str_replace_all(name, "_", " "))) %>% 
  ggplot(aes(Type, value, fill = crms)) +
  geom_boxplot(alpha = .6) +
  facet_wrap( ~ name, scales = 'free') +
  scale_y_continuous(label = scales::comma) +
  labs(x = NULL, y = NULL) +
  scale_fill_discrete(name = "CRMS")

boxplot_port_no

ggsave(
  boxplot_port_no,
  device = "png",
  filename = 'figures/boxplot_port_no.png',
  height = 4,
  width = 8,
  dpi = 300
)


# ANOVAs vessel characteristics--
q %>%
  pivot_longer(
    cols = c(
      Ports_12m:Max_LU_12m,
      time_since_idle,
      time_since_last_port,
      time_in_last_port
    )
  ) %>%
  group_by(name) %>% 
  nest() %>% 
  mutate(anovas = map(data, ~tidy(anova(lm(value~Type+crms, data = .x))))) %>% 
  select(anovas) %>% 
  unnest(anovas) %>%
  rename(F = "statistic") %>% 
  kable(digits = 2)


### check idle number of idles days variables ----
qplot(q$no_idle_days, q$Max_LU_12m) + 
  geom_abline(
  slope = 1,
  intercept = 0,
  lty = 2,
  color = 2
)

## summary tables No. of ports in the last 12 mo,laid-up_more_10_d and max_LU_12m-----
q %>%
  pivot_longer(cols = c(Ports_12m:Max_LU_12m, no_idle_days, time_since_idle)) %>%
  group_by(name, Type) %>%
  summarise(across(value, list(
    median = median,
    min = min,
    max = max
  ), na.rm = T)) %>% 
  kable(digits = 2, caption = 'summary tables No. of ports in the last 12 mo,laid-up_more_10_d and max_LU_12m by CRMS')

## summary tables No. of ports in the last 12 mo,laid-up_more_10_d and max_LU_12m------
q %>%
  pivot_longer(
    cols = c(
      Ports_12m:Max_LU_12m,
      no_idle_days,
      time_since_idle,
      time_since_last_port,
      time_in_last_port
    )
  ) %>%
  group_by(name, Type) %>%
  summarise_at("value",
               list(median = median,
                    min = min,
                    max = max),
               na.rm = T) %>%
  kable(digits = 1, caption = 'ummary tables No. of ports in the last 12 mo,laid-up_more_10_d and max_LU_12m by type and CRMS')


## Q52 Where was that port or anchorage? Indicate port and country------------
q %>%
  mutate(country = str_extract(
    `52 Where was that port or anchorage? Indicate port and country`,
    '\\b[^,]+$'
  )) %>%
  group_by(country) %>%
  count %>%
  arrange(-n, country) %>%
  kable(caption = 'Country of anchorage during laid-up period')

# ## Q53 When was the start date of the idle or laid-up period indicated in question 51?------
# summary(q$`53 When was the start date of the idle or laid-up period indicated in question 51?`)
# 
# ## Q54 When was the end date of the idle or laid-up period indicated in question 51?-----------
# summary(q$`54 When was the end date of the idle or laid-up period indicated in question 51?`)

# 4. Inspections ------------
tabyl(q, Inspection) %>% 
  kable(digits = 2)

## Inspection outcome ------------
distinct(q, `62 What was the inspection outcome?`) %>% 
  kable(digits = 2)


## time_since_last_inspection -------
ggplot(q, aes(Type, time_since_last_inspection, fill = crms)) +
  geom_boxplot(alpha = .6) +
  scale_y_log10()

# Management plans ---------

## Management plan -------
tabyl(q, BF_management_plan) %>%
  kable(digits = 2)

# aware of IMO guidelines ----
tabyl(q, Aware_of_IMO_BFG) %>%
  kable(digits = 2)

# THE END #####