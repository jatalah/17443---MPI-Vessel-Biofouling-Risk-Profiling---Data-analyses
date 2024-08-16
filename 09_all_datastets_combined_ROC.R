#' ---
#' title: "MPI vessel risk profiling - merging Cawthron and NIWA data for risk indicators modelling"
#' output: 
#'     html_document: 
#'       code_folding: hide
#' author: Javier Atalah <br>  <br>  <javier.atalah@cawthron.org.nz>
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---

#' <img src = "figures/caw.webp">
#' <br>
#' <br>
#' 
#+ warning=FALSE, message=FALSE

library(readxl)
library(plotROC, quietly = T)
library(sjPlot, warn.conflicts = F)
library(pdp, warn.conflicts = F)
library(knitr)
library(yardstick)
library(tidyverse, quietly = T)
filter <- dplyr::filter
theme_set(theme_minimal())

# read NIWA data from Inglis et al. 2010 -----
niwa_data <-
  read_excel("data/HF_vessel_summary_voyage_history.xls",
             sheet = "Raw data by vessel") 

# read data from Vessel inspection optimisation where CRMS status was determined for vessels in the NIWA data----
opti_data <- 
  read_csv("data/optimisation_global_model_dat.csv", show_col_types = FALSE) %>% 
  select(vessel_code, vessel_type, crms)

# models data ---
model_data <- read_csv('cleaned_data/model_data.csv', show_col_types = FALSE)

# get variable names--------
names <- read_csv('cleaned_data/model_data_names.csv')

# explore----
niwa_data_clean <- 
niwa_data %>% 
  dplyr::filter(Provider %in% c("KML","NZD")) %>% 
  mutate(vessel_code = str_remove_all(QuesCode,'-')) %>% 
  select(vessel_code, 
         AFInt,
         NPorts,
         MMDays,
         PortDays) %>% 
  left_join(opti_data, by = 'vessel_code') %>% 
  rename(time_since_af  = "AFInt", type =  "vessel_type") %>% 
  drop_na(crms, time_since_af)


# merge datasets----
all_data <-
  bind_rows(niwa_data_clean, model_data) %>%
  mutate(crms = fct_relevel(crms, 'fail'),
         type = fct_collapse(type, 
                             Cargo = c("Container","Reefer", "Cargo"),
                             `Bulk/Tanker` = c("Bulk","Tanker")))

write_csv(all_data, 'cleaned_data/all_crms_data.csv')


# all_data <- read_csv('cleaned_data/all_crms_data.csv') %>% 
#   mutate(crms = fct_relevel(crms, 'fail'))

# total number of vessels----
nrow(all_data)

# number of vessels by type and crms ----
all_data %>% 
  group_by(type, crms) %>% 
  count() %>%
  print() %>% 
  write_clip()


# NPorts	Ports since last AF/DD
# MMDays	Duration of lay-up (d)
# PortDays	Mean days in port (last 20 recs)

labels <- as_labeller(
  c(
    NPorts = 'Ports since last AF/DD',
    MMDays	= 'Duration of lay-up (d)',
    PortDays	= 'Mean days in port',
    time_since_af = 'Time since AF'
  )
)

# histogram of time_since_af which is the main commonality between NIWA dataset and Cawthron surveys--
ggplot(all_data) +
  geom_histogram(aes(time_since_af, fill = type), bins = 15, alpha  = 0.8)


# # ROC plots and analyses  --------
roc_analyses_all_data <-
  all_data %>%
  select(crms, time_since_af:PortDays) %>%
  pivot_longer(cols = time_since_af:PortDays) %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    roc_aucs = map(data,  ~ .x %>% roc_auc(crms, value, event_level = 'first')),
    roc_curves = map (data, ~ .x %>% roc_curve(truth = crms, value, event_level = 'first')),
    youden = map(
      roc_curves,
      ~ .x %>%
        mutate(youden = specificity  + sensitivity - 1,
               closest_topleft= (1 - sensitivity)^2 + (1- specificity)^2) %>%
        slice_max(youden) %>% 
        slice_max(closest_topleft)
    ))

# summary table by type ---------
roc_sum_table_all_data <- 
  roc_analyses_all_data %>% 
  select(roc_aucs, youden) %>% 
  unnest(cols = c(roc_aucs, youden)) %>% 
  select(!contains(c(".m", ".estimator", "youden", "closest_topleft"))) %>% 
  rename(AUC = ".estimate") %>% 
  arrange(name, -AUC) %>% 
  ungroup() 


# format table for report --------
roc_sum_table_all_data %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  mutate(
    name = fct_recode(
      name,
      `Ports since last AF/DD` = "NPorts",
      `Duration of lay-up (d)` =  "MMDays",
      `Mean days in port` = "PortDays",
      `Time since AF` = "time_since_af"
    )
  )

# write_clip(roc_sum_table_all_data)

kable(roc_sum_table_all_data, digits = 2)

all_data %>%
  select(crms, type, time_since_af:PortDays) %>% 
  mutate(crms = factor(crms)) %>%
  pivot_longer(cols = time_since_af:PortDays) %>%
  group_by(name) %>%
  roc_curve(truth = crms, value, event_level = 'first')  %>%
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity)) +
  geom_abline(lty = 3) +
  coord_equal() +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap(~ name, labeller = labels) +
  labs(x = 'False positive rate', y = 'True positive rate') +
  geom_text(
    data = roc_sum_table_all_data,
    aes(
      x = Inf,
      y = .1,
      label = paste0("AUC = ", round(AUC, 2))
    ),
    hjust   = 1,
    vjust   = -1,
    size = 3
  ) +
  geom_text(
    data = roc_sum_table_all_data,
    aes(
      x = Inf,
      y = -Inf,
      label = paste0("Threshold = ", round(.threshold, 2))
    ),
    hjust   = 1,
    vjust   = -1,
    size = 3
  )


ggsave(
  last_plot(),
  filename = 'figures/roc_plots_all_data.png',
  dpi = 300,
  width = 6,
  height = 4
)


# ROC plots and analyses by type --------
roc_analyses_all_data_type <-
  all_data %>%
  select(crms, type, time_since_af:PortDays) %>%
  pivot_longer(cols = time_since_af:PortDays) %>%
  group_by(name,type) %>%
  nest() %>%
  mutate(
    roc_aucs = map(data,  ~ .x %>% roc_auc(crms, value, event_level = 'first')),
    roc_curves = map (data, ~ .x %>% roc_curve(truth = crms, value, event_level = 'first')),
    youden = map(
      roc_curves,
      ~ .x %>%
        mutate(youden = specificity  + sensitivity - 1,
               closest_topleft= (1 - sensitivity)^2 + (1- specificity)^2) %>%
        slice_max(youden) %>% 
        slice_max(closest_topleft)
  ))

# summary table by type --------
roc_sum_table_all_data_type <- 
  roc_analyses_all_data_type %>% 
  select(roc_aucs, youden) %>% 
  unnest(cols = c(roc_aucs, youden)) %>% 
  select(!contains(c(".m", ".estimator", "youden", "closest_topleft"))) %>% 
  rename(AUC = ".estimate") %>% 
  arrange(name, type, -AUC) %>% 
  distinct(name, type, .keep_all = T) %>% 
  ungroup() 


# format table for report --------
roc_sum_table_all_data_type %>%
  mutate_if(is.numeric, ~signif(.,2)) %>% 
  mutate(
    name = fct_recode(
      name,
      `Ports since last AF/DD` = "NPorts",
      `Duration of lay-up (d)` =  "MMDays",
      `Mean days in port` = "PortDays",
      `Time since AF` = "time_since_af"
    )
  )

# write_clip(roc_sum_table_all_data_type)
kable(roc_sum_table_all_data_type, digits = 2)

# Sketch ROC curves by vessel type ---------------
all_data %>%
  select(crms, type, time_since_af:PortDays) %>% 
  mutate(crms = factor(crms)) %>%
  pivot_longer(cols = time_since_af:PortDays) %>%
  group_by(name, type) %>%
  roc_curve(truth = crms, value, event_level = 'first')  %>%
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity, color = type)) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_discrete(name = NULL) +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap( ~ name, labeller = labels) +
  labs(x = 'False positive rate', y = 'True positive rate') +
  geom_text(
    data = roc_sum_table_all_data_type,
    aes(
      x = Inf,
      y = .05,
      color = type,
      label = paste0("AUC = ", signif(AUC, 2)," ; ", "Thr. = ", signif(.threshold, 2))
    ),
    position = position_stack(), 
    hjust   = 1,
    vjust   = -1,
    size = 2.3
  ) 
  
ggsave(
  last_plot(),
  filename = 'figures/roc_plots_all_data_type.png',
  dpi = 300,
  width = 8,
  height = 5
)

# logistic model with time_since_af and type as predictors----------------
m0 <- glm(crms ~ time_since_af  + NPorts +  MMDays + PortDays  + type , data = all_data, family = binomial)
m1 <- glm(crms ~ time_since_af  +  type , data = all_data, family = binomial)
summary(m1)

# model selection based on AIC----
m2 <- MASS::stepAIC(m1)
summary(m2)

# model summary---
tab_model(m2)

# response curves 
m2 %>%
  partial(pred.var = "time_since_af",
          prob = TRUE,
          grid.resolution = 250) %>%
  as_tibble() %>%
  ggplot() +
  geom_line(aes(time_since_af, yhat)) +
  labs(y = 'Fail probability', x = "Time since AF application (days)") +
  geom_vline(xintercept = 546, lty = 3) +
  annotate(
    x = 570,
    y = 0.6,
    geom = 'text',
    label = 'Threshold = 546 days',
    angle = 90,
    size = 4
  )
