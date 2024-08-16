#' ---
#' title: "Validation of MPI existing indicators"
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

library(tidyverse)
library(janitor)
library(plotROC)
library(readxl)
library(knitr)
theme_set(theme_minimal())
rm(list = ls())
filter <- dplyr::filter

# read_data------
crms_mpi <-
  read_excel(
    "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/1. Vessel sampling communications/VesselSelection_and_Tracking.xlsx",
    "Risk level"
  ) %>%
  mutate(crms = tolower(`Pass/Fail`), .keep= 'unused',
         .after = `Caw c`) %>% 
  select(1:`Areas failing`) %>% 
  clean_names() %>% 
  mutate(mpi_pred = if_else(mpi_c>1,'fail','pass'),
         crms = fct_relevel(crms, 'fail'),
         mpi_pred = fct_relevel(mpi_pred, 'fail'))


crms_mpi %>%
  count(crms) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

crms_mpi %>%
  mutate(crms = if_else(crms == "fail", 1, 0)) %>%
  ggplot(aes(d = crms, m = mpi_c)) + geom_roc(labelsize = 3) +
  geom_abline(lty = 3)

crms_mpi %>% 
  select(type, crms, mpi_c, mpi_pred, age_of_antifoul_when_sampled:lay_up_period_in_last_12_months) %>% 
  dplyr::filter(crms != mpi_pred) %>% 
  write_csv('tables/mpi_risk_validation_misclasified.csv')

crms_mpi %>%
  select(crms, mpi_pred) %>%
  table() %>% 
  as_tibble() 

cm <- yardstick::conf_mat(crms_mpi, truth = crms, estimate = mpi_pred)

confusion_plot <-
  autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  labs(x = 'Observed CRMS status', y = "Predicted CRMS status")
confusion_plot

summary(cm) %>% 
  select(-.estimator) %>% 
  write_csv('tables/mpi_risk_validation_stats.csv') %>% 
  print() %>% 
  kable()

confusion_plot

ggsave(
  confusion_plot,
  filename = 'figures/confusion_plot_mpi_validation.png',
  width = 3,
  height = 3,
  dpi = 300
)

caret::confusionMatrix(data = factor(crms_mpi$crms), reference = factor(crms_mpi$mpi_pred)) 

# The overall accuracy is computed along with a 95 percent confidence intervals and a one-sided test to see if it is better than the "no information rate," which is taken to be the largest class percentage in the data.

# Calculates Cohen's Kappa test and associated p-value as an index of inter-rater agreement between the two methods (MPI risk alert Matrix and observed by vessel inspection) on compliance status (pass vs. fail).

