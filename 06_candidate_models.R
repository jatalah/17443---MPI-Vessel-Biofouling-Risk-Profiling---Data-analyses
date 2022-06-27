#' ---
#' title: "MPI Vessel Biofouling Risk Profiling - exploring candidate models"
#' output: 
#'     html_document: 
#'       code_folding: hide
#' author: Javier Atalah <br>  <br>  <javier.atalah@cawthron.org.nz>
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' ---
#' <img src = "figures/caw.webp">
#' <br>
#' <br>


#+ knitr::opts_chunk$set(fig.width=8, fig.height=8, echo=FALSE, warning=FALSE, message=FALSE)
library(tidyverse)
library(caret)
library(yardstick)
library(randomForest)
library(probably)
library(knitr)
library(gbm)
library(ggpubr)
library(plotmo)
library(plotROC)
theme_set(theme_minimal())
rm(list = ls())

# read data-------
model_data <- 
  read_csv('cleaned_data/model_data.csv') %>% 
  mutate(crms = factor(crms)) %>% 
  mutate(crms = fct_relevel(crms, 'fail'))


# CRMS compliance status ------
model_data %>%
  count(crms) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  kable()

model_data %>%
  count(crms, type) %>% 
  group_by(type) %>% 
  mutate(prop = round(prop.table(n),2)) %>% 
  arrange(type) %>% 
  kable()

# check correlated variables ---------
model_data_names <- read_csv('cleaned_data/model_data_names.csv')

pre_cor <-
  round(cor(
    model_data %>%
      rename_at(vars(model_data_names$name), ~ model_data_names$new_name) %>% 
      select(where(is.numeric) & !contains('vessel_')) ,
    
    method = "pearson",
    use = "complete.obs"
  ),
  2)

p_mat <- 
  ggcorrplot::cor_pmat(model_data %>% select(where(is.numeric) & !contains('vessel_')))

pred_corplot <- 
  ggcorrplot::ggcorrplot(pre_cor, 
                         p.mat = p_mat, 
                         hc.order = TRUE,
                         type = "lower", 
                         insig = "blank",
                         lab = T,
                         colors = c("#6D9EC1", "white", "#E46726"),
                         lab_size = 3,
                         tl.cex = 10)
pred_corplot

ggsave(pred_corplot,
       filename = 'figures/pred_corplot.png',
       dpi = 300,
       width = 7,
       height = 5)


# remove highly correlated variables----------
hc  <- findCorrelation(pre_cor, cutoff=0.8) # putt any value as a "cutoff" 

red_model_data <-
  model_data %>% 
  dplyr::select(
    -dataset,
    -type,
    -loa,
    -max_draught,
    -time_since_owm,
    -max_speed,
    -cleaning_since_af,
    -vessel_number
  ) %>% 
  %>% 
  mutate(crms = fct_relevel(crms, 'fail')) %>% 
  write_csv('cleaned_data/reduced_model_data.csv')

# red_model_data <- read_csv('cleaned_data/reduced_model_data.csv')

model_data_names %>% 
  dplyr::filter(name%in%names(red_model_data)) %>% 
  pull(new_name)



# set train control -------
set.seed(1234567)
train_control <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    savePredictions =  TRUE,
    classProbs = TRUE,
    selectionFunction = "tolerance",
    sampling = "smote", ## sub-sampling for class imbalance
    summaryFunction = twoClassSummary)


# random forest------------
set.seed(1234567)
m_rf <-
  train(
    crms  ~ .,
    data = red_model_data,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "rf",
    ntree = 1000,
    tuneGrid = expand.grid(.mtry=c(2,5)),
    trControl=train_control,
    importance = T
  )

m_rf
m_rf$finalModel


# neural networks----------------
# m_nn <-
#   train(
#     crms  ~ .,
#     data = red_model_data,
#     preProcess = c("center", "scale","YeoJohnson"),
#     method = "nnet",
#     trControl=train_control
#   )
# m_nn

# Boosted regression trees----------
m_brt <-
  train(
    crms  ~ .,
    data = red_model_data,
    method = "gbm",
    trControl=train_control,
    preProcess = c("center", "scale","YeoJohnson"),
    tuneGrid =  expand.grid(
      .n.trees = c(100, 500, 1000),
      .interaction.depth = c(1:3),
      .shrinkage = c(0.1),
      .n.minobsinnode = (10)
    ) 
  )
m_brt$results %>% 
  as_tibble() %>% 
  arrange(-Spec)

# Extreme boosting
m_xgbTree <-
  train(
    crms  ~ .,
    data = red_model_data,
    method = "xgbTree",
    trControl=train_control,
    preProcess = c("center", "scale","YeoJohnson")
  )
m_xgbTree$results %>% 
  as_tibble() %>% 
  arrange(-Spec)
m_xgbTree$bestTune
confusionMatrix(m_xgbTree)

# resamples performance-------
mean(m_xgbTree$resample$ROC)
mean(m_xgbTree$resample$Sens)
mean(m_xgbTree$resample$Spec)
mean(m_xgbTree$resample$ROC)

# logistic regression ------
m_logistic <-
  train(
    crms  ~ .,
    data = red_model_data,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "glm",
    trControl=train_control
  )

write_rds(m_logistic, 'outputs/m_logistic.rds')

# resamples performance-------
mean(m_logistic$resample$ROC)
mean(m_logistic$resample$Sens)
mean(m_logistic$resample$Spec)

# Support vector machine------
# m_svm <-
#   train(
#     crms  ~ .,
#     data = red_model_data,
#     preProcess = c("center", "scale","YeoJohnson"),
#     method = "lssvmRadial",
#     trControl=train_control
#   )
# 
# m_svm

# collect resamples and compare models-------------
results <-
  resamples(list(
    RF = m_rf,
    GLM = m_logistic,
    BRT = m_brt,
    # NN = m_nn,
    Xboost = m_xgbTree
  )) %>% 
  write_rds('outputs/candidate_models_results.rds')

results <- read_rds('outputs/candidate_models_results.rds')

# summarize the distributions-----
summary(results)
results$values %>% as_tibble() %>% 
  pivot_longer(-Resample) %>% 
  separate(name, c('model', 'metric')) %>% 
  group_by(model, metric) %>% 
  summarise(across(
    value,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max
    ),
    na.rm = T
  )) %>% 
  arrange(metric)

# Box-plots candidate model results --------------
candidate_models_performance_plot <-
  ggplot(results, metric = c("ROC", "Sens","Spec")) +
  labs(y = "Cross-validation performance")

candidate_models_performance_plot

ggsave(candidate_models_performance_plot,
       filename = 'figures/candidate_models_performance_plot.png',
       dpi = 300,
       width = 7,
       height = 5)
