library(caret)
library(tidyverse)
library(tidymodels)
library(yardstick)
library(probably)
library(knitr)
library(ggpubr)
library(plotmo)
library(plotROC)
library(pROC)
library(DHARMa)
library(pdp)
library(sjPlot)
theme_set(theme_minimal())
rm(list = ls())

model_data <- 
  read_csv('cleaned_data/model_data.csv') %>% 
  mutate(crms = factor(crms),
         type = fct_collapse(type, `Bulk/Tanker` = c("Bulk" , "Tanker")))

red_model_data <- 
  model_data %>% 
  dplyr::select(
    -dataset,
    -loa,
    -max_draught,
    -time_since_owm,
    -max_speed,
    -cleaning_since_af,
    -vessel_number
  ) %>% 
  mutate(crms = fct_relevel(crms, 'fail'))

red_model_data %>%
  group_by(type) %>%
  count()

red_model_data %>%
  group_by(type) %>%
  count(crms) %>% 
  mutate(prop = round(prop.table(n),2)*100)

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


m_logistic <-
  train(
    crms  ~ .,
    data = red_model_data,
    preProcess = c("center", "scale","YeoJohnson"),
    method = "glm",
    metric = "ROC",
    trControl=train_control
  )



# read model---
# m_logistic <- read_rds('outputs/m_logistic.rds')
summary(m_logistic$finalModel)
tab_model(m_logistic$finalModel, file = 'tables/full_glm.doc')

testDispersion(m_logistic$finalModel)

# variable importance plot----
# var_imp_plot <- 
# varImp(m_logistic$finalModel) %>% 
#   as_tibble(rownames = 'var') %>%
#   mutate(var = str_to_sentence(var),
#          var = str_replace_all(var, "_", " "),
#          var = str_replace_all(var, "af", "AF")) %>% 
#   ggplot(aes(x= fct_reorder(var,Overall), y = Overall, fill =  fct_reorder(var,Overall))) +
#   geom_col(color = 1) +
#   coord_flip() +
#   labs(x = 'Predictor variables', y = 'Relative influence (%)') +
#   scale_fill_viridis_d(guide = 'none')

var_imp_plot <- 
varImp(m_logistic$finalModel) %>% 
  as_tibble(rownames = 'var') %>%
  mutate(var = str_to_sentence(var),
         var = str_replace_all(var, "_", " "),
         var = str_replace_all(var, "af", "AF")) %>% 
  ggplot(aes(x = reorder(var, Overall ), y = Overall )) +
  geom_point() +
  geom_segment(aes(
    x = var,
    xend = var,
    y = 0,
    yend = Overall
  )) +
  ylab("Variable importance") +
  xlab("Predictor") +
  coord_flip()

ggsave(
  var_imp_plot,
  filename = 'figures/var_imp_plot.png',
  dpi = 300,
  width = 5,
  height = 4
)

# model selection----
m1 <-
  glm(
    crms ~ tonnage_dwt + beam + av_speed + ports_12m + laid_up_more_10d +
      max_lu_12m + time_since_survey + time_since_af + hull_works_since_af,
    family = 'binomial',
    data = red_model_data )

summary(m1)


m2 <- MASS::stepAIC(m1)
summary(m2)


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


# Final caret model -----------
set.seed(12345678)
final_model <-
  train(
    crms ~ av_speed + time_since_af,
    data = red_model_data  %>%  mutate(crms = fct_relevel(crms, 'fail')),
    preProcess = c("center", "scale","YeoJohnson"),
    method = "glm",
    metric = 'ROC',
    trControl=train_control
  )
final_model
write_rds(final_model, 'outputs/final_m_logistic.rds')
tab_model(final_model$finalModel, file = 'tables/final_glm_model.doc')
tab_model(final_model$finalModel, transform = NULL)


final_model$results %>%
  as_tibble() 

resample_res <- 
  final_model$pred %>% 
  as_tibble()

confusionMatrix(final_model, positive = 'fail')
confusionMatrix(final_model, "average", positive = 'fail')
confusionMatrix(final_model, "none", positive = 'fail')

confusionMatrix(data = resample_res$pred, reference = resample_res$obs, positive = 'fail')


# Partial plots----------
library(pdp) 
pdp_glm <- 
ggarrange(
  final_model %>%
    partial(pred.var = "time_since_af", prob = TRUE, which.class = "fail") %>%
    autoplot(
      smooth = F,
      ylab = 'Fail probability',
      xlab = "Time since last AF (days)"
    ) + ylim(0,1),
  final_model %>%
    partial(pred.var = "av_speed", prob = TRUE, which.class = "fail") %>%
    autoplot(
      smooth = F,
      ylab = 'Fail probability',
      xlab = "Average speed (kts)"
    ) + ylim(0,1))
pdp_glm
ggsave(pdp_glm,
       filename = 'figures/pdp_glm.png',
       dpi = 300,
       width = 6,
       height = 2.5)

# ROC curves for resamples prediction results ------
metrics(resample_res, truth = obs, pred) %>% 
  kable(digits = 2)

# AUC for resamples----
resample_res %>%
  group_by(Resample) %>% 
  roc_auc(truth = obs, fail)

resample_res %>%
  # group_by(Resample) %>%
  roc_curve(truth = obs, fail) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  # facet_wrap(~Resample) +
  scale_color_discrete(guide = NULL) +
  style_roc() +
  geom_abline(lty = 3)

roc_p <- ggplot(resample_res,aes(d = obs, m = pass)) + geom_roc(n.cuts = 0)

roc_p + annotate("text",
                 x = 0.75,
                 y = 0.25,
                 label = paste("AUC =", round((calc_auc(
                   roc_p
                 ))$AUC, 2))) + geom_abline(lty = 3)


roc(
  factor(resample_res$obs),
  resample_res$fail,
  percent = TRUE,
  plot = T,
  quiet = TRUE,
  print.auc = TRUE,
  print.thres = "best",
  auc = T
)

# Calculate best thresholds----
thres_find <-
  thresholder(
    final_model,
    threshold = seq(.01, 0.99, by = 0.01),
    final = TRUE,
    statistics = "all"
  ) %>% 
  as_tibble()

thres_find

best_thres <- 
  thres_find %>%
  dplyr::filter(J == max(J)) %>%
  pull(prob_threshold)

best_thres

ggplot(thres_find, aes(x = prob_threshold, y = J)) + 
  geom_line()
ggplot(thres_find, aes(x = prob_threshold, y = Dist)) + 
  geom_line()
ggplot(thres_find, aes(x = prob_threshold, y = Sensitivity)) + 
  geom_line() + 
  geom_line(aes(y = Specificity), col = "red")

###
d1 <-
  predict(final_model,
          newdata = red_model_data,
          type = "prob") %>%
  as_tibble() %>%
  bind_cols(
    predict(final_model,
            newdata = red_model_data,
            type = "raw") %>%
      as_tibble_col(column_name = 'pred_crms')
  ) %>%
  mutate(adj_pred_crms = if_else(fail > best_thres, 'fail', 'pass')) %>%
  bind_cols(model_data, .) %>%
  mutate(crms = factor(crms)) %>%
  mutate(crms = fct_relevel(crms, 'fail'),
         pred_crms = fct_relevel(pred_crms, 'fail'))



# misclassified vessels ------
misclasified_vessels <- 
  d1 %>% 
  dplyr::select(vessel_number,crms, pred_crms, fail, pass) %>% 
  dplyr::filter(crms != pred_crms) 

misclasified_vessels %>% kable(digits = 1)


# confusion matrix plot-----
cm <- conf_mat(d1, truth = crms, estimate = pred_crms)
cm 

# Performance statistics summary ---------
summary(cm) %>% 
  kable(digits = 2)

confusionMatrix(data = factor(d1$pred_crms), reference = d1$crms)

# confusion matrix plot -----------
confusion_plot <-
  autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  labs(x = 'Observed CRMS status', y = "Predicted CRMS status")

confusion_plot

ggsave(
  confusion_plot,
  filename = 'figures/confusion_plot_new_indicators.png',
  width = 3,
  height = 3,
  dpi = 300
)

# for final model predictions --------
auc <- d1 %>% roc_auc(truth = crms, fail)

roc_plot <- 
  d1 %>%
  roc_curve(truth = crms, fail) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  annotate(
    geom = 'text',
    x = .75,
    y = .2,
    label = paste0("AUC = ", round(auc$.estimate, 2))
  )

roc_plot


# Feature selection -----
set.seed(1234567)
ctrl <- 
  rfeControl(
    functions = lrFuncs,
    method = "cv",
    number = 5,
    repeats = 3,
    p = 0.8,
    returnResamp = "all",
    saveDetails = TRUE
  )

rfProfile <-
  rfe(red_model_data[, 2:10],
      red_model_data[[1]],
      sizes = c(1:9),
      rfeControl = ctrl)

# summarize the results
print(rfProfile)
rfe_preds <- predictors(rfProfile)
rfe_preds
rfProfile$fit
head(rfProfile$resample)

# plot results
pickSizeBest(rfProfile$results, metric = "Accuracy", maximize = T)

rfe_performance_plot <- 
  ggarrange(ggplot(rfProfile, metric = "Accuracy") + scale_x_continuous(breaks = 1:7),
            ggplot(rfProfile, metric = "Kappa") + scale_x_continuous(breaks = 1:7))

rfe_performance_plot

ggsave(
  rfe_performance_plot,
  filename = 'figures/rfe_performance_plot.png',
  dpi = 300,
  width = 5,
  height = 4
)



glm_pred_dat <- 
  predict(m2, type = 'response', newdata = red_model_data) %>% as_tibble_col(column_name = 'glm_prob') %>%
  mutate(glm_pred = if_else(glm_prob > 0.5, 'fail', 'pass')) %>% 
  bind_cols(red_model_data) %>%
  mutate(crms = fct_relevel(crms, 'fail'),
         glm_pred = fct_relevel(glm_pred, 'fail'))


confusionMatrix(data = factor(glm_pred_dat$glm_pred), reference = glm_pred_dat$crms)


glm_pred_dat %>%
  roc_auc(truth = crms, glm_prob)


glm_pred_dat %>%
  roc_curve(truth = crms, glm_prob) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal()

glm_pred_dat %>%
  roc_curve(truth = crms, glm_prob) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal()

