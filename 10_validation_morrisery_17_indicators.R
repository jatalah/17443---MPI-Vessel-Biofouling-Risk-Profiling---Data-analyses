library(tidyverse)
library(broom)
library(caret)
library(clipr)
library(yardstick)

all_data <- read_csv('cleaned_data/all_crms_data.csv')

d <-
  read_csv('cleaned_data/model_data.csv', show_col_types = FALSE) %>%
  mutate(crms = factor(crms),
         type = fct_collapse(type, `Bulk/Tanker` = c("Bulk" , "Tanker"))) 

d %>% distinct(type)

# type     
# <chr>    
#   1 Container
# 2 RoRo     
# 3 Bulk     
# 4 Passenger

crms_pred_17_func <-function(data) {
    data %>%
      mutate(
        pred_17 = case_when(
          str_detect(type, c("Bulk|Tanker")) &
            (time_since_owm > 260 |
               time_since_af > 260) |
            (laid_up_more_10d > 0 &  ports_12m > 35) ~ "fail",
          type == "Container" &
            time_since_owm > 550 |
            (time_since_survey > 200 &  time_since_af > 500) ~ "fail",
          type == "RoRo" &
            (time_since_owm > 230 |
               time_since_af > 230 |  ports_12m > 80) ~ "fail",
          type == "Passenger" &
            (
              time_since_owm > 230 |
                time_since_af > 320 |
                laid_up_more_10d > 0 | max_lu_12m > 5
            )  ~ "fail",
          TRUE ~ 'pass'
        )
      ) %>% 
    mutate(pred_17 = factor(pred_17))
  }
crms_pred_21_func <- function(data) {
  data %>%
    mutate(
      pred_21 = case_when(
        type == "Bulker/Tanker" &
          time_since_af > 324 |
          laid_up_more_10d > 0 |
          time_since_owm > 324 |
          av_speed < 12 ~ "fail",
        type == "Container" &
          time_since_af > 550 |
          time_since_owm > 569 ~ "fail",
        type == "RoRo" &
          time_since_af > 363  |
          time_since_owm > 363 |
          time_since_survey > 329 ~ "fail",
        type == "Passenger" &
          time_since_af > 437 |
          time_since_owm > 270 ~ "fail",
        TRUE ~ 'pass'
      )
    ) %>% 
    mutate(pred_21 = factor(pred_21))
}
crms_pred_21_simple_func <- function(data) {
    data %>% mutate(pred_21_simple = if_else(time_since_af > 342, 'fail', 'pass') %>% factor())
  }


# validate 2017 indicators on 2021 data -----------
pred_17_df <- crms_pred_17_func(data = d %>% dplyr::filter(dataset == "quest_21"))
cf_pred_17 <- confusionMatrix(pred_17_df$crms, pred_17_df$pred_17)
cf_pred_17 %>% tidy() 
cf_pred_17$table %>% as_tibble() %>% write_clip()

# using yardstick----
cm <- conf_mat(pred_17_df, truth = crms, estimate = pred_17)

confusion_plot <-
  autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  labs(x = 'Observed CRMS status', y = "Predicted CRMS status")
confusion_plot

ggsave(
  confusion_plot,
  filename = 'figures/confusion_plot_2017_validation.png',
  width = 3,
  height = 3,
  dpi = 300
)

# validate 2021 indicators on 2021 and 2017 data -----------
pred_21_df <- crms_pred_21_func(data = d)
cf_pred_21 <- confusionMatrix(pred_21_df$crms, pred_21_df$pred_21)
cf_pred_21 %>% tidy()
cf_pred_21$table %>% as_tibble() 
# using yardstick----
cm_21 <- conf_mat(pred_21_df, truth = crms, estimate = pred_21)

confusion_plot_21 <-
  autoplot(cm_21, type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  labs(x = 'Observed CRMS status', y = "Predicted CRMS status")
confusion_plot_21


ggsave(
  confusion_plot_21,
  filename = 'figures/confusion_plot_2021_validation.png',
  width = 3,
  height = 3,
  dpi = 300
)


# validate simple 2021 indicators on 2021 and 2017 data -----------
pred_21_simple_df <- crms_pred_21_simple_func(data = d)
cf_pred_21_simple <- confusionMatrix(pred_21_simple_df$crms, pred_21_simple_df$pred_21_simple)
cf_pred_21_simple %>% tidy() 
cf_pred_21_simple$table %>% as_tibble()



# misclassified vessels----------
d_pred %>% 
dplyr::filter(crms != pred_21)


# using all three datsets--------
all_data <- read_csv('cleaned_data/all_crms_data.csv')
all_data %>% distinct(type)


all_data_pred <-
  all_data %>%
  mutate(
    pred_21 = case_when(
      type == "Bulker/Tanker" &
        time_since_af > 450 |
        NPorts > 47 ~ "fail",
      type == "Cargo" &
        time_since_af > 340 ~ "fail",
      type == "RoRo" &
        time_since_af > 320  |
        NPorts > 32 ~ "fail",
      type == "Passenger" &
        time_since_af > 370 |
        MMDays > 2 ~ "fail",
      TRUE ~ 'pass'
    )
  ) %>%
  mutate(
    pred_21a = case_when(
      type == "Bulker/Tanker" & time_since_af > 324 ~ "fail",
      type == "Container" &
        time_since_af > 550  ~ "fail",
      type == "RoRo" &
        time_since_af > 363  ~ "fail",
      type == "Passenger" &
        time_since_af > 437 ~ "fail",
      TRUE ~ 'pass'
    )
  ) %>% 
  mutate(pred_21_simple = if_else(time_since_af > 342, 'fail', 'pass')) 
  


caret::confusionMatrix(
  data = factor(all_data_pred$crms),
  reference = factor(all_data_pred$pred_21_simple)
) %>%
  tidy()
