## Individual variables ROC ----------
# library(pROC)
library(yardstick)
library(knitr)
library(clipr)
theme_set(theme_minimal())

# read data--------
model_data <- 
  read_csv('cleaned_data/model_data.csv') %>% 
  mutate(crms = factor(crms),
         type = fct_collapse(type, `Bulk/Tanker` = c("Bulk" ,"Tanker")))

names <- read_csv('cleaned_data/model_data_names.csv')

# names %>% 
#   mutate(label = paste0(name, '=', '"', Predictor, '",')) %>% 
#   select(label) %>% 
#   write_clip()

labels <- 
  as_labeller(
    c(vessel_number="vessel_number",
      dataset="dataset",
      crms="CRMS",
      type="Type",
      tonnage_dwt="Tonnage",
      max_draught="Max. draught",
      loa="LOA",
      beam="Beam",
      av_speed="Av. speed",
      max_speed="Max. speed",
      ports_12m="Port 12 m",
      laid_up_more_10d="Laid-up >10 d",
      max_lu_12m="Max. laid-up last 12 m",
      time_since_survey="Time since survey",
      time_since_af="Time since AF",
      time_since_owm="Time since OWM",
      cleaning_since_af="Cleaning since AF",
      hull_works_since_af="Hull works since AF"))


# ROC plots and analyses  --------
roc_analyses <-
  model_data %>%
  pivot_longer(cols = tonnage_dwt:time_since_owm) %>%
  group_by(name) %>%
  nest() %>%
  mutate(
    roc_aucs = map(data,  ~ roc_auc(.x, crms, value)),
    roc_curves = map (data, ~ roc_curve(.x, truth = crms, value)),
    best = map(
      roc_curves,
      ~ .x %>%
        mutate(best = specificity + sensitivity -1) %>%
        slice_max(best)
    )
  )

# summary table -----
roc_sum_table <- 
  roc_analyses %>% 
  select(roc_aucs, best) %>% 
  unnest(cols = c(roc_aucs, best)) %>% 
  ungroup() %>% 
  rename(AUC = ".estimate") 


# format table for report --------
roc_sum_table %>% 
  left_join(names) %>%
  select(Predictor, AUC:sensitivity) %>% 
  mutate_if(is.numeric, ~signif(.,2)) %>%
  arrange(-AUC)
  

kable(roc_sum_table, digits = 2)
# write_clip(roc_sum_table)


all_roc_plot <- 
model_data %>%
  pivot_longer(cols = tonnage_dwt:time_since_owm) %>%
  group_by(name) %>%
  roc_curve(truth = crms, value)  %>%
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity)) +
  geom_abline(lty = 3) +
  coord_equal() +
  labs(x = 'False positive rate', y = 'True positive rate') +
  geom_text(
    data = roc_sum_table,
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
    data = roc_sum_table,
    aes(
      x = Inf,
      y = .0,
      label = paste0("Threshold = ", round(.threshold, 2))
    ),
    hjust   = 1,
    vjust   = -1,
    size = 3
  ) +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap( ~ name, labeller = labels) 


all_roc_plot

# save all ROC plots----------
ggsave(
  all_roc_plot,
  filename = 'figures/all_roc_plot.png',
  dpi = 300,
  width = 9,
  height = 6
)

# ROC analysis by type ---------------
roc_analyses_type <-
  model_data %>%
  pivot_longer(cols = tonnage_dwt:time_since_owm) %>%
  group_by(name, type) %>%
  nest() %>%
  mutate(
    roc_aucs = map(data,  ~ .x %>% roc_auc(crms, value)),
    roc_curves = map (data, ~ .x %>% roc_curve(truth = crms, value)),
    best = map(
      roc_curves,
      ~ .x %>%
        mutate(youden = specificity  + sensitivity - 1,
               closest_topleft= (1 - sensitivity)^2 + (1- specificity)^2) %>%
        slice_max(youden) %>% 
        slice_max(closest_topleft)
    )
  )

# summary table by type -----
roc_sum_table_type <- 
  roc_analyses_type %>% 
  select(roc_aucs, best) %>% 
  unnest(cols = c(roc_aucs, best)) %>% 
  select(!contains(c(".m", ".estimator", "best"))) %>% 
  rename(AUC = ".estimate") %>% 
  arrange(name, type, -AUC) %>% 
  distinct(name, type, .keep_all = T) %>% 
  ungroup() 


# format table for report --------
roc_sum_table_type %>% 
  left_join(names) %>% 
  select(Predictor, everything(), - name) %>% 
  mutate_if(is.numeric, ~signif(.,2)) %>% write_clip()


labels_type <- 
  as_labeller(
    c(tonnage_dwt="Tonnage",
      av_speed="Av. speed",
      max_speed="Max. speed",
      ports_12m="Port 12 m",
      laid_up_more_10d="Laid-up >10 d",
      max_lu_12m="Max. laid-up last 12 m",
      time_since_survey="Time since survey",
      time_since_af="Time since AF",
      time_since_owm="Time since OWM",
      cleaning_since_af="Cleaning since AF",
      hull_works_since_af="Hull works since AF"))


all_roc_plot_type <- 
  model_data %>%
  select(-c(max_draught:beam)) %>% 
  mutate(crms = factor(crms)) %>%
  pivot_longer(cols = tonnage_dwt:time_since_owm) %>%
  group_by(name, type) %>%
  roc_curve(truth = crms, value)  %>%
  ggplot() +
  geom_path(aes(x = 1 - specificity, y = sensitivity, color = type)) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_discrete(name = NULL) +
  labs(x = 'False positive rate', y = 'True positive rate') +
  # geom_text(
  #   data = roc_sum_table_type %>% dplyr::filter(!name %in%c("max_draught","loa" ,"beam")),
  #   aes(
  #     x = Inf,
  #     y = .05,
  #     color = type,
  #     label = paste0("AUC = ", signif(AUC, 2)," ; ", "Thr. = ", signif(.threshold, 2))
  #   ),
  #   position = position_stack(), 
  #   hjust   = 1,
  #   vjust   = -1,
  #   size = 2.3
  # ) +
  theme(panel.spacing = unit(1, "lines")) +
  facet_wrap( ~ name, labeller = labels_type)


all_roc_plot_type

ggsave(
  all_roc_plot_type,
  filename = 'figures/all_roc_plot_type.png',
  dpi = 300,
  width = 9,
  height = 6
)

roc_sum_table_type %>% 
  dplyr::filter(AUC>.7)





# ggarrange(plotlist = roc_analyses$roc_plots, common.legend = T)

# Other approaches--------
# my_roc <- model_data %>% roc(crms, time_since_af)
# plot(my_roc)
# coords(my_roc, "best", ret = "threshold")
# 
# d1 %>% 
#   roc_curve(factor(pred_crms), time_since_survey) %>% 
#   mutate(best = specificity + sensitivity) %>%
#   slice_max(best)
# 
# 
# # other approaches to ROC curves for sanity check ----
# red_model_data %>%
#   roc(
#     crms,
#     time_since_af,
#     percent = TRUE,
#     plot = T,
#     quiet = TRUE,
#     print.auc = TRUE,
#     print.thres = "best"
#   )

# using the plotROC library by vessel type-----------
# library(plotROC)
# 
# roc_plot_faceted <-
#   model_data %>%
#   # pivot_longer(cols = tonnage_dwt:hull_works_since_af) %>%
#   pivot_longer(cols = c(time_since_owm, time_since_af)) %>% 
#   mutate(crms = if_else(crms == "fail", 1, 0)) %>%
#   ggplot(aes(d = crms, m = value)) +
#   geom_roc(labelsize = 3, n.cuts = 10, size = .1) +
#   facet_wrap(~name) +
#   theme_minimal() +
#   geom_abline(lty = 3)
# 
# roc_plot_faceted
# 
# calc_auc(roc_plot_faceted)
# 
# 
# model_data %>%
#   pivot_longer(cols = tonnage_dwt:hull_works_since_af) %>%
#   mutate(crms = if_else(crms == "fail", 1, 0)) %>%
#   ggplot(aes(d = crms, m = value)) +
#   geom_roc(labelsize = 3, n.cuts = 6, size = .1) +
#   facet_wrap(~name) +
#   theme_minimal() +
#   geom_abline(lty = 3)


# model_data %>%
#   mutate(crms = if_else(crms == "fail", 1, 0)) %>%
#   ggplot(aes(d = crms, m = time_since_af)) + geom_roc(labelsize = 4)

# roc(model_data$crms, model_data$time_since_af, percent=TRUE,
#     # arguments for plot
#     plot=TRUE, auc.polygon=F, max.auc.polygon=FALSE, grid=TRUE,
#     print.auc=TRUE, show.thres=TRUE, print.thres="best", print.thres.best.method="closest.topleft")
# 
# roc(model_data$crms, model_data$time_since_owm, percent=TRUE,
#     # arguments for plot
#     plot=TRUE, auc.polygon=F, grid=TRUE,
#     print.auc=TRUE, show.thres=TRUE,
#     print.thres="best",
#     print.thres.best.method="closest.topleft")

# roc_plots = pmap(
#   list(roc_curves,
#        roc_aucs,
#        best,
#        name),
#   .f =
#     ~ggplot(..1, aes(x = 1 - specificity, y = sensitivity)) +
#     geom_path() +
#     geom_abline(lty = 3) +
#     coord_equal() +
#     labs(x = 'False positive rate', y = 'True positive rate') +
#     annotate(
#       "text",
#       x = .2,
#       y = .9,
#       label = paste0('AUC = ', ..2 %>% pull(.estimate) %>% round(., 2)),
#       size = 3
#     ) +
#     annotate(
#       "text",
#       x = .2,
#       y = .7,
#       label = paste0('Threshold = ', ..3 %>% pull(.threshold) %>% round(., 1)),
#       size = 3
#     ) +
#     labs(subtitle = ..4)
# )