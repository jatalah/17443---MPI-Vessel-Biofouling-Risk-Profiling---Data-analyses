#' ---
#' title: "MPI Vessel Biofouling Risk Profiling - data exploration of biofouling variables"
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

#+ warning=FALSE, message=FALSE

# load libraries and clean env.-------
library(tidyverse, quietly = T)
library(ggcorrplot)
library(janitor)
library(knitr)
library(indicspecies)
suppressMessages(library(vegan))
library(ggord)
library(ggpubr)
library(DT)

theme_set(theme_minimal())
rm(list = ls())

d <- read_csv('cleaned_data/caw21_biofouling_data_clean.csv') 


d %>% 
  select(`% cover`:Richness) %>% 
  cor() %>% 
  as_tibble(rownames = 'var') %>% 
  kable()

# Vessel numbers by type ----
d %>% 
  distinct(vessel_code, type) %>% 
  tabyl(type) %>% 
  kable()

# convert to long format---
d_long <- 
  d %>% 
  pivot_longer(cols = `% cover`:Richness)

#'<H3>  Histogram of response variables </H3> 
#+ fig.width = 8
ggplot(d_long, aes(value, fill = crms)) +
  geom_histogram(bins = 8) +
  facet_wrap(~name, scales = 'free')


#'<H3>  Summary table response variables </H3> 
d_long %>%
  group_by(name, crms) %>% 
  summarise(across(
    value,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      n = ~n(),
      IQR1 = ~ quantile(., probs = 0.25, na.rm = T),
      IQR3 = ~ quantile(., probs = 0.75, na.rm = T)
    ),
    na.rm = T
  )) %>% 
  kable(digits = 2)

# Numbers of samples by area and subarea----
d %>% 
  group_by(area, subarea, type) %>% 
  count() %>% 
  ggplot() +
  geom_col(aes(area, y = n, fill = subarea),
           alpha = 0.6,
           color = 'gray20') +
  facet_wrap(~type, scales = 'fixed') +
  scale_fill_viridis_d() +
  coord_flip()

# average data by vessel, crms and area ---
d_long_sum <- 
  d_long %>% 
  group_by(vessel_code, type, crms, area, name) %>% 
  summarise(value = mean(value), .groups = 'drop')

#'<H3>  Summary table response variables </H3> 
d_long_sum %>%
  group_by(name, crms) %>% 
  summarise(across(
    value,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      n = ~n(),
      IQR1 = ~ quantile(., probs = 0.25, na.rm = T),
      IQR3 = ~ quantile(., probs = 0.75, na.rm = T)
    ),
    na.rm = T
  ))  %>% 
  mutate(se = value_sd /sqrt(value_n)) %>% 
  kable(digits = 2)

#'<H3> Cover, LoF and FR rating by vessel type</H3> 
#+ fig.width = 8, fig.height = 5
boxplot_type <- 
  ggplot(d_long_sum, aes(type, value, fill = crms)) +
  geom_boxplot(alpha = .2) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~name, scales = 'free')

boxplot_type


#'<H3> Cover, LoF and FR rating by area</H3> 
boxplot_area <- 
  ggplot(d_long_sum, aes(area, value, fill = crms)) +
  geom_boxplot(alpha = .2) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~name, scales = 'free_x')

boxplot_area

#'<H3> Summary table Cover, LoF and FR rating by area</H3> 
d_long_sum %>%
  group_by(name, area) %>% 
  summarise(across(
    value,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      n = ~n(),
      IQR1 = ~ quantile(., probs = 0.25, na.rm = T),
      IQR3 = ~ quantile(., probs = 0.75, na.rm = T)
    ),
    na.rm = T
  ), .groups = 'drop')  %>% 
  mutate(se = value_sd /sqrt(value_n)) %>%
  arrange(name, -value_mean) %>% 
  datatable() %>% 
  formatRound(columns=c(3:11), digits=2)

# save boxplot by vessel area-----
ggsave(
  boxplot_area,
  filename = 'figures/boxplot_area.png',
  device = 'png',
  dpi = 300,
  width = 6.5,
  height = 6.5
)

#'<H3> Cover, LoF and FR rating by area and subarea</H3> 
#+ fig.width = 14
ggplot(d_long, aes(area, value, fill= subarea)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~name, scales = 'free')

#'<H3> Cover, LoF and FR rating by side</H3> 
#+ fig.width = 8
bp_side <-
  d_long %>% 
  group_by(vessel_code, type, crms, area, side, name) %>% 
  summarise(value = mean(value), .groups = 'drop') %>% 
  drop_na(side) %>%
  ggplot(aes(side, value, fill =crms)) +
  geom_boxplot(alpha = .2) +
  labs(x = NULL, y = NULL) +
  # coord_flip() +
  facet_wrap( ~ name, scales = 'free')
bp_side

#'<H3> Cover, LoF and FR rating by region</H3> 
boxplot_region <- 
  d_long %>% 
  group_by(vessel_code, type, crms, area, region, name) %>% 
  summarise(value = mean(value), .groups = 'drop') %>%  
  drop_na(region) %>% 
  ggplot(aes(region, value, fill = crms)) +
  labs(x = NULL, y = NULL) +
  geom_boxplot(alpha = .2) +
  facet_wrap( ~ name, scales = 'free')

boxplot_region

# save all box-plots together------
all_bp <-
  ggarrange(
    boxplot_type,
    boxplot_region,
    bp_side,
    nrow = 3,
    labels = 'auto',
    common.legend = T
  )

ggsave(
  boxplot_type,
  filename = 'figures/biofouling_boxplots.png',
  device = 'png',
  dpi = 300,
  width = 6,
  height = 4
)

#'<H3> Cover, LoF and FR rating by hull depth</H3> 
d_long %>%
  drop_na(hull_depth) %>%
  ggplot(aes(hull_depth, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ name, scales = 'free')

#'<H3> Cover, LoF and FR rating by caw_risk_score</H3> 
d_long %>% 
  ggplot(aes(factor(caw_risk_score) , value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ name, scales = 'free')

#'<H3> Cover, LoF and FR rating by CRMS compliance</H3> 
ggplot(d_long, aes(crms, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~name, scales = 'free')

#'<H3> Cover, LoF and FR rating by sample category</H3> 
ggplot(d_long, aes(category, value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~name, scales = 'free')


#'<H2> Community analyses</H2> 
taxa_long <-
  d %>%
  group_by(vessel_code, type, area, crms) %>%
  summarise(across(clean_hull:other, mean), .groups = 'drop') %>%
  pivot_longer(cols = clean_hull:other) %>%
  mutate(
    name = str_to_sentence(name),
    name = fct_recode(
      name,
      `Green fil. algae` = "Fil_green_algae",
      Macroalgae = "Macro_algae",
      `Clean hull` = "Clean_hull"
    )
  )
#'<H3> Box-plot taxa by crms type</H3> 
# box-plot taxa by crms type----
taxa_long %>% 
  ggplot() +
  geom_boxplot(aes(name, value, fill = crms)) +
  facet_wrap(~type) +
  coord_flip() +
  scale_y_log10()

#'<H3> Bar taxa composition by CRMS and type</H3> 
taxa_bar_plot <- 
taxa_long %>% 
  group_by(crms, type, name) %>% 
  summarise(value = mean(value), .groups = 'drop') %>%
  ggplot() +
  geom_col(aes(crms, value, fill = fct_reorder(name, value)), position = 'fill', alpha = .8) +
  facet_wrap(~type) +
  scale_fill_viridis_d(option = 'A', direction = -1, name = NULL, begin = .08) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = 'CRMS', y = NULL)

taxa_bar_plot
ggsave(taxa_bar_plot,
       filename = 'figures/taxa_bar_plot.png',
       height = 4.6,
       width = 7.3,
       dpi = 600)

#'<H3> Summary table taxa cover by CRMS and type</H3> 
taxa_long %>%
  group_by(name, crms) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    .groups = 'drop'
  ) %>%
  arrange(crms,-mean) %>%
  datatable() %>% 
  formatRound(columns=c(3:6), digits=2)

## transform data ----
taxa_t <-
  d %>%
  # mutate(biofilm = biofilm + clean_hull, .keep = 'unused', .after = 'FR rating')
  group_by(vessel_code, type, crms) %>%
  summarise(across(clean_hull:other, mean), .groups = 'drop') %>%
  mutate(across(clean_hull:other, ~ log(. + 1))) 

taxa_pa <- 
taxa_t %>% 
  mutate_if(is.numeric, ~replace(., . > 0, 1))


#'<H3> Unconstrained ordination by CRMS compliance status</H3> 
mds <- metaMDS(select(taxa_t,clean_hull:other), distance = 'bray', trace = 0)
mds_pa <- metaMDS(select(taxa_pa,clean_hull:other), distance = 'jaccard', trace = 0)

mds_crms <- 
ggord(
    mds,
    poly = F,
    repel = T,
    txt = 3,
    ellipse = F,
    arrow = 0,
    grp_in = taxa_t$crms,
    fill = taxa_t$crms,
    alpha = .5,
    veclsz = .2,
    vec_ext = 0.5,
    grp_title = "Compliance\nstatus"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
mds_crms

mds_ord_pa <- 
  ggord(
    mds_pa,
    poly = F,
    repel = T,
    txt = 3,
    ellipse = F,
    arrow = 0,
    grp_in = taxa_pa$crms,
    fill = taxa_pa$crms,
    alpha = .5,
    veclsz = .2,
    vec_ext = 0.5,
    grp_title = "Compliance\nstatus"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
mds_ord_pa


#'<H3> PERMANOVA by CRMS and type </H3> 
adonis(select(taxa_pa,clean_hull:other)~crms+type, data = taxa_pa)$aov.tab %>% as_tibble() %>% kable(digits = 3)

#'<H3> PERMDISP by type </H3> 
anova(betadisper(dist(select(taxa_pa,clean_hull:other)), taxa_pa$type)) %>% as_tibble() %>% kable(digits = 2)

#'<H3> PERMANOVA by CRMS </H3> 
anova(betadisper(dist(select(taxa_pa,clean_hull:other)), taxa_pa$crms)) %>% as_tibble() %>% kable(digits = 3)

#'<H3> Indicator species of CRMS compliance status</H3> 
# indicator taxa-----
multip_crms <- multipatt(taxa_t[,-c(1:3)], taxa_t$crms)
summary(multip_crms)

#'<H3> Unconstrained ordination by vessel type</H3> 
mds_type <- 
ggord(
  mds_pa,
  poly = F,
  repel = T,
  txt = 3,
  ellipse = F,
  arrow = 0,
  grp_in = taxa_t$type,
  fill = taxa_t$type,
  alpha = .5,
  veclsz = .2,
  vec_ext = 0.5,
  grp_title = "Vessel\ntype"
) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

mds_ord_pa
mds_type

ggsave(ggarrange(mds_ord_pa, mds_type, legend = 'bottom', labels = 'auto'),
       width = 7, height = 3, 
       filename = 'figures/biofouling_mds_plots.png',
       dpi = 300)

