# load libraries and clean env.---------
library(tidyverse)
library(janitor)
library(readxl)
filter <- dplyr::filter
rm(list = ls())

# get CRMS compliance assessment -----
crms_caw21 <-
  read_excel(
    "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/1. Vessel sampling communications/VesselSelection_and_Tracking.xlsx",
    "Risk level"
  ) %>%
  select(Name, 'Pass/Fail') %>%
  rename(vessel = Name,
         crms = "Pass/Fail") %>%
  mutate(
    vessel = str_to_title(vessel),
    vessel = fct_recode(
      vessel,
      `Maersk Garonne` = "Maersk Garrone",
      Alcyone = "Alycone",
      `Seaspan Hamburg` = "Seaspan"
    )
  )

tabyl(crms_caw21, crms)


# get vessel raw data----
caw21_raw <-
  read_excel(
    "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/2. Field data/VesselBiofoulingMaster.xlsx",
    na = "NA"
  ) %>%
  clean_names() %>%
  mutate(vessel = str_to_title(vessel))


# q1uality check data----
glimpse(caw21_raw)

names(caw21_raw)

# check  character
table(caw21_raw$mobiles_write_in)

# check area 
tabyl(caw21_raw, area_8)

tabyl(caw21_raw, region_st_mid_bow)

# clean data -----
caw21_clean <- 
  caw21_raw %>% 
  mutate(area = fct_recode(area_8, # create an area that encompass subareas
                         `Bilge keel`	=	"Bilge keel"	,
                         `Thruster`	=	"Bow thruster"	,
                         `Bulbous bow`	=	"Bulbous bow"	,
                         `Thruster`	=	"Stern thruster"	,
                         # Other	=	"Anode"	,
                         Anode =	"ICCP"	,
                         DDSS	=	"Dock block"	,
                         DDSS	=	"Dock blocks"	,
                         Hull	=	"Draft marks"	,
                         Hull	=	"Hull",
                         Other	=	"Transducer",
                         `Propeller and shaft`	=	"Propeller"	,
                         `Propeller and shaft`	=	"Propeller blade"	,
                         `Propeller and shaft`	=	"Propeller boss"	,
                         `Propeller and shaft`	=	"Propeller shad"	,
                         `Propeller and shaft`	=	"Propeller shaft"	,
                         `Propeller and shaft`	=	"Rope guard"	,
                         `Rudder and shaft`	=	"Rudder"	,
                         `Rudder and shaft`	=	"Rudder B"	,
                         `Rudder and shaft`	=	"Rudder H"	,
                         `Rudder and shaft`	=	"Rudder L"	,
                         `Rudder and shaft`	=	"Rudder S"	,
                         `Rudder and shaft`	=	"Rudder T"	,
                         `Rudder and shaft`	=	"Rudder Tr"	,
                         `Sea chest gratings`	=	"Sea chest"	,
                         `Propeller and shaft`	=	"Stern arch"	,
                         `Thruster`	=	"Thruster"),
         crab = case_when(mobiles_write_in == "Crab"~"1", TRUE ~ "0") %>% as.numeric, # create crab variable
         gastropod = case_when(mobiles_write_in == "Whelks?"~ "1",TRUE~ "0") %>% as.numeric, # create gastropod, ID uncertainty
         vessel = str_to_title(vessel)) %>% 
  select(-mobiles_write_in) %>% # remove mobiles_write_in
  left_join(crms_caw21, by = 'vessel') %>% # join CRMS compliance
  dplyr::filter(area_8 != "ISea chest",
         area_sampled == "TRUE") %>% # remove endoscope data to be analyses separately 
  rename(vessel_code = vessel,
         LoF = lo_f,
         side = side_port_stbd,
         region = region_st_mid_bow,
         subarea = area_8,
         area_type = area_32) %>% 
  mutate(region = fct_recode(region, Midships = "Mid"), # fix region inconsistencies
         # side = fct_recode(side, Port = "P?", Starboard = "St?", Port = "?"), # fix side inconsistencies
         hull_depth = fct_recode(hull_depth, `Flat bottom` = "FB"), # rename flat bottom
         # highest_fr_rating = if_else(highest_fr_rating>100, 50, highest_fr_rating), # fix highest_fr_rating 501 value 
         # LoF = if_else(is.na(LoF) & macro_percent_cover  == 0, 0, LoF), # fix LoF NAs
         # LoF = if_else(is.na(LoF) & macro_percent_cover  == 100, 5, LoF), # fix LoF NAs
         # LoF = if_else(is.na(LoF) & macro_percent_cover  == 15, 3, LoF), # fix LoF NAs
         # LoF = if_else(is.na(LoF) & macro_percent_cover  == 4, 2, LoF), # fix LoF NAs
         clean_hull = if_else(clean_hull<0, 0, clean_hull),
         clean_hull = if_else(is.na(clean_hull) & macro_percent_cover == 0 & LoF ==0 , 100 , clean_hull),
         clean_hull = if_else(is.na(clean_hull) & macro_percent_cover == 0, 100 - biofilm , clean_hull),
         clean_hull = if_else(sample_id == "XING ZHI HAI_Rope guard_Niche_NA_Stern_NA_1",  macro_percent_cover +biofilm , clean_hull),
         biofilm = if_else(is.na(biofilm) & clean_hull == 100, 0, biofilm),
         subarea = fct_collapse(subarea,  DDSS	=	c("Dock block", "Dock blocks")),# clean_hull negative values
         type = fct_recode(type, container = "reefer")) %>%  
  relocate(c(crab,gastropod), .after = ascidians) %>% 
  relocate(area, .before = subarea) %>% 
  relocate(crms, .after = caw_risk_score) %>% 
  relocate(area_type, .after = area) %>% 
  dplyr::filter(sample_id  != "CAP CAPRICORN_Rope guard_Niche_NA_Stern_NA_NA") %>% # missing data due to bad viz
  mutate_at(vars(gooseneck:other_organism_45), ~replace_na(., 0))

# Check for missing values---
naniar::gg_miss_var(caw21_clean) 
glimpse(caw21_clean)
names(caw21_clean)

# check for vars full of NAs
caw21_clean %>% keep( ~ all(is.na(.x))) %>% names

# check factor levels--
tabyl(caw21_clean, area)
tabyl(caw21_clean, subarea)
tabyl(caw21_clean, area_type)
tabyl(caw21_clean, side)
tabyl(caw21_clean, region)
tabyl(caw21_clean, crms)
tabyl(caw21_clean, category)
tabyl(caw21_clean, hull_depth)

# check response variable
tabyl(caw21_clean, LoF)
tabyl(caw21_clean, macro_percent_cover)
tabyl(caw21_clean, highest_fr_rating)# need to sort out missing values here
hist(caw21_clean$macro_percent_cover,
     xlab = "% cover",
     main = NULL)

summary(caw21_clean)

# check the macro_percent_cover variable
caw21_clean %>%
  rowwise() %>%
  mutate(tc = sum(c_across(gooseneck:other), na.rm = T)) %>%
  ggplot(aes(macro_percent_cover , tc)) +
  geom_point()

caw21_clean %>%
  rowwise() %>%
  mutate(tc = sum(c_across(gooseneck:other), na.rm = T)) %>%
  filter(tc != macro_percent_cover) %>%
  select(sample_id)

# check duplicated sample-id----
caw21_clean %>%
  group_by(sample_id) %>%
  dplyr::filter(n() > 1) %>% 
  select(sample_id)# there are 14 duplicated sample id

# save clean biofouling data--------
write_csv(caw21_clean, 'cleaned_data/caw21_clean.csv')