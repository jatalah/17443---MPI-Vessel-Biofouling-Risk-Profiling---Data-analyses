library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(caret)
rm(list = ls())

# Read questionnaire alignment data and names ----------
quest_align <-
  read_excel('data/questionnaire_alignment.xlsx', 1) %>%
  select(1) %>%
  rename(name_2021 = "2021") %>%
  mutate(no_2021 = 1:64) %>%
  right_join(read_excel('data/questionnaire_alignment.xlsx', 3)) %>%
  drop_na(name_2021)


# Read and clean questionnaire 2021 data ----------
quest_21 <-
  read_excel(
    "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/2. Field data/Questionnaire_forms_data.xlsx",
    trim_ws = T
  ) %>% select(-c(1:5)) %>%
  rename_at(vars(quest_align$name_2021), ~ quest_align$question_17) %>%
  mutate(
    Time_since_survey = (`2 Date of arrival` - Last_class_survey) %>% as.numeric(),
    Time_since_AF = (
      `2 Date of arrival` - `27 When was the vessel's current complete antifouling coating applied? \r\nPlease use the date the vessel was refloated, if known`
    ) %>% as.numeric(),
    Time_since_OWM = (`2 Date of arrival` - `Last out-of-water maintenance date`) %>% as.numeric(),
    Multi_AF = if_else(
      `28 How many antifouling coatings are on the submerged surfaces of this vessel?` >
        1,
      "Yes",
      "No"
    ),
    AF_applied = str_trim(tolower(AF_applied))
  ) %>%
  left_join(
    read_excel(
      "C:/Users/javiera/Cawthron/17443 - MPI Vessel Biofouling Risk Profiling - Documents/1. Vessel sampling communications/VesselSelection_and_Tracking.xlsx",
      "Risk level"
    ) %>% # add compliance data
      select(1, 'Pass/Fail') %>%
      rename(`Vessel number` = 1,
             crms = "Pass/Fail")
  )  %>%
  mutate(
    crms = str_to_sentence(crms),
    Cleaning_since_AF = if_else(str_detect(Cleaning_since_AF, "Yes"), "Yes", "No"),
    Hull_works_since_AF = if_else(str_detect(Hull_works_since_AF, "Yes"), "Yes", "No"),
    time_since_cleaning = if_else(
      is.na(
        `43 If answered YES to the previous question - enter the date when maintenance/repair works occurred`
      ),
      Time_since_AF,
      (
        as.numeric(
          `2 Date of arrival`  - `43 If answered YES to the previous question - enter the date when maintenance/repair works occurred`
        )
      )
    ),
    Port_of_registration = str_to_sentence(Port_of_registration),
    Type = fct_recode(Type, Container = "Reefer"),
    `Hull protection` = fct_recode(`Hull protection`, Other = "Pure Epoxy based Anti-Corrosive Paint for Shell (High Solid type)"),
    no_idle_days = as.numeric(
      `54 When was the end date of the idle or laid-up period indicated in question 51?` -
        `53 When was the start date of the idle or laid-up period indicated in question 51?`
    ),
    time_since_idle = as.numeric(
      ymd(`2 Date of arrival`) - ymd(
        `54 When was the end date of the idle or laid-up period indicated in question 51?`
      )
    ),
    time_since_last_port = as.numeric(
      ymd(`2 Date of arrival`) - ymd(
        `57 Date of departure from the last port visited prior to arrival in New Zealand`
      )
    ),
    time_in_last_port = as.numeric(
      ymd(
        `57 Date of departure from the last port visited prior to arrival in New Zealand`
      ) -  ymd(
        `56 Date of arrival at the last port visited prior to arrival in New Zealand`
      )
    ),
    time_since_last_inspection =  as.numeric(ymd(`2 Date of arrival`) - ymd(`63 Enter the date when the last inspection took place`)),
    Regular_intake_treatment = fct_recode(
        Regular_intake_treatment,
        `Every 1 - 3 months` = "Once a month",
        `Every 1 - 3 months` = "Every 2 months",
        `Every 1 - 3 months` = "Every 3 months",
        `Every 1 - 3 months` = "Every 1 - 2 months",
        `Every 3 - 6 months` = "During underwater inspection",
        # `Every 6 - 12 months` = "Every 6-12 months ",
        `Every 1 - 3 months` = "Regularly",
        `Every 1 - 3 months` = "Continously"
      ),
      ports_since_owm = fct_relevel(
        `47 How many different ports has this vessel visited since its most recent out-of-water maintenance?`,
        "0",
        "1 - 5",
        '6 - 10',
        "11 - 20",
        "21 - 50",
        ">50"
      ),
    Set_route = str_to_sentence(Set_route),
    AF_applied = str_to_sentence(AF_applied)
    )

# write questionnaire data 2021 clean--------
write_csv(quest_21, 'cleaned_data/questionnaire_data_2021_clean.csv')

# Read questionnaire 2017 data ----------
quest_17 <- 
  read_excel("data/Cawthron_vessel_data_2017.xlsx", 1, na = "NA") %>% 
  rename(`Vessel number` = Nr, 
         crms = "LOF_fail_mpi" ) %>% 
  mutate(Cleaning_since_AF = if_else(is.na(Cleaning_since_AF), "No", Cleaning_since_AF))

# merge 2017 and 2021 questionnaire data ----
all_quest <- bind_rows(quest_21 = quest_21, quest_17 = quest_17, .id = "Dataset") 

# select and save data to be use for modeling ------
model_data <-
  all_quest %>%
  select(
    `Vessel number`,
    quest_align$question_17,
    crms,
    Time_since_survey:Multi_AF,
    `2 Date of arrival`  ,
    `43 If answered YES to the previous question - enter the date when maintenance/repair works occurred`,
    Dataset
  ) %>%
  select(
    1,
    Dataset,
    crms,
    Type,
    Tonnage_DWT:Max_speed,
    Ports_12m:Max_LU_12m,
    Time_since_survey:Time_since_OWM,
    Cleaning_since_AF,
    Hull_works_since_AF
  ) %>%
  mutate(Cleaning_since_AF = if_else(Cleaning_since_AF=="Yes", 1, 0),
         Hull_works_since_AF = if_else(Hull_works_since_AF=="Yes", 1, 0),
         crms = tolower(crms), 
         Type = fct_recode(Type, Container = "General Cargo")) %>%
  clean_names() %>% 
  dplyr::filter(type != "Fishing") # remove one fishing vessel in the 2017 data 

# check NAs------------
model_data %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

# data imputation ---------
impute_para <-
  preProcess(as.data.frame(model_data[, -c(1:2)]), method = "bagImpute")

model_dat_imp <- predict(impute_para, model_data)

# write model data ie questionnaire data 2017 and 2021----------
write_csv(model_dat_imp, 'cleaned_data/model_data.csv')