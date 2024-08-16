# endscope data
# percentage cover, species richness
# possible analysis pass/fail vs internals, and sea chest grate vs internals

# 23 May 2022
library(tidyverse)
library(janitor)
library(readr)

# read data
internals <- 
  read_csv(
    'data/internalsdata_riskprofiling_405761_v1.csv')

# clean names
internals <- internals %>%
  clean_names()

# numeric values coming through as character for some reason
i <- c(15:35)
internals[ , i] <- apply(internals[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
# check class
sapply(internals, class)

# filter out endoscope rope guard and failed assessments
internals <- filter(internals, inside_grate == "TRUE", area == "seachest")

# remove unnecessary columns
internals <- select(internals, vessel_id, rrisk, macro_cover:pass_fail_i)

# add richness column
internals <- mutate(internals, taxa_diversity = gooseneck + fil_green_algae + macroalgae + 
                      barnacles + tubeworms + bryozoans + bivalves + hydroids + 
                      anemones + ascidians + othersp)

# plots

ggplot(data = internals) + 
  geom_point(mapping = aes(x = rrisk, y = taxa_diversity))

ggplot(data = internals) + 
  geom_point(mapping = aes(x = rrisk, y = macro_cover))

ggplot(data = internals) + 
  geom_point(mapping = aes(x = rrisk, y = macro_cover))





#### Likely not needed
# Create a tibble from the data frame.
# This could have been done with the next step but obscured the main point.
tb_internals = as_tibble(internals)
tb_internals$rrisk <- as.character(as.numeric(tb_internals$rrisk))  # Convert one variable numeric to character
tb_internals$vessel_id <- as.character(as.numeric(tb_internals$vessel_id))
# We have to assign the RHS to an object to save the column to the object.
# It can be the same as the original tibble.
tb_internals = tb_internals %>% rowwise() %>% mutate(taxa_diversity = sum(c_across(gooseneck:mobiles)))
