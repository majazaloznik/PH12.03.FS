###############################################################################
## Select subset for plotting
###############################################################################
## preliminaries - libraries and import data
###############################################################################

library(tidyverse)

## import output of 01-Data-Import
full <- read.csv("data/complete.csv")


## select subset - Developing countries that have a policy to Raise fert level in 2015
###############################################################################

full %>% 
  group_by(Country..name) %>% 
  filter(year == 2015,
                Policy.on.fertility.level == "Raise",
                Development == "Developing")  %>% 
  filter(!is.na(Country.Name)) %>% 
  ungroup() %>% 
  select(Country.Name) %>% unlist() -> selection

full %>% 
  filter(Country.Name %in% selection) %>% 
  mutate(Policy.on.fertility.level = as.factor(Policy.on.fertility.level),
         Policy.on.growth = as.factor(Policy.on.growth))-> working.df
