library(tidyverse)
library(tidymodels)
library(tidycensus)
library(lubridate)
library(tigris)
library(sf)

source("scripts/sc_geo.R")

master_df <- read_csv("data/sc_final.csv")
master_car <- read_csv("data/carnegie_master.csv")
master_prog <- read_csv("data/sc_programs.csv")

glimpse(master_df)
glimpse(master_car)
glimpse(master_prog)

master_final <- master_df %>% 
  select(-tuition_rev_per_fte, -expenditure_per_fte) %>% 
  right_join(master_car, by = 'id') %>% 
  right_join(master_prog, by = c('year', 'id')) %>% 
  filter(school.ownership != 3,
         !is.na(degree_awd)) %>% 
  mutate(school.ownership = as.factor(if_else(school.ownership == 1, "public", "private"))) %>% 
  pivot_wider(names_from = degree_awd, values_from = pct) %>% 
  pivot_longer(cols = c(tuition_in_state, tuition_out_state, tuition_program_year),
               names_to = "tuition_type",
               values_to = "tuition_rate") %>%
  filter(!is.na(tuition_rate)) %>% 
  rename(name = school.name,
         city = school.city,
         state = school.state,
         ownership = school.ownership,
         size = school_size,
         size_details = school_size_details,
         ) %>% 
  arrange(desc(year), id)

glimpse(master_final)

write_csv(master_final, "data/master_final,csv")

# 651,663
# 1,005,903
# 630,436
  