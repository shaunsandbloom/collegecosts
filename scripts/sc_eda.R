library(tidyverse)
library(tidymodels)
library(tidycensus)
library(lubridate)
library(tigris)
library(sf)

#### LOAD DATA ####

df1 <- read_csv("data/sc_final.csv")
df2 <- read_csv("data/sc_programs.csv")

territories <- c("AS", "FM", "GU", "MH", "MP", "PR", "PR", "PW", "VI")

#### JOIN DATA ####
master_df <- df1 %>% 
  left_join(df2, by = c("id", "year")) %>% 
  rename(
    school = school.name,
    t_in = tuition_in_state,
    t_out = tuition_out_state,
    t_pyear = tuition_program_year,
    rev_per_fte = tuition_rev_per_fte,
    exp_per_fte = expenditure_per_fte,
    city = school.city,
    state = school.state,
    ownership = school.ownership
  ) %>% 
  filter(
    !state %in% territories,
    !is.na(location.lat),
    !is.na(location.lon),
    ownership != 3
  ) %>% 
  st_as_sf(coords = c('location.lon', 'location.lat'), crs = 4326)
  
#####

cdd <- read_csv("data/carnegie_dd.csv")

cdd_size <- cdd %>% 
  filter(dev_name == 'carnegie_size_setting')%>% 
  separate(label, 
           into = c('school_type', 'school_size'), 
           sep = ',', 
           extra = 'merge') %>% 
  filter(!is.na(school_size),
         school_type == "Four-year") %>% 
  mutate(school_size = str_trim(school_size)) %>% 
  separate(school_size,
           into = c("school_size", "school_size_details"),
           sep = ", ",
           extra = 'merge')

cdd_locale <- cdd %>% 
  filter(dev_name == "locale") %>% 
  separate(label, 
           into = c('locale_type', 'locale_size'), 
           sep = ':', 
           extra = 'merge') %>% 
  mutate(locale_size = str_trim(locale_size),
         locale_size = str_replace(locale_size, '\\s*\\([^\\)]+\\)', '')) %>% 
  separate(locale_size,
           into = 'locale_size',
           sep = ' ',
           extra = 'drop')

carnegie <- read_csv("data/carnegie.csv")
glimpse(master_df)

glimpse(carnegie)
glimpse(cdd_size)
glimpse(cdd_locale)

master_carnegie <- carnegie %>% 
  filter(!is.na(label)) %>% 
  select(-label) %>% 
  pivot_wider(names_from = category, values_from = value) %>% 
  left_join(cdd_size, by = c(carnegie_size_setting = "value")) %>% 
  select(-dev_name) %>% 
  left_join(cdd_locale, by = c(locale = "value")) %>% 
  select(-dev_name) %>% 
  filter(!is.na(school_type)) %>% 
  mutate(school_size = as.factor(school_size),
         school_size_details = as.factor(school_size_details),
         locale_type = as.factor(locale_type),
         locale_size = as.factor(locale_size)) %>%
  select(-locale, -carnegie_undergrad, -carnegie_size_setting, -school_type)

write_csv(master_carnegie, "data/carnegie_master.csv")

# student.size, student.enrollment.all