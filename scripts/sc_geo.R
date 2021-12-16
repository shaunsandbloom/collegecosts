library(tidyverse)
library(tidymodels)
library(tidycensus)
library(lubridate)
library(tigris)
library(sf)

# Loading geospatial data
geo_df <- states(cb = TRUE, progress_bar = FALSE) %>% 
  shift_geometry(position = c("below", "outside")) %>% 
  filter(!STUSPS %in% territories) %>% 
  st_transform(crs = 5070)

#### JOINING... ####

df3 <- df2 %>% 
  st_transform(5070) %>% 
  st_join(geo_df, join = st_intersects) %>% 
  group_by(AFFGEOID) %>% 
  summarize(n = n())

#### POTTING... ####
st_join(geo_df, df3, join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +
  theme_void()

# join function...
geo_join <- function(df){
  territories <- c("AS", "FM", "GU", "MH", "MP", "PR", "PR", "PW", "VI")
  geo <- states(cb = TRUE, progress_bar = FALSE) %>% 
    shift_geometry(position = c('below', 'outside')) %>% 
    filter(!STUSPS %in% territories) %>% 
    st_transform(crs = 5070)
  
  df <- df %>% 
    st_transform(5070) %>% 
    st_join(geo, join = st_intersects)
  return(df)  
}

master_df %>% 
  distinct(degree_awd)
