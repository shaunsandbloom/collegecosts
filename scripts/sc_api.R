library(tidyverse)
library(readxl)
library(rscorecard)
library(httr)
library(jsonlite)

source("scripts/sc_key.R")

# Reference...
# https://www.youtube.com/watch?v=fdg0UbfOi68&lc=z230wrqrrzr4wllin04t1aokgkwqnanosr2p4ytrwnupbk0h00410

# Max
# https://github.com/maxkapur/CollegeScorecardTutorial/blob/main/CollegeScorecardTutorial.ipynb

# U.S Department of Education College Scorecard
# https://collegescorecard.ed.gov/data/documentation/

# More info on Gov Open Data API
# https://github.com/RTICWDT/open-data-maker/blob/master/API.md

# 2022 Best Engineering School
# https://www.usnews.com/best-graduate-schools/top-engineering-schools/eng-rankings

#### API QUERY FUNCTION ####
query_scorecard <- function(endpoint, filters, fields, per_page, page){
  api_path <- "https://api.data.gov/ed/collegescorecard/v1/"
  query <- paste0(
    api_path, 
    endpoint,
    ".json?",
    filters, 
    "&fields=", 
    paste(fields, collapse = ","),
    "&per_page=",
    per_page,
    "&page=",
    page,
    "&api_key=", 
    sc_key
    )
  print("Querying...")
  response <- GET(query)
  if(http_error(response)) {
    print("Query failed.")
    print(http_status(response)$message)
  } else {
    print("Query confirmed.")
    print(paste("Data type...", http_type(response)))
    
    content <- content(response, as = "text")
    results <- fromJSON(content)
  }
  results2 <- results$results
  return(results2)
}

### UNIVERISTY INFO QUERY ####
filters <- ""
fields <- c(
  "id",
  "school.name",
  "school.ownership",
  "school.city",
  "school.state",
  "school.ownership",
  "location.lat",
  "location.lon"
)

# Looping over empty table and adding new rows from each query to it...
schools <- tibble()

for (i in seq(1:100)) {
  df <- query_scorecard(
    endpoint = "schools",
    filters = filters,
    fields = fields,
    per_page = 100,
    page = i
  )
  schools <- schools %>%
    bind_rows(df)
  print(paste("Total Rows:", dim(schools)[1]))
}

glimpse(schools)

write_csv(
  schools,
  file = "data/sc_school_info.csv",
  col_names = TRUE,
  quote = "needed",
  escape = "backslash",
  eol = "\n"
)


#### COSTS QUERY ####
cost_query <- function(year, fields, pages){
  
  # create empty tibble...
  results <- tibble()
  
  # customize the fields...
  params <- paste0(year, ".", fields)
  
  # loop over `query_scorecard()`...
  for (i in pages){
    
    # build the query...
    df <- query_scorecard(
      endpoint = "schools",
      filters = "",
      fields  = c("id",params),
      per_page = 100,
      page = i
    )
    results <- results %>% 
      bind_rows(df)
    print(paste("Total rows:", dim(results)[1]))
    
    Sys.sleep(5)
  }
  final_results <- results %>% 
    pivot_longer(
      cols = -id,
      names_to = "keys",
      values_to = "values"
    ) %>% 
    separate(keys, into = c('year', 'keys'), sep = 4) %>% 
    pivot_wider(
      names_from = keys,
      values_from = values
    )
  return(final_results)
}
#### COSTS ####
years <- seq(1950, 2019)
pages <- seq(1, 66)
fields <- c(
  'cost.attendance.academic_year',
  'cost.attendance.program_year',
  'cost.avg_net_price.public',
  'cost.avg_net_price.private',
  'cost.tuition.in_state',
  'cost.tuition.out_of_state',
  'cost.tuition.program_year',
  'school.tuition_revenue_per_fte',
  'school.instructional_expenditure_per_fte'
)

for (year in seq(2017, 1980)){
  print(paste("Requesting", year, "data..."))
  df <- cost_query(year, fields, pages)
  print(paste(year, "data collected..."))
  print("Appending to file...")
  write_csv(df, file = "data/sc2019costs.csv", append = TRUE)
}

#### PROGRAM PCT ####

# set pages...
pages <- seq(1,66)
# set years...
years <- seq(2017, 1996)
fields <- prog_pct

# initiate empty table
schools <- tibble()

for (year in years){
  params <- paste0(year,'.',fields)
  params <- c("id", params)
  schools <- tibble()
  for(i in pages) {
    print(paste("Year:", year, "..."))
    df <- query_scorecard(
      endpoint = "schools",
      filters = "",
      fields = params,
      per_page = 100,
      page = i
    )
    Sys.sleep(5)
    schools <- schools %>% 
      bind_rows(df)
    print(paste("Total rows...", dim(schools)[1]))
  }
  # transform `schools`
  schools  <- schools %>% 
    select(id, everything()) %>% 
    pivot_longer(cols = -id, names_to = 'key', values_to = 'pct') %>% 
    separate(col = key, into = c('year', 'prog'), extra = 'merge') %>% 
    mutate(prog = str_replace(prog, "academics.", "")) %>% 
    separate(prog, into = c("prog", "degree_awd"), sep = 19) %>%
    filter(pct > 0) %>% 
    select(-prog)
  # append `schools` to master file
  write_csv(schools, file = "data/sc_programs.csv", append = TRUE)
  print(paste("Master file updated:", year))
}

#### CARNEGIE ####

pages <- seq(1,66)
# set years...
fields <- c("school.locale", "school.carnegie_undergrad", "school.carnegie_size_setting")

schools2 <- tibble()
for (year in c(1996)){
  params <- paste0(year,'.',fields)
  params <- c("id", params)
  for(i in pages) {
    print(paste("Year:", year, "..."))
    print(paste("Page:", i, "..."))
    df <- query_scorecard(
      endpoint = "schools",
      filters = "",
      fields = params,
      per_page = 100,
      page = i
    )
    Sys.sleep(5)
    schools2 <- schools2 %>% 
      bind_rows(df)
    print(paste("Total rows...", dim(schools2)[1]))
  }
}

schools <- schools %>% 
  select(id, everything()) %>% 
  pivot_longer(cols = -1, names_to = "key", values_to = "value") %>%
  mutate(key = str_replace(key, "school.", "")) %>%
  separate(key, into = c('year', 'category'), extra = 'merge') %>%
  select(-year) %>% 
  left_join(dd, by = c('category' = 'dev_name', 'value' = 'value')) %>% 
  mutate(label = na_if(label, "Not applicable"))

schools
write_csv(schools, file = "data/carnegie.csv")

# dd <- read_csv("data/carnegie_dd.csv")

#### RECAP ####
# what do we have?
# school info
# year
# cost
# geo
# ownership
# % degree awarded

pages <- seq(1,66)
fields = c("student.size")

# schools2 <- tibble()
for (year in seq(2007, 1996)){
  params <- paste0(year,'.',fields)
  params <- c("id", params)
  for(i in pages) {
    print(paste("Year:", year, "..."))
    print(paste("Page:", i, "..."))
    df <- query_scorecard(
      endpoint = "schools",
      filters = "",
      fields = params,
      per_page = 100,
      page = i
    )
    df <- df %>%
      mutate(year = year) %>% 
      rename('student.size' = params[2])
    Sys.sleep(5)
    schools2 <- schools2 %>% 
      bind_rows(df)
    print(paste("Total rows...", dim(schools2)[1]))
  }
}