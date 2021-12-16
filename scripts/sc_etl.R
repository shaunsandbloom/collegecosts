library(tidyverse)
df <- read_csv("data/sc2019costs.csv") %>% 
  arrange(desc(year))

new_df <- df %>% 
  mutate(
    attendance = case_when(
      is.na(.cost.attendance.academic_year) ~ .cost.attendance.program_year,
      is.na(.cost.attendance.program_year) ~ .cost.attendance.academic_year
      ),
    avg_net_price = case_when(
      is.na(.cost.avg_net_price.public) ~ .cost.avg_net_price.private,
      is.na(.cost.avg_net_price.private) ~ .cost.avg_net_price.public
    )) %>% 
  select(
    id,
    year,
    "tuition_in_state" = .cost.tuition.in_state,
    "tuition_out_state" = .cost.tuition.out_of_state,
    "tuition_program_year" = .cost.tuition.program_year,
    "tuition_rev_per_fte" = .school.tuition_revenue_per_fte,
    "expenditure_per_fte" = .school.instructional_expenditure_per_fte
  )

final_df <- new_df %>% 
  left_join(schools, by = "id") %>% 
  select(
    year,
    id,
    school.name,
    tuition_in_state,
    tuition_out_state,
    tuition_program_year,
    tuition_rev_per_fte,
    expenditure_per_fte,
    school.city,
    school.state,
    school.ownership,
    location.lat,
    location.lon
  ) %>% 
  filter(year > 1995)

# write_csv(final_df, file = "data/sc_final.csv")

df1 <- read_csv("data/sc_final.csv")
df2 <- read_csv("data/sc_programs.csv")

df3 <- df1 %>% 
  left_join(df2, by = c("id", "year")) %>% 
  filter(is.na(degree_awd)) %>% 
  rename(
    school = school.name,
    t_in = tuition_in_state,
    t_out = tuition_out_state,
    t_pyear = tuition_program_year,
    rev_per_fte = tuition_rev_per_fte,
    exp_per_fte = expenditure_per_fte,
    city = school.city,
    state = school.state
  )