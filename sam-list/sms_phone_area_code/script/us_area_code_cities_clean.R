library(tidyverse)
us_states_abbreviation <- read_csv("raw_data/us_states_abbreviation.csv")
us_area_code_cities <- read_csv("raw_data/us-area-code-cities.csv")


us_area_code_cities_clean <- us_area_code_cities |> 
  select(-Country) |> 
  left_join(us_states_abbreviation, by = "State") |> 
  distinct()


write_csv(us_area_code_cities_clean, "clean_data/us_area_code_cities.csv")
