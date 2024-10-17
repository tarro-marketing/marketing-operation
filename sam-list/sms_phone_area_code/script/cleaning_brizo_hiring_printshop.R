library(tidyverse)
Brizo <- read_csv("raw_data/Brizo.csv", col_types = cols(
  phone = col_character(),
  date_updated = col_double()))
Hiring_Post <- read_csv("raw_data/Hiring Post.csv", col_types = cols(
  phone = col_character(),
  date_updated = col_double()))
Print_Shop <- read_csv("raw_data/Print Shop.csv", col_types = cols(
  phone = col_character(),
  date_updated = col_double()))

Brizo <- Brizo |> 
  mutate(phone = as.character(phone),
         phone = str_remove(phone, "^1"),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())


Hiring_Post <- Hiring_Post |> 
  mutate(phone = as.character(phone),
         phone = str_remove(phone, "^1"),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())

Print_Shop <- Print_Shop |> 
  mutate(phone = as.character(phone),
         phone = str_remove(phone, "^1"),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())

############################################
phone_area_code_cities <- read_csv("clean_data/information/phone_area_code_cities.csv", 
                                   col_types = cols(`Area Code` = col_character()))|> 
  distinct()
############################################
Brizo <- Brizo |> 
  left_join(phone_area_code_cities, by = "Area Code") |> 
  select(phone, `Area Code`, State,`State Abbreviation`, 
         Country, everything())
##########################################

Hiring_Post <- Hiring_Post |> 
  left_join(phone_area_code_cities, by = "Area Code") |> 
  select(phone, `Area Code`, State,`State Abbreviation`, 
         Country, everything())

##########################################

print_shop_new <- Print_Shop |> 
  left_join(phone_area_code_cities, by = "Area Code") |> 
  select(phone, `Area Code`, State,`State Abbreviation`, 
         Country, everything())



write_csv(brizo_new,"clean_data/Brizo.csv",na = "")
write_csv(hiring_post_new,"clean_data/Hiring Post.csv",na = "")
write_csv(print_shop_new,"clean_data/Print Shop.csv",na = "")
