library(tidyverse)

inboud_call_follow_up <- read_csv("raw_data_added/Inbound Call - Need SMS Followup.csv")

hiring_post_update <- read_csv("raw_data_added/Hiring post Jan.csv")





inboud_call_follow_up <- inboud_call_follow_up |> 
  rename(phone = Phone) |> 
  mutate(phone = as.character(phone),
         phone = str_remove(phone, "^1"),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())


hiring_post_update <- hiring_post_update |> 
  rename(phone = Phone) |> 
  mutate(phone = as.character(phone),
         phone = str_remove(phone, "^1"),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())



############################################
phone_area_code_cities <- read_csv("clean_data/information/phone_area_code_cities.csv", 
                                   col_types = cols(`Area Code` = col_character()))|> 
  distinct()
############################################
inboud_call_follow_up_new <- inboud_call_follow_up |> 
  left_join(phone_area_code_cities, by = "Area Code") |> 
  select(phone, `Area Code`, State,`State Abbreviation`, 
         Country, everything()) |> 
  drop_na(`Area Code`)


write_csv(inboud_call_follow_up_new,"clean_data/clean_individual_list/inboud_call_follow_up_new.csv",na = "")

