library(tidyverse)

brizo_list <- read_csv("Clean-Data/brizo_list.csv")

brizo_list |> 
  select(Phone, `Price Range`,State, Cuisines) |> 
  drop_na(Phone)-> price_range_matching



december_dm_lead <- read_csv("Clean-Data/december_dm_lead.csv")

december_dm_lead |> 
  mutate(Phone = as.character(`Mobile - Primary`),
         `Price Range`= "NA",
         State = "NA", 
         Cuisines= "NA") |> 
  filter(is.na(Phone)) -> dm_lead_na

december_dm_lead |> 
  mutate(Phone = as.character(`Mobile - Primary`)) |> 
  drop_na(Phone) |> 
  left_join(price_range_matching, by = "Phone") -> matched_dm_lead

matched_dm = rbind(matched_dm_lead, dm_lead_na)

matched_dm |> 
  select(flow, types, mql, sql, cw, `Price Range`, everything())-> matched_dm

write_csv(matched_dm,"cuisines_type/december_dm_lead_brizo.csv", na="")  