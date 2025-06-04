library(tidyverse)

phone_total <- read_csv("clean_data/sms_list.csv")


phone_total |> 
  group_by(State, list) |> 
  summarise(counts=n()) |> 
  pivot_wider(names_from = list, values_from = counts) -> summary_table


write_csv(summary_table,"clean_data/summary_table.csv")
