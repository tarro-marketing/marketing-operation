library(tidyverse)

brizo <- read_csv("clean_data/clean_individual_list/brizo_list_sms.csv", 
                  col_types = cols(`Area Code` = col_character()))
printshop <- read_csv("clean_data/clean_individual_list/Print Shop.csv", 
                      col_types = cols(`Area Code` = col_character()))
inboud_call <- read_csv("clean_data/clean_individual_list/inboud_call_follow_up_new.csv")
wondersleadqueue_new <- read_csv("clean_data/clean_individual_list/wondersleadqueue_new.csv")
masterlist <- read_csv("clean_data/clean_individual_list/masterlist.csv")



brizo |> 
  group_by(Country) |> 
  summarise(counts=n()) -> brizo

printshop <- printshop |> 
  group_by(Country) |> 
  summarise(counts=n())

inboud_call <- inboud_call |> 
  group_by(Country) |> 
  summarise(counts=n())

wondersleadqueue_new <- wondersleadqueue_new |> 
  group_by(Country) |> 
  summarise(counts=n())

masterlist <- masterlist |> 
  group_by(Country) |> 
  summarise(counts=n())