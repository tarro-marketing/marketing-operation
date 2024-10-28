library(tidyverse)

brizo <- read_csv("clean_data/clean_individual_list/brizo_list_sms.csv", 
                  col_types = cols(`Area Code` = col_character()))


hiring_post <- read_csv("clean_data/clean_individual_list/Hiring Post.csv", 
                        col_types = cols(`Area Code` = col_character()))

printshop <- read_csv("clean_data/clean_individual_list/Print Shop.csv", 
                      col_types = cols(`Area Code` = col_character()))

hiring_post_update_new <- read_csv("clean_data/clean_individual_list/hiring_post_update_new.csv")
inboud_call_follow_up_new <- read_csv("clean_data/clean_individual_list/inboud_call_follow_up_new.csv")
wondersleadqueue_new <- read_csv("clean_data/clean_individual_list/wondersleadqueue_new.csv")
masterlist_new <- read_csv("clean_data/clean_individual_list/masterlist_new.csv")


colnames(hiring_post)
colnames(hiring_post_update_new)
colnames(masterlist_new)

masterlist_new<- masterlist_new |> 
  select(Phone, State_Name, State, `Area Code`,Country)

hiring_post <- hiring_post |> 
  select(phone, "State Abbreviation", State, `Area Code`,Country) |> 
  rename(Phone = phone,
         State = "State Abbreviation",
         State_Name = State)


hiring_post_update_new <- hiring_post_update_new |> 
  select(phone, "State Abbreviation", State, `Area Code`,Country) |> 
  rename(Phone = phone,
         State = "State Abbreviation",
         State_Name = State)



rbind(masterlist_new, hiring_post, hiring_post_update_new) -> masterlist

masterlist |> 
  mutate(Phone = str_remove_all(Phone,"[:punct:]|[:space:]")) -> masterlist



masterlist |> 
  distinct(Phone, .keep_all = TRUE) -> masterlist

write_csv(masterlist,"clean_data/clean_individual_list/masterlist.csv")
