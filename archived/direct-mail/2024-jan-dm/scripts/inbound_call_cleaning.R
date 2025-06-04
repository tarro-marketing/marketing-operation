library(tidyverse)

jan_inbound_call_notes <- read_csv("raw_data/jan_inbound_call_notes.csv")

# Create the dataframe
campaign_phone_number <- data.frame(Phone = c("8885200811", "8885753251", "8885200559", "8882353802", "8882353802", "8882353802"), 
                                    list_source = c("Brizo_US", "Old_List_US", "Print_Shop_US", "Brizo_CAN", "Oldlist_CAN", "Print_Shop_CAN"))


dm <- jan_inbound_call_notes |> 
  filter(`Lead Status`!= "Internal") |> 
  filter(`Extension Number`%in% campaign_phone_number$Phone) |> 
  mutate(`Extension Number` = as.character(`Extension Number`),
         list_source = case_when(
           `Extension Number` == "8885200811" ~ "Brizo_US",
           `Extension Number` == "8885753251" ~ "Old_List_US",
           `Extension Number` == "8885200559" ~ "Print_Shop_US",
           `Extension Number` == "8882353802" ~ "CAN",
           TRUE ~ NA_character_ )) |> 
  filter(Date <= "12/27/2023"& Date >="01/31/2024") |> 
  select(list_source, Date, everything())

write_csv(dm,"clean_data/inboundcall_dm_jan.csv", na = "")
 
