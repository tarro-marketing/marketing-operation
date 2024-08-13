library(tidyverse)


wonders_lead_queue <- read_csv("sms list matching/Wonders Lead Queue __ SAM List - Wonders Lead Queue - All (No Match).csv")

sms_list <- read_csv("sms list matching/SMS List 0111 - Brizo.csv")

sms_list <- sms_list |> 
  mutate(Phone.x = str_remove_all(Phone.x, "-"),
         Phone.x = str_remove_all(Phone.x, "[^0-9]"))

wonders_lead_queue <- wonders_lead_queue |> 
  drop_na(`Mobile - Primary`) |> 
  mutate(`Mobile - Primary` = str_remove_all(`Mobile - Primary`, "-"),
         `Mobile - Primary` = str_remove_all(`Mobile - Primary`, "[^0-9]"),
         `Mobile - Primary` = str_remove(`Mobile - Primary`, "^1"))


wonders_lead_queue |> 
  anti_join(sms_list, by = c("Mobile - Primary" = "Phone.x"))-> net_new_phone_numbers

wonders_lead_queue |> 
  semi_join(sms_list, by = c("Mobile - Primary" = "Phone.x"))-> common_new_phone_numbers



net_new_phone_numbers <- net_new_phone_numbers |> 
  mutate(`Mobile - Primary` = str_replace_all(
    `Mobile - Primary`,"^(\\d{3})(\\d{3})(\\d{4})$","(\\1) \\2-\\3"))


write_csv(net_new_phone_numbers,"cleaned_data/wonders_lead_queue_net_new_sms.csv",na="")
