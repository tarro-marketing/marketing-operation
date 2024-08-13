library(tidyverse)



April_SMS_SAM_List <- read_csv("wonders nurture queue/April SMS SAM List.csv")

SMS_Incremental <- read_csv("wonders nurture queue/Marketing Nurture Queue 4.24.24 - SMS Incremental.csv")



current_sms <- April_SMS_SAM_List |> 
  mutate(Phone = str_remove_all(Phone, "[[:punct:][:space:]]")) 
  

sms_incremental <- SMS_Incremental |> 
  mutate(`Mobile - Primary` = str_remove_all(`Mobile - Primary`,  "[[:punct:][:space:]]"),
         `Business Phone` = str_remove_all(`Business Phone`, "[[:punct:][:space:]]")) |> 
  anti_join(current_sms, by = c(`Mobile - Primary`= "Phone" )) |> 
  mutate(`Mobile - Primary` = sub("(\\d{3})(\\d{3})(\\d{4})", "(\\1) \\2-\\3", `Mobile - Primary` ),
         `Business Phone` =sub("(\\d{3})(\\d{3})(\\d{4})", "(\\1) \\2-\\3", `Mobile - Primary` ))-> sms_incremental


write_csv(sms_incremental, "wonders nurture queue/SMS (Deduped).csv", na = "")
