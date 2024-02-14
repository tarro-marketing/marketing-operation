library(tidyverse)


SAM_Brizo <- read_csv("raw_data/February 2024 DM Mailing List SAM - Brizo - All.csv") |> 
  mutate(phone = str_remove_all(Phone,"[:punct:]|[:space:]")) |> 
  select(Phone,phone,`State/Province Code`, `City/Town`, `Zip/Postal Code`)

SMS_Brizo <- read_csv("clean_data/Brizo.csv") |> 
  rename(`Phone` = `phone`) |> 
  mutate(phone = str_remove_all(Phone,"[:punct:]|[:space:]"))





distinct_sam <- SAM_Brizo |> 
  distinct(phone) 
sam_brizo_matching <- SAM_Brizo |> 
  filter(phone %in% distinct_sam$phone) |> 
  group_by(phone) |> 
  filter(row_number() == 1)  |> 
  ungroup() 

distinct_sms <- SMS_Brizo |> 
  distinct(phone) 

sms_brizo_matching <- SMS_Brizo |> 
  filter(phone %in% distinct_sms$phone)

match_brizo <- sms_brizo_matching |> 
  left_join(sam_brizo_matching, by = "phone") |> 
  select(Phone.x,`Area Code`, State, `State Abbreviation`, Phone.y, `State/Province Code`,`City/Town`,`Zip/Postal Code`,Country)


sum(is.na(match_brizo$Phone.y))


BRIZO_LIST_SMS <- match_brizo |> 
  mutate(state_group = case_when(
    is.na(`State/Province Code`) ~ `State Abbreviation`,
    TRUE ~ as.character(`State/Province Code`)
  ))

write_csv(BRIZO_LIST_SMS, "clean_data/brizo_list_sms.csv")
