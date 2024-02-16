library(tidyverse)

unique_scan <- read_csv("Clean-Data/unique_scan.csv") # has snowball
clean_form_submission <- read_csv("Clean-Data/clean_form_submission.csv") # has snowball
clean_inbound_call <- read_csv("Clean-Data/clean_inbound_call.csv") 

drop_1_lead <- read_csv("Clean-Data/december_dm_lead.csv") |> 
  rename("Phone" = "Mobile - Primary") |> 
  mutate("Phone"=as.character(Phone)) |> 
  filter(drop_group == "Drop 1 - US")



list_a_only_once <- read_csv("Clean-Data/list_a_only_once.csv") |> 
  mutate(Phone=as.character(Phone))->list_a_only_once 



drop_2 <- read_csv("Raw-Data/Drop 2.csv") |> 
  mutate(Phone=as.character(Phone)) |> 
  rename("snowball_id" = `Snowball Map`) |> 
  mutate(snowball_id = sprintf("%05d", as.integer(as.character(snowball_id)))) |> 
  mutate(Phone=as.character(Phone))-> drop_2


unique_scan |> 
  filter(drop_group == "Drop 1 - US") |> 
  anti_join(drop_2,by = "snowball_id") -> scan_drop1_only_once

scan_drop1_only_once |> 
  group_by(drop_group) |> 
  summarise(counts=n())


clean_form_submission |> 
  rename("snowball_id" = "Snowball") |> 
  filter(drop_group == "Drop 1 - US") |> 
  anti_join(drop_2,by = "snowball_id") |> 
  mutate(Phone = as.character(Phone))-> clean_form_submission_no2 #also mql


write_csv(clean_form_submission,"Clean-Data/drop1_no_drop_2_form_submittion.csv")
december_dm_lead |> 
  filter(drop_group=="Drop 1 - US") |> 
  filter(mql==TRUE) ->x

december_dm_lead |> 
  filter(drop_group=="Drop 1 - US") |> 
  filter(mql==TRUE) |> 
  semi_join(list_a_only_once,by = "Phone") ->xx

december_dm_lead %>%
  mutate(`Business Phone` = as.character(`Business Phone`)) |> 
  filter(drop_group == "Drop 1 - US") %>%
  filter(mql == TRUE) %>%
  anti_join(list_a_only_once, by = c("Business Phone"= "Phone")) -> xxx # no match


december_dm_lead |> 
  mutate(`Business Phone` = as.character(`Business Phone`)) |> 
  filter(drop_group=="Drop 1 - US") |> 
  filter(mql==TRUE) |> 
  semi_join(list_a_only_once,by = c("Business Phone"= "Phone")) ->xxxx # has match




december_dm_lead %>%
  mutate(`Business Phone` = as.character(`Business Phone`)) |> 
  filter(drop_group == "Drop 1 - US") %>%
  filter(mql == TRUE) %>%
  drop_na(`Business Phone`) |> 
  anti_join(drop_2, by = c("Business Phone"= "Phone")) -> did_not_receive_drop_2 # no match

december_dm_lead %>%
  mutate(`Business Phone` = as.character(`Business Phone`)) |> 
  filter(drop_group == "Drop 1 - US") %>%
  filter(mql == TRUE) %>%
  drop_na(`Business Phone`) |> 
  semi_join(drop_2,by = c("Business Phone"= "Phone")) -> receive_drop_2 # has match


write_csv(did_not_receive_drop_2,"Clean-Data/Drop_1_no_Drop2.csv")


jot_form_lead <- read_csv("Raw-Data/jot_form_lead.csv")
drop_2 |> 
  semi_join(jot_form_lead, by ='snowball_id') -> showed


clean_inbound_call <- read_csv("Clean-Data/clean_inbound_call.csv") |> 
  mutate(Phone=as.character(Phone))

clean_inbound_call |> 
  anti_join(drop_2, by = "Phone") |> 
  group_by(drop_group) |> 
  summarise(counts=n())-> did_not_receive_drop_2_ibc


clean_inbound_call |> 
  anti_join(drop_2, by = "Phone") -> did_not_receive_drop_2_ibc_raw_data

write_csv(did_not_receive_drop_2_ibc_raw_data,"Clean-Data/did_not_receive_drop_2_ibc_raw_data.csv")

clean_inbound_call |> 
  group_by(drop_group) |> 
  summarise(counts=n())-> ibc_summary


did_not_receive_drop_2_ibc |> 
  mutate(label="did_not_receive_drop_2_ibc") ->did_not_receive_drop_2_ibc

ibc_summary |> 
  mutate(label="ibc_summary")->ibc_summary


rbind(did_not_receive_drop_2_ibc2,ibc_summary2) -> diff_inc
