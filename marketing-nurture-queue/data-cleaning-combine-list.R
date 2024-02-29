library(tidyverse)

setwd("~/PerformanceMarketing/list-matching/marketing-nurture-queue")

us_area_code <-read_csv("~/PerformanceMarketing/list-matching/us-area-code/area_code.csv",
                        col_types = cols(`Area_Code` = col_character())) 
  
SMS_List_0111_Group_1_SQL_Gift_ <- read_csv("data/SMS List 0111 - Group 1 (SQL Gift).csv")
SMS_List_0111_Group_2_CW_Gift_ <- read_csv("data/SMS List 0111 - Group 2 (CW Gift).csv")
SMS_List_0111_Group_3_No_Gift_ <- read_csv("data/SMS List 0111 - Group 3 (No Gift).csv")

sms_list=rbind(SMS_List_0111_Group_1_SQL_Gift_,SMS_List_0111_Group_2_CW_Gift_,SMS_List_0111_Group_3_No_Gift_)

# sms_list_unique <- sms_list |> 
#   distinct(Phone, .keep_all = TRUE)
# 
# # duplicate phone numbers 46420
# # unique total = 88525
# 
# 
# sms_list_unique <- sms_list_unique %>%
#   mutate(Is10Digits = if_else(nchar(Phone) == 10, TRUE, FALSE)) |> 
#   filter(Is10Digits== TRUE) |> 
#   select(-Is10Digits)
# 
# ## usable phone 85524

write_csv(sms_list, "clean-data/sms-list.csv",na="")

marketing_nurture_queue <- read_csv("data/marketing-nurture-queue-new.csv", 
                                        col_types = cols(MobilePrimary = col_character(), 
                                                         BusinessPhone = col_character()))

marketing_nurture_queue <- marketing_nurture_queue  |> 
  drop_na(MobilePrimary) |> 
  mutate(list_source="wonders-lead-queue",
         `Area Code` = str_extract(MobilePrimary,"^\\d{3}"),
         Is10Digits = if_else(nchar(MobilePrimary) == 10, TRUE, FALSE)) |> 
  filter(Is10Digits==TRUE) |> 
  left_join(us_area_code, by =c("Area Code"="Area_Code")) |> 
  rename(Phone=MobilePrimary) 

marketing_nurture_queue <- marketing_nurture_queue |> 
  mutate(State = case_when(is.na(`State/Province (text only)`) ~ State_Area,
                           TRUE ~ `State/Province (text only)`)) |> 
  filter(Country=="United States") |> 
  distinct(Phone, .keep_all = TRUE)


marketing_nurture_queue_sms_list <- marketing_nurture_queue |> 
  select(Phone, State, list_source,`Area Code`)  |> 
  distinct(Phone, .keep_all = TRUE)


  

write_csv(marketing_nurture_queue,"clean-data/marketing_nurture_queue.csv", na = "")
write_csv(marketing_nurture_queue_sms_list,"clean-data/marketing_nurture_queue_sms_list.csv",na = "")



