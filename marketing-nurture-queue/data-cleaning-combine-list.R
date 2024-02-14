library(tidyverse)

setwd("~/PerformanceMarketing/list-matching/marketing-nurture-queue")

us_area_code <-read_csv("~/PerformanceMarketing/list-matching/us-area-code/area_code.csv",
                        col_types = cols(`Area_Code` = col_character())) 
  

brizo <- read_csv("data/Brizo.csv", 
                  col_types = cols(Phone = col_character(), 
                                   PhoneBrizoDM = col_character(),
                                   `Area Code` = col_character())) |> 
  filter(Country=="United States") |> 
  mutate(list_source = "brizo") |> 
  rename(State = state_group,
         State_Name = State) |> 
  select(list_source, Phone, `Area Code`, State) 

inbound_call_sms_follow_up<- read_csv("data/Inbound-Call-SMS-Follow-Up.csv", 
                                       col_types = cols(phone = col_character(), 
                                                        `Area Code` = col_character())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "inbound-call-sms-follow-up") |>
  rename(State = `State Abbreviation`,
         State_name = State,
         Phone = phone) |> 
  select(list_source, Phone, `Area Code`, State)
  
masterlist <- read_csv("data/masterlist.csv", 
                       col_types = cols(Phone = col_character(), 
                                        `Area Code` = col_character())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "masterlist") |>
  select(list_source, Phone, `Area Code`, State)


printshop <- read_csv("data/Print-Shop.csv", 
                       col_types = cols(phone = col_character(), 
                                        `Area Code` = col_character(), date_updated = col_double())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "printshop") |>
  rename(State = `State Abbreviation`,
         State_name = State,
         Phone = phone) |> 
  select(list_source, Phone, `Area Code`, State)

wonders_lead_queue <- read_csv("data/wonders_lead_queue.csv", 
                               col_types = cols(BusinessPhone = col_character(), 
                                                Phone = col_character(),
                               `Lead Channel` = col_character(), 
                               `Zip/Postal Code` = col_character()))
wonders_lead_queue <- wonders_lead_queue|> 
  filter(Country == "United States") |> 
  mutate(list_source="wonders-lead-queue",
         `Area Code` = str_extract(Phone,"^\\d{3}")) |> 
  rename(State = `State/Province (text only)`) |> 
  drop_na(Phone) |> 
  left_join(us_area_code, by = c("Area Code"= "Area_Code")) |>
  mutate(State = case_when(is.na(State) ~ State_Area,
         TRUE ~ State)) |> 
  select(Phone, State, list_source,`Area Code`)  
  
sms_list <- rbind(brizo, inbound_call_sms_follow_up, masterlist, printshop, wonders_lead_queue)

sms_list_unique <- sms_list |> 
  distinct(Phone, .keep_all = TRUE)

# duplicate phone numbers 46420
# unique total = 88525


sms_list_unique <- sms_list_unique %>%
  mutate(Is10Digits = if_else(nchar(Phone) == 10, TRUE, FALSE)) |> 
  filter(Is10Digits== TRUE) |> 
  select(-Is10Digits)

## usable phone 85524

write_csv(sms_list_unique, "clean-data/sms-unique-list.csv")

marketing_nurture_queue <- read_csv("data/copy_of_marketing_nurture_queue.csv", 
                                            col_types = cols(MobilePrimary = col_character())) |> 
  rename(Phone=MobilePrimary)

marketing_nurture_queue <- marketing_nurture_queue  |> 
  drop_na(Phone) |> 
  mutate(list_source="wonders-lead-queue",
         `Area Code` = str_extract(Phone,"^\\d{3}"),
         Is10Digits = if_else(nchar(Phone) == 10, TRUE, FALSE)) |> 
  filter(Is10Digits==TRUE) |> 
  left_join(us_area_code, by =c("Area Code"="Area_Code"))

marketing_nurture_queue <- marketing_nurture_queue |> 
  mutate(State = case_when(is.na(`State/Province (text only)`) ~ State_Area,
                           TRUE ~ `State/Province (text only)`)) |> 
  filter(Country=="United States") |> 
  distinct(Phone, .keep_all = TRUE)


marketing_nurture_queue_sms_list <- marketing_nurture_queue |> 
  select(Phone, State, list_source,`Area Code`)  |> 
  distinct(Phone, .keep_all = TRUE)


  

write_csv(marketing_nurture_queue,"clean-data/marketing_nurture_queue.csv")
write_csv(marketing_nurture_queue_sms_list,"clean-data/marketing_nurture_queue_sms_list.csv")



