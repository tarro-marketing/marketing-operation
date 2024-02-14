library(tidyverse)


Brizo <- read_csv("marketing-nurture-queue/data/Brizo.csv", 
                  col_types = cols(Phone = col_character(), 
                                   PhoneBrizoDM = col_character(),
                                   `Area Code` = col_character())) |> 
  filter(Country=="United States") |> 
  mutate(list_source = "brizo") |> 
  rename(State=state_group) |> 
  select(list_source, Phone, `Area Code`, State) 

inbound_call_sms_follow_up<- read_csv("marketing-nurture-queue/data/Inbound-Call-SMS-Follow-Up.csv", 
                                       col_types = cols(phone = col_character(), 
                                                        `Area Code` = col_character())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "inbound_call_sms_follow_up") |>
  rename(State = `State Abbreviation`,
         State_name = State,
         Phone = phone) |> 
  select(list_source, Phone, `Area Code`, State)
  
masterlist <- read_csv("marketing-nurture-queue/data/masterlist.csv", 
                       col_types = cols(Phone = col_character(), 
                                        `Area Code` = col_character())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "masterlist") |>
  select(list_source, Phone, `Area Code`, State)


Print_Shop <- read_csv("marketing-nurture-queue/data/Print-Shop.csv", 
                       col_types = cols(phone = col_character(), 
                                        `Area Code` = col_character(), date_updated = col_double())) |> 
  filter(Country == "United States") |> 
  mutate(list_source = "printshop") |>
  rename(State = `State Abbreviation`,
         State_name = State,
         Phone = phone) |> 
  select(list_source, Phone, `Area Code`, State)

Wonders_Lead_Queue_SMS_Net_New <- read_csv("marketing-nurture-queue/data/Wonders-Lead-Queue-SMS-Net-New.csv", 
                                           col_types = cols(Phone = col_character(), 
                                                            `Area Code` = col_character())) |> 
  
  

marketing_nurture_queue <- read_csv("data/marketing-nurture-queue.csv")

us_area_code_cities <- read_csv("data/us_area_code_cities.csv")


Hiring_Post <- Hiring_Post %>%
  mutate(
    phone = gsub("[^\\d]", "", phone),
        phone = if_else(str_starts(phone, "1") & nchar(phone) == 11, str_sub(phone, 2), phone),
        phone = if_else(nchar(phone) == 10, phone, NA_character_),
        `Area Code` = if_else(!is.na(phone), str_extract(phone, "^\\d{3}"), NA_character_)
  ) %>%
  select(phone, `Area Code`, everything())