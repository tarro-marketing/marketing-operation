library(tidyverse)


masterlist <- read_csv("marketing-nurture-queue/data/masterlist.csv")
Brizo <- read_csv("marketing-nurture-queue/data/Brizo.csv")
Wonders_Lead_Queue_SMS_Net_New <- read_csv("marketing-nurture-queue/data/Wonders-Lead-Queue-SMS-Net-New.csv")
Inbound_Call_SMS_Follow_Up <- read_csv("marketing-nurture-queue/data/Inbound-Call-SMS-Follow-Up.csv")
Print_Shop <- read_csv("marketing-nurture-queue/data/Print-Shop.csv")


marketing_nurture_queue <- read_csv("marketing-nurture-queue/data/marketing-nurture-queue.csv")


Hiring_Post <- Hiring_Post %>%
  mutate(
    phone = gsub("[^\\d]", "", phone),
        phone = if_else(str_starts(phone, "1") & nchar(phone) == 11, str_sub(phone, 2), phone),
        phone = if_else(nchar(phone) == 10, phone, NA_character_),
        `Area Code` = if_else(!is.na(phone), str_extract(phone, "^\\d{3}"), NA_character_)
  ) %>%
  select(phone, `Area Code`, everything())