library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)

sms_sam_list_new <- read_sheet(ss = "1XSl6Izw6YRYlbXWmInQUmgu4vsMhaERZ_jKa09EUlPg",
           sheet = "SMS SAM List",
           range = "B:H") |>
  clean_names()
sms_sam_list_old<- read_sheet(ss = "1HDKnaLuPzP_-GWX0gGNsl72w5rd0H-YroC8g7RmVZtA",
           sheet = "SAM List",
           range = "A:G") |>
  clean_names()


sms_sam_list <- bind_rows(sms_sam_list_new, sms_sam_list_old) |>
  distinct()



leads <- read_sheet(ss = "1m8xy8hN_b-KedR168wLjBe6-jgXJF9tJH9Gm8WBMDVE",
                    sheet = "Hiring Post Source Check") |>
  clean_names()


sms_sam_list_hiring_post <- sms_sam_list |>
  mutate(phone = as.character(phone),
         phone = str_remove_all(phone, "[\\(\\)\\-\\s]"),  # remove (, ), -, and spaces
         phone = str_remove(phone, "^1"),
         area_code = str_extract(phone,"^\\d{3}")) |>
  select(phone, area_code, everything()) |>
  distinct(phone, .keep_all = TRUE)


leads__c <- leads |>
  mutate(mobile_primary = as.character(mobile_primary),
         mobile_primary = str_remove_all(mobile_primary, "[\\(\\)\\-\\s]"),  # remove (, ), -, and spaces
         mobile_primary = str_remove(mobile_primary, "^1"),  # remove leading 1 if present
         area_code = str_extract(mobile_primary, "^\\d{3}")) |>
  mutate(onboarded = case_when(stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
         sql = case_when(
           onboarded == TRUE ~ TRUE, opportunity_id != "" ~ TRUE,
           TRUE ~ FALSE
         ),
         mql = case_when(
           onboarded == TRUE ~ TRUE,
           sql == TRUE ~ TRUE,
           !(is.na(latest_mql_time_stamp))&
             !(unqualified_reason %in%
                 c(
                   "Current Client",
                   "Duplicate",
                   "Not a Restaurant"
                 )) ~ TRUE,
           TRUE ~ FALSE
         ),
         mel = case_when(
           onboarded == TRUE ~ TRUE,
           sql == TRUE ~TRUE,
           mql ==TRUE ~TRUE,
           !(is.na(first_mel_timestamp)) &
             !(unqualified_reason %in% c("Current Client", "Duplicate")) ~ TRUE,
           TRUE ~ FALSE
             )) |>
  mutate(match_sam_list = if_else(mobile_primary %in% sms_sam_list_hiring_post$phone, TRUE, FALSE)) |>  #if match phone # returns true
  left_join(sms_sam_list_hiring_post, by = c("mobile_primary"="phone")) |>
  select(match_sam_list, list, mobile_primary, mel, mql, sql, onboarded, everything())


write_sheet(leads__c, ss = "1HDKnaLuPzP_-GWX0gGNsl72w5rd0H-YroC8g7RmVZtA",
            sheet = "Improvement Check Data")


quick_summary <- leads__c |>
  group_by(from_hiring_post) |>
  summarise(counts = n())


