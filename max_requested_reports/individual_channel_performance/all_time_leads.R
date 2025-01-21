library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)


client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path"
)
email <- keyring::key_get(service = "googlesheets4", username = "email")

# configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

rm(client_secret_path, email)
################### loading data ###########################

all_lead__r <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Marketing MEL/MQL Report",
    range = "A:AM"
  ) |>
  clean_names()

#################### Cleaning Data ##########################


all_lead__c <- all_lead__r |>
  filter( !is.na(latest_campaign),
          !str_detect(latest_campaign, "outbound")) |>
  mutate(
    first_mel_timestamp = parse_date_time(first_mel_timestamp, orders = c("ymd HMS", "ymd")),
    first_mel_month = format(first_mel_timestamp, "%Y-%m"),
    first_mel_year = year(first_mel_timestamp)
  ) |>
  mutate(
    mobile_primary = str_replace_all(mobile_primary, "[^\\d]", ""),
    business_phone_line_1 = str_replace_all(
      str_replace_all(business_phone_line_1, "-", ""),
      "[^0-9]", ""
    ),
    is_onboarded = case_when(stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    is_sql = case_when(
      opportunity_id != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    is_mql = case_when(
      !(is.na(latest_mql_time_stamp))&
        !(unqualified_reason %in%
          c(
            "Current Client",
            "Duplicate",
            "Not a Restaurant"
          )) ~ TRUE,
      TRUE ~ FALSE
    ),
    is_mel = case_when(
      !(is.na(first_mel_timestamp)) &
      !(unqualified_reason %in% c("Current Client", "Duplicate")) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  rename(business_phone = business_phone_line_1) |>
  select(is_mel, is_mql, is_sql, is_onboarded, everything())

write_csv(all_lead__c, "~/marketing-operation/max_requested_reports/individual_channel_performance/all_time_leads_1_14_2025.csv", na = "")

rm(list = setdiff(ls(), "all_lead__c"))
