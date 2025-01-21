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
  mutate(
    mobile_primary = str_replace_all(mobile_primary, "[^\\d]", ""),
    business_phone_line_1 = str_replace_all(
      str_replace_all(business_phone_line_1, "-", ""),
      "[^0-9]", ""
    ),
    flow_type = case_when(
      company_account %in% "WeCom Integration" ~ "Wecom",
      created_by == "JotForm Integration User" ~ "JotForm",
      created_by == "Carlito Academia" ~ "QR Scan",
      created_by %in% "(?i)ClickSend" ~ "SMS Replies",
      created_by == "Public Site Guest User" ~ "Email",
      TRUE ~ "inbound call"
    ),
    onboarded = case_when(stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
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
    )
  ) |>
  rename(business_phone = business_phone_line_1) |>
  select(mel, mql, sql, onboarded, everything())

sheet_write(all_lead__c,
            ss="1KRaC1IeLlMmufiiOuPnicMJr132LP9MYl4eRDVKXS3k",
            sheet="2024 Raw Data")

write_csv(all_lead__c, "/Users/yukachen/marketing-operation/all-time-funnel-data/all_time_leads.csv")


write_csv(all_lead__c, "/Users/yukachen/marketing-operation/all-time-funnel-data/all_time_leads_1_14_2025.csv")

