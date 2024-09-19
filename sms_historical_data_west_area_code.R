library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)

source("/Users/yukachen/marketing-operation/all-time-funnel-data/all_time_leads.R")

all_time_leads <- read_csv("/Users/yukachen/marketing-operation/all-time-funnel-data/all_time_leads.csv")

west_coast_state_area_code <- read_sheet(
  ss = "1-8Y2nnNyc3qokKKcg_NXf6Kw3xKJM55aikCLiBnN-7U",
  sheet = "west_coast_state_area_code",
  col_types = "cccc"
) |>
  clean_names()

west_sms_leads <-
  all_time_leads |>
  filter(str_detect(lead_channel, ("(?i)sms")) | str_detect(latest_campaign, "(?i)sms")) |>
  mutate(area_code = str_extract(mobile_primary, "\\d{3}")
         # mel = ifelse(mel, "\u2713", "\u2717"),
         # mql = ifelse(mql, "\u2713", "\u2717"),
         # sql = ifelse(sql, "\u2713", "\u2717"),
         # onboarded = ifelse(onboarded, "\u2713", "\u2717")
         # # ✓ is \u2713, ✗ is \u2717
  ) |>
  rename(state_sfdc = state_province_text_only) |>
  select(mel, mql, sql, onboarded, area_code, mobile_primary, business_phone, everything()) |>
  inner_join(west_coast_state_area_code, by = "area_code", relationship = "many-to-many") |>
  select(mel, mql, sql, onboarded, area_code, mobile_primary, state, state_sfdc, lead_channel, latest_campaign, everything())



campaign_aggregated_table <- west_sms_leads |>
  group_by(latest_campaign, state) |>
  summarise(MEL = sum(mel == TRUE),
            MQL = sum(mql == TRUE),
            SQL = sum(sql == TRUE),
            Onboarded = sum(onboarded == TRUE))


state_aggregated_table <- west_sms_leads |>
  group_by(state) |>
  summarise(MEL = sum(mel == TRUE),
            MQL = sum(mql == TRUE),
            SQL = sum(sql == TRUE),
            Onboarded = sum(onboarded == TRUE))


monthly_state_aggregated_table <- west_sms_leads |>
  mutate(month = month(first_mel_timestamp)) |>
  group_by(month, state) |>
  summarise(MEL = sum(mel == TRUE),
            MQL = sum(mql == TRUE),
            SQL = sum(sql == TRUE),
            Onboarded = sum(onboarded == TRUE))



write_sheet(west_sms_leads,
            ss = "1oJ_-MHBsd84CncLG2esYwkAYJD6xNDo8Mz24yP9fvqk",
            sheet = "Cleaned SFDC Leads Data")

write_sheet(campaign_aggregated_table,
            ss = "1oJ_-MHBsd84CncLG2esYwkAYJD6xNDo8Mz24yP9fvqk",
            sheet = "Aggregated Table")


write_sheet(state_aggregated_table,
            ss = "1oJ_-MHBsd84CncLG2esYwkAYJD6xNDo8Mz24yP9fvqk",
            sheet = "State Aggregated Table")

write_sheet(monthly_state_aggregated_table,
            ss = "1oJ_-MHBsd84CncLG2esYwkAYJD6xNDo8Mz24yP9fvqk",
            sheet = "Monthly State Aggregated Table")




