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
  clean_names() |>
  distinct(area_code, .keep_all = TRUE)

us_state_area_code <- read_sheet(
  ss = "1-8Y2nnNyc3qokKKcg_NXf6Kw3xKJM55aikCLiBnN-7U",
  sheet = "area_code_north_america",
  col_types = "cccc"
) |>
  clean_names() |>
  distinct(area_code, .keep_all = TRUE)

west_sms_leads <-
  all_time_leads |>
  filter((str_detect(lead_channel, "(?i)sms") | str_detect(latest_campaign, "(?i)sms")) &
           !str_detect(latest_campaign, "(?i)MktOutbound")) |>
  mutate(area_code = str_extract(mobile_primary, "\\d{3}")) |>
  distinct(lead_id, .keep_all = TRUE) |>
  rename(state_sfdc = state_province_text_only) |>
  inner_join(west_coast_state_area_code, by = "area_code") |>
  select(mel, mql, sql, onboarded, area_code, mobile_primary, state, state_sfdc, lead_channel, latest_campaign, everything())


sms_leads <-
  all_time_leads |>
  # filter((str_detect(lead_channel, "(?i)sms") | str_detect(latest_campaign, "(?i)sms")) &) |>
  filter(!str_detect(latest_campaign, "(?i)MktOutbound")) |>
  mutate(area_code = str_extract(mobile_primary, "\\d{3}")) |>
  distinct(lead_id, .keep_all = TRUE) |>
  rename(state_sfdc = state_province_text_only) |>
  inner_join(us_state_area_code, by = "area_code") |>
  filter(country == "United States") |>
  select(mel, mql, sql, onboarded, area_code, mobile_primary, state, state_sfdc, lead_channel, latest_campaign, everything())

state_aggregated_table_all <- sms_leads |>
  group_by(state_abbreviation) |>
  summarise(MEL = sum(mel == TRUE),
            MQL = sum(mql == TRUE),
            `MEL→MQL%` = if_else(MEL == 0, NA_real_, MQL / MEL),
            SQL = sum(sql == TRUE),
            `MQL→SQL%` = if_else(MQL == 0, NA_real_, SQL/MQL),
            Onboarded = sum(onboarded == TRUE),
            `SQL→Onboarded%` = if_else(SQL == 0, NA_real_, Onboarded/SQL),
            `MEL → SQL%` = if_else(MEL == 0, NA_real_, SQL/MEL),
            ) |>
  arrange(desc(MEL)) |>   # arrange by MEL in descending order
  mutate(`MEL Rank` = row_number())

write_sheet(state_aggregated_table_all,
            ss = "1oJ_-MHBsd84CncLG2esYwkAYJD6xNDo8Mz24yP9fvqk",
            sheet = "State Area Code (All Channel)")




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




