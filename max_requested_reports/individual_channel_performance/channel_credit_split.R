library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)


#source("~/marketing-operation/max_requested_reports/individual_channel_performance/all_time_leads.R")


lead_2025 <- all_lead__c |>
  filter(first_mel_year == 2025) |>
  select(-first_mel_year) |>
  select(first_mel_month, everything())

data_split <- lead_2025 |>
  mutate(lead_channel_split = str_split(lead_channel, ";\\s*"),
         total_leads = 1) |>
  unnest(lead_channel_split) |>
  group_by(first_mel_month, lead_channel, total_leads) |>
  mutate(channel_counts = n_distinct(lead_channel_split)) |>
  ungroup() |>
  mutate(funnel_credit = total_leads / channel_counts)



write_csv(data_split, "~/marketing-operation/max_requested_reports/individual_channel_performance/2025_lead_c.csv")

rm(list = setdiff(ls(), "data_split","all_lead__c"))



