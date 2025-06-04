library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)


glimpse(data_split)


aggregated_table <- data_split |>
  group_by(lead_channel_split, first_mel_month) |>
  summarise(total_mel = sum(funnel_credit * is_mel),
            total_mql = sum(funnel_credit * is_mql),
            total_sql = sum(funnel_credit * is_sql),
            total_onboarded = sum(funnel_credit * is_onboarded),
            .groups = "drop")


write_csv(aggregated_table, "/Users/yukachen/marketing-operation/max_requested_reports/individual_channel_performance/aggregated_table_2023.csv", na = "")
