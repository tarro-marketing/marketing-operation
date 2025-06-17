library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)


glimpse(data_split)


aggregated_table <- data_split %>%
  group_by(lead_channel_split, first_mel_month) %>%
  summarise(
    total_mel = sum(funnel_credit * is_mel, na.rm = TRUE),
    total_mql = sum(funnel_credit * is_mql, na.rm = TRUE),
    total_sql = sum(funnel_credit * is_sql, na.rm = TRUE),
    total_cw  = sum(funnel_credit * is_cw,  na.rm = TRUE),
    .groups    = "drop"
  )


write_csv(aggregated_table, "/Users/yukachen/marketing-operation/max_requested_reports/individual_channel_performance/aggregated_table_2025.csv", na = "")
