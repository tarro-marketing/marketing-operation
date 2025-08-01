library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)
library(here)

setwd(here("campaign-analysis"))
source("campaignmembers_opportunities.R")

################### loading data ###########################

# https://docs.google.com/spreadsheets/d/1IYMZ0a-yL7LQUgCsy6EZnSjHLBKXC48P9QRE9Kh8Jzo/edit?gid=1306183246#gid=1306183246
campaign_information <-
  read_sheet(
    ss = "1IYMZ0a-yL7LQUgCsy6EZnSjHLBKXC48P9QRE9Kh8Jzo",
    sheet = "DM",
    range = "A:E"
  )

################### data cleaning ###########################


campaign_member_data <- campaign_members_opportunities |>
  left_join(campaign_information, by = c("campaign_name" = "sub_campaign_tag")) |>
  filter(!is.na(campaign_keyword) & !str_detect(campaign_name, "Re-Engagement|Mooncake|Migration"))

campaign_member_data <- campaign_member_data |>
  mutate(across(where(is.list), ~ map_chr(., ~ ifelse(is.null(.x), NA, toString(.x)))))

campaign_member_data <- campaign_member_data |>
  filter(member_first_associated_date >= as.Date('2025-01-01')) |>
  mutate(month = tolower(month)) |>
  mutate(month_number = match(month, tolower(month.name))) |>
  mutate(campaign_end_date = ceiling_date(ymd(paste0("2025-", month_number, "-01")), "month") - 1)

# rm(campaign_information, campaign_members_opportunities)


campaign_member_data <- campaign_member_data |>
  mutate(
    member_first_associated_date = as.Date(member_first_associated_date, format = "%Y-%m-%d"),
    lead_first_mel_timestamp = as.Date(lead_first_mel_timestamp, format = "%Y-%m-%d"),
    lead_latest_mql_time_stamp = as.Date(lead_latest_mql_time_stamp, format = "%Y-%m-%d"),
    lead_converted_opportunity_created_date = as.Date(lead_converted_opportunity_created_date, format = "%Y-%m-%d"),
    date_time_closed_won = as.Date(date_time_closed_won, format = "%Y-%m-%d"),
    campaign_end_date = ceiling_date(ymd(paste0("2025-", month_number, "-01")), "month") - 1,
    lead_created_date = as.Date(lead_created_date, format = "%Y-%m-%d")
  )


campaign_member_data <- campaign_member_data |>
  mutate(
    mel_campaign_period = if_else(lead_first_mel_timestamp <= campaign_end_date, "30D", "60D"),
    mql_campaign_period = if_else(lead_latest_mql_time_stamp <= campaign_end_date, "30D", "60D"),
    sql_campaign_period = if_else(lead_converted_opportunity_created_date <= campaign_end_date, "30D", "60D"),
    cw_campaign_period = if_else(date_time_closed_won <= campaign_end_date, "30D", "60D")
  )



campaign_member_data2 <- campaign_member_data |>
  select(-campaign_end_date, -month_number) |>
  mutate(
    # create the 'mel' column, treating nas in unqualified_reason as valid
    mel = if_else(!is.na(lead_first_mel_timestamp) &
                    (is.na(lead_unqualified_reason) |
                       (tolower(lead_unqualified_reason) != "current client" &
                          tolower(lead_unqualified_reason) != "duplicate")), TRUE, FALSE),

    # create the 'mql' column, treating nas in unqualified_reason as valid
    mql = if_else(!is.na(lead_latest_mql_time_stamp) &
                    (is.na(lead_unqualified_reason) |
                       !tolower(lead_unqualified_reason) %in% c("current client", "duplicate", "not a restaurant", "incorrect phone number")), TRUE, FALSE),

    # create the 'sql' column, treating nas as false
    sql = !is.na(lead_converted_account_account_id),

    # create the 'cw' column, treating nas as false
    cw = if_else(!is.na(lead_converted_opportunity_stage) &
                          (lead_converted_opportunity_stage %in% c("Onboarded", "Closed Won") |
                             (lead_converted_opportunity_stage == "Closed Lost" & stage_at_closed_lost == "Closed Won")), TRUE, FALSE)
  ) |>
  mutate(
    # ensuring the logical flow for the funnel stages
    sql = if_else(cw, TRUE, sql),
    mql = if_else(sql, TRUE, mql),
    mel = if_else(mql, TRUE, mel)
  )

# rm(campaign_member_data)

write_sheet(campaign_member_data2, ss = "1hqz_0x0HtsB_bOOrydMbRH8SG4mkSRQ-Ebdz1lRW7uk", sheet = "campaign_member_data")


# rm(campaign_member_data2)




campaign_member_data_c <- campaign_member_data2 |>
  mutate(
    month = case_when(
      grepl("2025Jan", campaign_name) |  grepl("Jan25", campaign_name) ~ "January",
      grepl("2025Feb", campaign_name) |  grepl("Feb25", campaign_name) ~ "February",
      grepl("2025Mar", campaign_name) |  grepl("Mar25", campaign_name)  ~ "March",
      grepl("2025Apr", campaign_name) |  grepl("Apr25", campaign_name)  ~ "April",
      grepl("2025May", campaign_name) |  grepl("May25", campaign_name)  ~ "May",
      grepl("2025Jun", campaign_name) |  grepl("Jun25", campaign_name)  ~ "June",
      grepl("2025Jul", campaign_name) |  grepl("Jul25", campaign_name)  ~ "July",
      grepl("2025Aug", campaign_name) |  grepl("Aug25", campaign_name)  ~ "August",
      grepl("2025Sep", campaign_name) |  grepl("Sep25", campaign_name)  ~ "September",
      grepl("2025Oct", campaign_name) |  grepl("Oct25", campaign_name)  ~ "October",
      grepl("2025Nov", campaign_name) |  grepl("Nov25", campaign_name)  ~ "November",
      grepl("2025Dec", campaign_name) |  grepl("Dec25", campaign_name)  ~ "December",
      TRUE ~ NA_character_ # Handle cases where the campaign_name doesn't match the pattern
    ),
    month_number = case_when(
      grepl("Jan", campaign_name) ~ 1,
      grepl("Feb", campaign_name) ~ 2,
      grepl("Mar", campaign_name) ~ 3,
      grepl("Apr", campaign_name) ~ 4,
      grepl("May", campaign_name) ~ 5,
      grepl("Jun", campaign_name) ~ 6,
      grepl("Jul", campaign_name) ~ 7,
      grepl("Aug", campaign_name) ~ 8,
      grepl("Sep", campaign_name) ~ 9,
      grepl("Oct", campaign_name) ~ 10,
      grepl("Nov", campaign_name) ~ 11,
      grepl("Dec", campaign_name) ~ 12,
      TRUE ~ NA_real_
    ),
    subgroup = sub(".*((2025|25)\\w+.*)", "\\1", campaign_name)
  ) |>
  filter(!is.na(month))






summary_data <- campaign_member_data_c |>
  mutate(
    day = day(lead_created_date) # Extract the day of the month
  ) |>
  group_by(month, month_number, subgroup) |>
  summarize(
    mel_30d = sum(mel_campaign_period == "30D" & mel == TRUE, na.rm = TRUE),
    mql_30d = sum(mql_campaign_period == "30D" & mql == TRUE, na.rm = TRUE),
    sql_30d = sum(sql_campaign_period == "30D" & sql == TRUE, na.rm = TRUE),
    cw_30d = sum(cw_campaign_period == "30D" & cw == TRUE, na.rm = TRUE),
    mel_60d = sum(mel == TRUE, na.rm = TRUE),
    mql_60d = sum(mql == TRUE, na.rm = TRUE),
    sql_60d = sum(sql == TRUE, na.rm = TRUE),
    cw_60d = sum(cw == TRUE, na.rm = TRUE)) |>
  pivot_longer(
    cols = starts_with("mel_30d") | starts_with("mql_30d") | starts_with("sql_30d") | starts_with("cw_30d") |
      starts_with("mel_60d") | starts_with("mql_60d") | starts_with("sql_60d") | starts_with("cw_60d"),
    names_to = c(".value", "Period"),
    names_sep = "_"
  ) |>
  select(
    `Month Number` = month_number,
    `Campaign Month` = month,
    `Campaign Subgroup` = subgroup,
    `MEL` = mel,
    `MQL` = mql,
    `SQL` = sql,
    `CW` = cw,
    Period
  ) |>
  mutate(
    `Campaign Month` = paste0(`Campaign Month`, " (", toupper(Period), ")")
  ) |>
  select(-Period) |>
  arrange(`Month Number`, `Campaign Month`, `Campaign Subgroup`)

print(summary_data)

write_sheet(summary_data, ss = "1hqz_0x0HtsB_bOOrydMbRH8SG4mkSRQ-Ebdz1lRW7uk", sheet = "30D/60D Summary Campaign Details")


rid_data <- campaign_member_data_c |>
  select(year, month, campaign_keyword, campaign_name, cw, cw_campaign_period, lead_converted_opportunity_restaurant_id) |>
  filter(cw == TRUE)

write_sheet(rid_data, ss = "1hqz_0x0HtsB_bOOrydMbRH8SG4mkSRQ-Ebdz1lRW7uk", sheet = "RID")


write_sheet(campaign_member_data_c, ss = "1hqz_0x0HtsB_bOOrydMbRH8SG4mkSRQ-Ebdz1lRW7uk", sheet = "campaign_member_data_c")



test <- campaign_members_opportunities |>
  filter(str_detect(campaign_name,"(?i)mkt_dm_chn_mar|mkt_dm_eng_mar"))

test2 <- campaign_member_data|>
  filter(str_detect(campaign_name,"(?i)mkt_dm_chn_mar|mkt_dm_eng_mar"))
