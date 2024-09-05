library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)
library(here)

setwd(here("direct-mail", "campaign-member"))

client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path"
)
email <- keyring::key_get(service = "googlesheets4", username = "email")

# configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

################### loading data ###########################

campaign_members <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] mkt campaign report",
    range = "A1:AL"
  ) |>
  clean_names() |>
  filter(str_detect(lead_channel, "DM"))


campaign_information <-
  read_sheet(
    ss = "1IYMZ0a-yL7LQUgCsy6EZnSjHLBKXC48P9QRE9Kh8Jzo",
    sheet = "campaign_information",
    range = "A:D"
  )




campaign_member_data <- campaign_members %>%
  left_join(campaign_information, by = c("campaign_name" = "sub_campaign_tag")) |>
  filter(!is.na(campaign_keyword) & !str_detect(campaign_name, "Re-Engagement|Mooncake|Migration"))

campaign_member_data <- campaign_member_data %>%
  mutate(across(where(is.list), ~ map_chr(., ~ ifelse(is.null(.x), NA, toString(.x)))))

campaign_member_data <- campaign_member_data %>%
  mutate(month = tolower(month)) %>%
  mutate(month_number = match(month, tolower(month.name))) %>% # Convert month name to number
  mutate(campaign_end_date = ceiling_date(ymd(paste0("2024-", month_number, "-01")), "month") - 1) # Assume year 2024

campaign_member_data <- campaign_member_data %>%
  mutate(
    member_first_associated_date = as.Date(member_first_associated_date, format = "%Y-%m-%d"),
    lead_first_mel_timestamp = as.Date(lead_first_mel_timestamp, format = "%Y-%m-%d"),
    lead_latest_mql_time_stamp = as.Date(lead_latest_mql_time_stamp, format = "%Y-%m-%d"),
    lead_converted_opportunity_created_date = as.Date(lead_converted_opportunity_created_date, format = "%Y-%m-%d"),
    lead_converted_account_pos_first_order_date = as.Date(lead_converted_account_pos_first_order_date, format = "%Y-%m-%d"),
    campaign_end_date = ceiling_date(ymd(paste0("2024-", month_number, "-01")), "month") - 1
  )

campaign_member_data <- campaign_member_data %>%
  mutate(
    mel_campaign_period = if_else(lead_first_mel_timestamp <= campaign_end_date, "30D", "60D"),
    mql_campaign_period = if_else(lead_latest_mql_time_stamp <= campaign_end_date, "30D", "60D"),
    sql_campaign_period = if_else(lead_converted_opportunity_created_date <= campaign_end_date, "30D", "60D"),
    cw_onboarded_campaign_period = if_else(lead_converted_account_pos_first_order_date <= campaign_end_date, "30D", "60D")
  )

campaign_member_data <- campaign_member_data %>%
  select(-campaign_end_date, -month_number)


campaign_member_data_c <- campaign_member_data %>%
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
    sql = if_else(!is.na(lead_converted_account_account_id), TRUE, FALSE),

    # create the 'onboarded' column, treating nas as false
    onboarded = if_else(!is.na(lead_converted_opportunity_stage) & lead_converted_opportunity_stage == "Onboarded", TRUE, FALSE)
  ) %>%
  mutate(
    # ensuring the logical flow for the funnel stages
    sql = if_else(onboarded, TRUE, sql), # if onboarded is true, sql should be true
    mql = if_else(sql, TRUE, mql), # if sql is true, mql should be true
    mel = if_else(mql, TRUE, mel) # if mql is true, mel should be true
  )


write_sheet(campaign_member_data_c, ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU", sheet = "30D/60D Data")


# campaign_member_data_c %>%
#   group_by(month) %>%
#   group_walk(~ write_sheet(.x,
#     ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
#     sheet = paste0("campaign_members_", .y$month)
#   ))


# campaign_member_data_c %>%
#   group_by(month) %>%
#   group_walk(~ write.csv(.x, paste0("campaign_members_", .y$month, ".csv"), row.names = FALSE))





campaign_member_data_c <- campaign_member_data_c %>%
  mutate(lead_created_date = as.Date(lead_created_date, format = "%Y-%m-%d")) # Adjust format if necessary

campaign_member_data_c <- campaign_member_data_c %>%
  mutate(
    month = case_when(
      grepl("2024Jan", campaign_name) ~ "January",
      grepl("2024Feb", campaign_name) ~ "February",
      grepl("2024Mar", campaign_name) ~ "March",
      grepl("2024Apr", campaign_name) ~ "April",
      grepl("2024May", campaign_name) ~ "May",
      grepl("2024Jun", campaign_name) ~ "June",
      grepl("2024Jul", campaign_name) ~ "July",
      grepl("2024Aug", campaign_name) ~ "August",
      grepl("2024Sep", campaign_name) ~ "September",
      grepl("2024Oct", campaign_name) ~ "October",
      grepl("2024Nov", campaign_name) ~ "November",
      grepl("2024Dec", campaign_name) ~ "December",
      TRUE ~ NA_character_ # Handle cases where the campaign_name doesn't match the pattern
    ),
    month_number = case_when(
      grepl("2024Jan", campaign_name) ~ 1,
      grepl("2024Feb", campaign_name) ~ 2,
      grepl("2024Mar", campaign_name) ~ 3,
      grepl("2024Apr", campaign_name) ~ 4,
      grepl("2024May", campaign_name) ~ 5,
      grepl("2024Jun", campaign_name) ~ 6,
      grepl("2024Jul", campaign_name) ~ 7,
      grepl("2024Aug", campaign_name) ~ 8,
      grepl("2024Sep", campaign_name) ~ 9,
      grepl("2024Oct", campaign_name) ~ 10,
      grepl("2024Nov", campaign_name) ~ 11,
      grepl("2024Dec", campaign_name) ~ 12,
      TRUE ~ NA_real_
    ),
    subgroup = sub(".*(2024\\w+.*)", "\\1", campaign_name)
  ) |>
  filter(!is.na(month))

summary_data <- campaign_member_data_c %>%
  mutate(
    day = day(lead_created_date) # Extract the day of the month
  ) %>%
  group_by(month, month_number, subgroup) %>%
  summarize(
    mel_30d = sum(mel[day <= 30], na.rm = TRUE),
    mql_30d = sum(mql[day <= 30], na.rm = TRUE),
    sql_30d = sum(sql[day <= 30], na.rm = TRUE),
    onboarded_30d = sum(onboarded[day <= 30], na.rm = TRUE),
    mel_60d = sum(mel[day <= 60], na.rm = TRUE),
    mql_60d = sum(mql[day <= 60], na.rm = TRUE),
    sql_60d = sum(sql[day <= 60], na.rm = TRUE),
    onboarded_60d = sum(onboarded[day <= 60], na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("mel_30d") | starts_with("mql_30d") | starts_with("sql_30d") | starts_with("onboarded_30d") |
      starts_with("mel_60d") | starts_with("mql_60d") | starts_with("sql_60d") | starts_with("onboarded_60d"),
    names_to = c(".value", "Period"),
    names_sep = "_"
  ) %>%
  select(
    `Month Number` = month_number,
    `Campaign Month` = month,
    `Campaign Subgroup` = subgroup,
    `MEL` = mel,
    `MQL` = mql,
    `SQL` = sql,
    `CW - Onboarded` = onboarded,
    Period
  ) %>%
  mutate(
    `Campaign Month` = paste0(`Campaign Month`, " (", toupper(Period), ")")
  ) %>%
  select(-Period) %>%
  arrange(`Month Number`, `Campaign Month`, `Campaign Subgroup`)

print(summary_data)

write_sheet(summary_data, ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU", sheet = "30D/60D Summary Campaign Details")
