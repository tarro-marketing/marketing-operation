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
    range = "A2:AK"
  )

campaign_members <- campaign_members |> clean_names()

campaign_members |>
  filter(str_detect(lead_channel,"DM"))


campaign_information <-
  read_sheet(
    ss = "1IYMZ0a-yL7LQUgCsy6EZnSjHLBKXC48P9QRE9Kh8Jzo",
    sheet = "campaign_information",
    range = "A:D"
  )



campaign_member_data <- campaign_members %>%
  rowwise() %>%
  mutate(
    matching_campaign = list(
      campaign_information %>%
        filter(str_detect(sub_campaign_tag, fixed(campaign_keyword)))
    )
  ) %>%
  unnest(cols = matching_campaign) %>%
  ungroup()

# If there are list columns, unnest or flatten them
campaign_member_data <- campaign_member_data %>%
  mutate(across(where(is.list), ~ map_chr(.x, toString)))


campaign_member_data <- campaign_member_data %>%
  mutate(month = tolower(month)) %>%
  mutate(month_number = match(month, tolower(month.name))) %>%  # Convert month name to number
  mutate(campaign_end_date = ceiling_date(ymd(paste0("2024-", month_number, "-01")), "month") - 1)  # Assume year 2024

# Step 4: Create the 'campaign_period' column
campaign_member_data <- campaign_member_data %>%
  mutate(
    member_first_associated_date = as.Date(member_first_associated_date, format = "%Y-%m-%d"),
    campaign_period = if_else(member_first_associated_date <= campaign_end_date, "30D", "60D")
  )

# Optionally, drop the temporary 'campaign_end_date' and 'month_number' columns if not needed
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
    sql = if_else(onboarded, TRUE, sql),       # if onboarded is true, sql should be true
    mql = if_else(sql, TRUE, mql),             # if sql is true, mql should be true
    mel = if_else(mql, TRUE, mel)              # if mql is true, mel should be true
  )


summary_data <- campaign_member_data_c %>%
  group_by(month, campaign_period) %>%
  summarize(
    mel_count = sum(mel, na.rm = TRUE),
    mql_count = sum(mql, na.rm = TRUE),
    sql_count = sum(sql, na.rm = TRUE),
    onboarded_count = sum(onboarded, na.rm = TRUE)
  ) %>%
  mutate(
    numeric_month = match(tolower(month), tolower(month.name)),
    year = if_else(numeric_month >= 10, 2023, 2024)
  ) %>%
  ungroup() |>
  select(numeric_month, year, everything())



write_sheet(summary_data,ss="18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",sheet="SFDC Leads + Campaigns")


campaign_member_data_c %>%
  group_by(month) %>%
  group_walk(~ write_sheet(.x,
                           ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
                           sheet = paste0("campaign_members_", .y$month)))


campaign_member_data_c %>%
  group_by(month) %>%
  group_walk(~ write.csv(.x, paste0("campaign_members_", .y$month, ".csv"), row.names = FALSE))





