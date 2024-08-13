library(tidyverse)
library(googlesheets4)
library(keyring)
library(janitor)
client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


all_leads <- read_sheet(ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
           sheet = "[all time] Marketing MEL/MQL Report",
           range = "A2:AH")

channel <- read_sheet(ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64", sheet = "Lead Channel Category") 


glimpse(all_leads)
glimpse(channel)
# clean column names
all_leads <- all_leads |>
  clean_names()
lead_channel_grouping <- channel |> 
  clean_names() |> 
  rename(lead_channel_groups = lead_channel,
         channel = lead_source)


# assign lead_value
all_leads <- all_leads |>
  mutate(lead_value = 1)

# split channel and calculate lead value
lead_values <- all_leads |>
  mutate(channel_list = str_split(lead_channel, ";\\s*")) |>
  unnest(channel_list) |>
  left_join(lead_channel_grouping, by = c("channel_list" = "channel")) |>
  group_by(lead_id, lead_channel) |>
  mutate(channel_count = n_distinct(channel_list)) |>
  ungroup() |>
  mutate(lead_value = lead_value / channel_count)



check <- lead_values |> 
  select(created_date, first_mel_timestamp, first_mql_time_stamp, 
         opportunity_created_date, placeholder_live_date,lead_channel_groups) |> 
  mutate(
    first_mel_timestamp = ymd_hms(first_mel_timestamp, quiet = TRUE),
    first_mql_time_stamp = ymd_hms(first_mql_time_stamp, quiet = TRUE),
    opportunity_created_date = ymd(opportunity_created_date, quiet = TRUE),
    placeholder_live_date = ymd(placeholder_live_date, quiet = TRUE),
    mel_to_mql_duration = round(difftime(first_mql_time_stamp, first_mel_timestamp, units = "days"), 1),
    mql_to_sql_duration = round(difftime(opportunity_created_date, first_mql_time_stamp, units = "days"), 1),
    sql_to_cw_duration = round(difftime(placeholder_live_date, opportunity_created_date, units = "days"), 1),
    Month = month(created_date)
  ) |> 
  group_by(lead_channel_groups) |> 
  summarise(
    avg_mel_to_mql_duration = round(mean(mel_to_mql_duration, na.rm = TRUE),1),
    avg_mql_to_sql_duration =  round(mean(mql_to_sql_duration, na.rm = TRUE),1),
    avg_sql_to_cw_duration =  round(mean(sql_to_cw_duration, na.rm = TRUE),1)
  )

check

