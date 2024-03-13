##################### Loadning Library ########################
library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(here)

#################### Inbound Calls ##########################
config <- yaml::read_yaml("C:/Users/skt/Documents/API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


inbound_call <- "00OUo000001OMZtMAO"
inbound_call <- sf_run_report(inbound_call)

inbound_call <- inbound_call |>
  mutate(
    `To Number` = str_replace_all(`To Number`, "[^\\d]", ""),
    `From Number` = str_replace_all(`From Number`, "[^\\d]", "")
  )


################## Channel Mapping ##########################

gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


campaign_channel_gsheet <- read_sheet("1y-5z_omCFu9lTkCasqopOE5B8icQzqmsKijnZQsqlZc",
  sheet = "Campaigns List",
  range = "A2:I"
)

campaign_channel <- campaign_channel_gsheet |>
  filter(Channel != "N/A") |>
  drop_na(`Extension Number`) |>
  mutate(`Extension Number` = str_split(`Extension Number`, "\\s+")) |>
  unnest(`Extension Number`)

campaign_channel <- campaign_channel |>
  mutate(
    Start_Date = as.Date(`Start Date`),
    End_Date = as.Date(`End Date`)
  )

ended_campaign_gsheet <- read_sheet("1y-5z_omCFu9lTkCasqopOE5B8icQzqmsKijnZQsqlZc",
                                      sheet = "Ended Campaigns",
                                      range = "A:I"
)

ended_campaign <- ended_campaign_gsheet |>
  filter(Channel != "N/A") |>
  drop_na(`Extension Number`) |>
  mutate(`Extension Number` = str_split(`Extension Number`, "\\s+")) |>
  unnest(`Extension Number`)|>
  mutate(
    Start_Date = as.Date(`Start Date`),
    End_Date = as.Date(`End Date`)
  )


phone_mapping <- bind_rows(campaign_channel, ended_campaign)



phone_mapping <- phone_mapping |>
  mutate(
    Month = format(as.Date(`Start Date`), "%B"),
    UniqueID = paste(`Extension Number`, Month, sep = "_")
  ) |>
  select(-Month) |>
  select(UniqueID, everything()) |>
  arrange(`Start Date`) |> # Or your chosen priority field
  distinct(UniqueID, .keep_all = TRUE) |>
  select(-Start_Date, -End_Date)

channel_gsheet <- read_sheet("1y-5z_omCFu9lTkCasqopOE5B8icQzqmsKijnZQsqlZc", sheet = "Channel(For SDR)")
channel <- channel_gsheet |>
  mutate(`Extension Number` = str_split(`Extension Number`, "\\s+")) |>
  unnest(`Extension Number`)

rm(channel_gsheet, config, campaign_channel_gsheet,ended_campaign_gsheet)


channel <- channel |>
  mutate(Channel = case_when(
    is.na(Channel) & str_detect(Campaign, "(?i)DM") ~ "DM",
    is.na(Channel) & str_detect(Campaign, "(?i)SMS") ~ "SMS",
    is.na(Channel) & str_detect(Campaign, "(?i)SEO") ~ "SEO",
    is.na(Channel) & str_detect(Campaign, "(?i)Google") ~ "Google",
    TRUE ~ Channel
  )) |>
  select(`Extension Number`, Channel, Campaign)




inbound_call_tracker_data <- inbound_call |>
  mutate(UniqueID = paste(`To Number`, format(as.Date(`Start Time`), "%B"), sep = "_")) |>
  left_join(phone_mapping, by = "UniqueID") |>
  mutate(
    `Start Time` = as_date(`Start Time`),
    `Start Date` = as_date(`Start Date`),
    `End Date` = as_date(`End Date`)
  )

rm(inbound_call, campaign_channel, ended_campaign)

not_na <- inbound_call_tracker_data |>
  filter(!is.na(Channel))

is_na <- inbound_call_tracker_data |>
  filter(is.na(Channel)) |>
  left_join(channel, by = c("To Number" = "Extension Number")) |>
  mutate(
    Channel = coalesce(Channel.x, Channel.y),
    `Campaign Name` = coalesce(`Campaign Name`, Campaign)
  ) |>
  select(-c(Channel.x, Channel.y, Campaign)) |>
  mutate(
    Status = case_when(
      !is.na(Channel) ~ "In Progress",
      TRUE ~ NA_character_
    ),
    Active = case_when(
      !is.na(Channel) ~ TRUE,
      TRUE ~ FALSE
    )
  )


phone_mapping_sorted <- phone_mapping %>%
  arrange(`Extension Number`, `Start Date`) # Replace 'Date' with your actual date/time column name

# Now filter to keep only the first (earliest) record for each Extension Number
phone_mapping_filtered <- phone_mapping_sorted %>%
  group_by(`Extension Number`) %>%
  slice_min(order_by = `Start Date`)
rm(phone_mapping, channel)

is.na2 <- is_na |>
  filter(is.na(Channel)) |>
  left_join(phone_mapping_filtered, by = c("To Number" = "Extension Number")) |>
  mutate(
    UniqueID = coalesce(`UniqueID.x`, `UniqueID.y`),
    `Campaign Name` = coalesce(`Campaign Name.x`, `Campaign Name.y`),
    `Parent Campaign` = coalesce(`Parent Campaign.x`, `Parent Campaign.y`),
    `Start Date` = coalesce(`Start Date.x`, `Start Date.y`),
    `End Date` = coalesce(`End Date.x`, `End Date.y`),
    Status = coalesce(`Status.x`, `Status.y`),
    Active = Active.y,
    Description = coalesce(`Description.x`, `Description.y`),
    Channel = coalesce(`Channel.x`, `Channel.y`)
  ) |>
  select(-`UniqueID.x`, -`UniqueID.y`, -`Campaign Name.x`, -`Parent Campaign.x`, -`Start Date.x`, -`End Date.x`, -`Status.x`, -`Active.x`, -`Description.x`, -`Channel.x`, -`Channel.y`, -`Campaign Name.y`, -`Parent Campaign.y`, -`Start Date.y`, -`End Date.y`, -`Status.y`, -`Active.y`, -`Description.y`)


is_na_no_na <- is_na |>
  drop_na(Channel)
ring_dna_tracker <- bind_rows(not_na, is_na_no_na, is.na2)


rm(phone_mapping_filtered, phone_mapping_sorted,not_na, is_na_no_na, is.na2, is_na, inbound_call_tracker_data)

sheet_write(data = ring_dna_tracker, ss = "1ZSCgZYODPvh3Bw1hoS98PocYr6bLmuDx1wjNZzipb2E", sheet = "data")
