##################### Loadning Library ########################
library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(here)

#################### Inbound Calls ##########################
config <- yaml::read_yaml("../../API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


inbound_call <- "00OUo000001BvDhMAK"
inbound_call <- sf_run_report(inbound_call)

inbound_call <- inbound_call |> 
  mutate( `To Number` = str_replace_all(`To Number`, "[^\\d]",""),
          `From Number`=str_replace_all(`From Number`, "[^\\d]","")) |> 
  select(`Start Time`, `From Number`, `To Number`, `Conversation Name`)


################## Channel Mapping ##########################

gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


campaign_channel_gsheet <- read_sheet("1y-5z_omCFu9lTkCasqopOE5B8icQzqmsKijnZQsqlZc", 
                                       sheet = "Campaigns List",
                                       range = "A2:I" )

campaign_channel <- campaign_channel_gsheet |> 
  filter(Channel != "N/A") |> 
  drop_na(`Extension Number`) |> 
  mutate(`Extension Number` = str_split(`Extension Number`, "\\s+"))  |> 
  unnest(`Extension Number`)

channel_gsheet <- read_sheet("1y-5z_omCFu9lTkCasqopOE5B8icQzqmsKijnZQsqlZc", sheet = "Channel(For SDR)")
channel <- channel_gsheet |> 
  mutate(`Extension Number` = str_split(`Extension Number`, "\\s+"))  |> 
  unnest(`Extension Number`)

rm(channel_gsheet, config, campaign_channel_gsheet)


channel <- channel |> 
  mutate(Channel = case_when(is.na(Channel)& str_detect(Campaign, "(?i)DM") ~ "DM",
                             is.na(Channel)& str_detect(Campaign, "(?i)SMS") ~ "SMS",
                             is.na(Channel)& str_detect(Campaign, "(?i)SEO") ~ "SEO",
                             is.na(Channel)& str_detect(Campaign, "(?i)Google") ~ "Google",
                             TRUE ~ Channel
                             )) |> 
  select(`Extension Number`, Channel, Campaign)






