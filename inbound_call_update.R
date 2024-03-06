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


inbound_call <- "00OUo000001BvDhMAK"
inbound_call <- sf_run_report(inbound_call)

inbound_call <- inbound_call |> 
  mutate( `To Number` = str_replace_all(`To Number`, "[^\\d]",""),
          `From Number`=str_replace_all(`From Number`, "[^\\d]","")) 


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


inbound_call_tracker_data <- inbound_call |> 
  left_join(campaign_channel, by = c("To Number"="Extension Number")) |> 
  mutate(`Start Time` = as_date(`Start Time`),
         `Start Date`= as_date(`Start Date`),	
         `End Date` = as_date(`End Date`))



sheet_write(data = inbound_call_tracker_data, ss = "1ZSCgZYODPvh3Bw1hoS98PocYr6bLmuDx1wjNZzipb2E", sheet = "data")


