library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)

source("/Users/yukachen/marketing-operation/data/lead_campaign.R")

dm_lead_campaign_c <- final_campaign_sfdc_lead |>
  filter(
    str_detect(campaign_name, "(?i)Mkt_DM_Snowflake_2024")
  )

write_sheet(dm_lead_campaign_c,
  ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
  sheet = "Lead Data (+Campaign)"
)

rm(list = setdiff(ls(), c("dm_lead_campaign_c")))

co
