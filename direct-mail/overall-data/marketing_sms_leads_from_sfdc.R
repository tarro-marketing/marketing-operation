library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)

source("/Users/yukachen/marketing-operation/data/lead_campaign.R")

sms_lead_campaign_c <- final_campaign_sfdc_lead |>
  filter(
    str_detect(campaign_name, "(?i)Mkt_sms") &
    !str_detect(campaign_name, "(?i)outbound")

  )

write_sheet(sms_lead_campaign_c,
            ss = "15bKc-5i_FiSgwncF1-0M50cZSrpttoSstlx2UhQJLFU",
            sheet = "SMS Lead Data (+Campaign)"
)

# rm(list = setdiff(ls(), c("sms_lead_campaign_c")))
