source("/Users/yukachen/marketing-operation/direct-mail/overall-data/marketing_sms_leads_from_sfdc.R")


sep_sms_leads <- sms_lead_campaign_c |>
  filter(str_detect(campaign_name,"(?i)2024sep"))

write_sheet( ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
             data = sep_sms_leads,
             sheet = "Sep")


sep_sms_campaign_summary <- sep_sms_leads |>
  group_by(campaign_name) |>
  summarise(
    MEL = sum(mel == TRUE, na.rm = TRUE),
    MQL = sum(mql == TRUE, na.rm = TRUE),
    SQL = sum(sql == TRUE, na.rm = TRUE),
    Onboarded = sum(onboarded == TRUE, na.rm = TRUE)
  )



range_write(
  ss = "15bKc-5i_FiSgwncF1-0M50cZSrpttoSstlx2UhQJLFU",
  data = sep_sms_campaign_summary,
  sheet = "Sep",
  range = "A2"
)



sep_sms_campaign_webflow_summary <- sep_sms_leads |>
  filter(flow == "webflow") |>
  group_by(campaign_name) |>
  summarise(
    MEL = sum(mel == TRUE, na.rm = TRUE),
    MQL = sum(mql == TRUE, na.rm = TRUE),
    SQL = sum(sql == TRUE, na.rm = TRUE),
    Onboarded = sum(onboarded == TRUE, na.rm = TRUE)
  )

range_write(
  ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
  data = sep_sms_campaign_webflow_summary,
  sheet = "Sep",
  range = "A7"
)
