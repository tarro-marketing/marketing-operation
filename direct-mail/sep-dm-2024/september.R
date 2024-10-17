source("/Users/yukachen/marketing-operation/direct-mail/overall-data/marketing_dm_leads_from_sfdc.R")


sep_dm_leads <- dm_lead_campaign_c |>
  filter(str_detect(campaign_name,"(?i)2024sep"))

write_sheet( ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
             data = sep_dm_leads,
             sheet = "Sep")


sep_dm_campaign_summary <- sep_dm_leads |>
  filter(lead_category_sfdc!="Ountbound")
  group_by(campaign_name) |>
  summarise(
    MEL = sum(mel == TRUE, na.rm = TRUE),
    MQL = sum(mql == TRUE, na.rm = TRUE),
    SQL = sum(sql == TRUE, na.rm = TRUE),
    Onboarded = sum(onboarded == TRUE, na.rm = TRUE)
  )



range_write(
  ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
  data = sep_dm_campaign_summary,
  sheet = "Sep",
  range = "A2"
)



sep_dm_campaign_webflow_summary <- sep_dm_leads |>
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
  data = sep_dm_campaign_webflow_summary,
  sheet = "Sep",
  range = "A7"
)
