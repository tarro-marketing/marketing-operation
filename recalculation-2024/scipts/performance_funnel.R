library(tidyverse)
setwd("~/PerformanceMarketing/direct-mail-2023/recalculation-2024")
final_campaign_sfdc_lead <- read_csv("clean_data/final_campaign_sfdc_lead.csv")



sfdc_lead <- final_campaign_sfdc_lead |>
  mutate(
    Mobile__Primary_SFDC = str_replace_all(str_replace_all(Mobile__Primary_SFDC, "-", ""), "[^0-9]", ""),
    Business_Phone_SFDC = str_replace_all(str_replace_all(Business_Phone_SFDC, "-", ""), "[^0-9]", "")
  ) |>
  mutate(flow = case_when(
    Created_By_SFDC %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
    TRUE ~ "inbound call"
  )) |>
  mutate(types = case_when(
    Created_By_SFDC == "JotForm Integration User" ~ "jotform",
    Created_By_SFDC == "Carlito Academia" ~ "qr scan",
    TRUE ~ "inbound call"
  )) ### create mql, sql, cw label

leads <- sfdc_lead |>
  mutate(
    CW = case_when(
      Stage_SFDC == "Closed Won" | Stage_SFDC == "Onboarded" ~ "TRUE",
      TRUE ~ "FALSE"),
    SQL = case_when(
      CW == TRUE ~ TRUE,
      Opportunity_ID_SFDC != "" ~ "TRUE",
      TRUE ~ "FALSE"
    ),
    MQL = case_when(
      SQL == TRUE ~ TRUE,
      CW == TRUE ~ TRUE,
      Menu_Type_SFDC != "" &
        StateProvince_text_only_SFDC != "" &
        (
          Created_By_SFDC == "JotForm Integration User" |
            Lead_Status_sfdc %in% c("Converted", "AE Assigned")
        ) &
        !(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate", "Not a Restaurant")
        ) ~ "TRUE",
      TRUE ~ "FALSE"
  ))
