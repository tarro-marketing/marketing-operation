library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)


client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path"
)
email <- keyring::key_get(service = "googlesheets4", username = "email")

# configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

################### loading data ###########################

contact_campaign_report <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] mkt campaign report",
    range = "A2:AH"
  )

dm_lead <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Marketing MEL/MQL Report",
    range = "A2:AK"
  )

campaign_lead_report <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Campaign Lead Report",
    range = "A2:AC"
  )


#################### Cleaning Data ##########################


dm_lead <- dm_lead |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_SFDC")) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .)),
    Business_Phone_SFDC = str_replace_all(
      str_replace_all(Business_Phone_SFDC, "-", ""),
      "[^0-9]", ""
    ),
    flow = case_when(
      Created_By_SFDC %in% c(
        "JotForm Integration User",
        "Carlito Academia"
      ) ~ "webflow",
      TRUE ~ "inbound call"
    ),
    types = case_when(
      Created_By_SFDC == "JotForm Integration User" ~ "jotform",
      Created_By_SFDC == "Carlito Academia" ~ "qr scan",
      TRUE ~ "inbound call"
    ),
    Onboarded = case_when(Stage_SFDC == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    CW = case_when(Stage_SFDC == "Closed Won" ~ TRUE, TRUE ~ FALSE),
    SQL = case_when(
      CW == TRUE ~ TRUE, Opportunity_ID_SFDC != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    MQL = case_when(
      SQL == TRUE ~ TRUE, CW == TRUE ~ TRUE, Menu_Type_SFDC != "" &
        StateProvince_text_only_SFDC != "" &
        (Created_By_SFDC == "JotForm Integration User" |
          Lead_Status_SFDC %in% c("Converted", "AE Assigned")
        ) & !(Unqualified_Reason_SFDC %in% c(
        "Current Client",
        "Duplicate",
        "Not a Restaurant"
      )) ~ TRUE,
      TRUE ~ FALSE
    ),
    MEL = case_when(
      !(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate")) ~ TRUE,
      TRUE ~ FALSE
    )
  )


campaign_lead_report2 <- campaign_lead_report |>
  mutate(
    `Mobile - Primary` =
      str_replace_all(`Mobile - Primary`, "[^\\d]", "")
  ) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Campaign_Lead")) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .))
  )

contact_campaign_report2 <- contact_campaign_report |>
  mutate(
    `Contact: Mobile - Primary` =
      as.character(`Contact: Mobile - Primary`)
  ) |>
  mutate(
    `Contact: Mobile - Primary` =
      str_replace_all(`Contact: Mobile - Primary`, "[^\\d]", "")
  ) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Contact_Report")) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .))
  ) |>
  mutate(
    Lead_Mobile__Primary_Contact_Report =
      as.character(Lead_Mobile__Primary_Contact_Report)
  )



#################### Joining Data ##########################

campaign_lead_report_join <- campaign_lead_report2 |>
  select(
    Latest_Campaign_Campaign_Lead,
    Mobile__Primary_Campaign_Lead,
    Lead_ID_Campaign_Lead
  ) |>
  distinct(Lead_ID_Campaign_Lead, .keep_all = TRUE)

contact_report_join <- contact_campaign_report2 |>
  select(
    Campaign_Name_Contact_Report,
    Lead_Mobile__Primary_Contact_Report,
    Lead_Converted_Account_Account_ID_Contact_Report
  ) |>
  distinct(Lead_Converted_Account_Account_ID_Contact_Report, .keep_all = TRUE)

no_latest_campaign <- dm_lead |>
  filter(is.na(Latest_Campaign_SFDC)) |>
  left_join(campaign_lead_report_join, by = c(
    "Lead_ID_SFDC" = "Lead_ID_Campaign_Lead",
    "Mobile__Primary_SFDC" = "Mobile__Primary_Campaign_Lead"
  )) |>
  left_join(contact_report_join,
    by = c(
      "Account_ID_SFDC" = "Lead_Converted_Account_Account_ID_Contact_Report",
      "Mobile__Primary_SFDC" = "Lead_Mobile__Primary_Contact_Report"
    )
  ) |>
  mutate(
    campaign_name =
      coalesce(
        Latest_Campaign_SFDC,
        Latest_Campaign_Campaign_Lead,
        Campaign_Name_Contact_Report
      )
  )


has_latest_campaign <- dm_lead |>
  filter(Latest_Campaign_SFDC != "-") |>
  mutate(
    Campaign_Name_Contact_Report = "",
    Latest_Campaign_Campaign_Lead = "",
    campaign_name = Latest_Campaign_SFDC
  )

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)

colnames(final_campaign_sfdc_lead)

final_campaign_sfdc_lead <- final_campaign_sfdc_lead |>
  select(
    flow, types, MEL, MQL, SQL, CW, Onboarded, campaign_name,
    Latest_Campaign_SFDC, Latest_Campaign_Campaign_Lead,
    Campaign_Name_Contact_Report, StateProvince_text_only_SFDC,
    everything()
  ) |>
  arrange(desc(Created_Date_SFDC))


write_csv(final_campaign_sfdc_lead,
  "overall-data/final_sfdc_lead.csv",
  na = ""
)


DM_Lead <- final_campaign_sfdc_lead |>
  filter(Lead_Channel_SFDC %in% "DM")

write_sheet(DM_Lead,
  ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU",
  sheet = "SFDC Leads + Campaigns"
)

rm(list = setdiff(ls(), c("DM_Lead")))
