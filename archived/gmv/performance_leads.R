
##################### Loadning Library ########################
library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(here)

#################### Loading Data ##########################
config <- yaml::read_yaml("C:/Users/skt/Documents/API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


performance_mkt_lead_sfdc <- "00OUo000001MO9ZMAW"
performance_mkt_lead <- sf_run_report(performance_mkt_lead_sfdc)

campaign_lead_report_sfdc <- "00OUo000001KSsPMAW"
campaign_lead_report <- sf_run_report(campaign_lead_report_sfdc)

campaign_w_contacts_report_sfdc <- "00OUo0000017gpxMAA"
contact_campaign_report <- sf_run_report(campaign_w_contacts_report_sfdc)

rm(performance_mkt_lead_sfdc, campaign_w_contacts_report_sfdc, campaign_lead_report_sfdc, config)

#################### Cleaning Data ##########################


performance_mkt_lead <- performance_mkt_lead |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_SFDC")) |> 
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .)),
    Business_Phone_SFDC = str_replace_all(str_replace_all(Business_Phone_SFDC, "-", ""), "[^0-9]", ""),
    flow = case_when(
      Created_By_SFDC %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
      TRUE ~ "inbound call"),
    types = case_when(
      Created_By_SFDC == "JotForm Integration User" ~ "jotform",
      Created_By_SFDC == "Carlito Academia" ~ "qr scan",
      TRUE ~ "inbound call"),
    Onboarded = case_when( Stage_SFDC == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    CW = case_when(Stage_SFDC == "Closed Won" ~ TRUE,TRUE ~ FALSE),
    SQL = case_when(CW == TRUE ~ TRUE,Opportunity_ID_SFDC != "" ~ TRUE,TRUE ~ FALSE),
    MQL = case_when(SQL == TRUE ~ TRUE,CW == TRUE ~ TRUE,Menu_Type_SFDC != "" &StateProvince_text_only_SFDC != "" &
                      ( Created_By_SFDC == "JotForm Integration User" |Lead_Status_SFDC %in% c("Converted", "AE Assigned")
                      ) &!(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate", "Not a Restaurant")) ~ TRUE,
                    TRUE ~ FALSE
    ))


campaign_lead_report <- campaign_lead_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Campaign_Lead")) |> 
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .)))


contact_campaign_report <- contact_campaign_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Contact_Report")) |> 
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .)))


lead_na_counts <- map_df(performance_mkt_lead, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
lead_ducplicates <- map_df(performance_mkt_lead, ~ sum(duplicated(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")

contact_rep_na_counts <- map_df(contact_campaign_report, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
contact_rep_duplicates <- map_df(contact_campaign_report, ~ sum(duplicated(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")

camp_rep_na_counts <- map_df(campaign_lead_report, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
camp_rep_duplicates <- map_df(contact_campaign_report, ~ sum(duplicated(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")



rm(lead_ducplicates, lead_na_counts, camp_rep_na_counts, camp_rep_duplicates, contact_rep_na_counts, contact_rep_duplicates)


#################### Joining Data ##########################

campaign_lead_report_join <- campaign_lead_report |>
  select(Latest_Campaign_Campaign_Lead, 
         Mobile__Primary_Campaign_Lead, 
         Lead_ID_Campaign_Lead) |>
  distinct(Lead_ID_Campaign_Lead, .keep_all = TRUE)

contact_report_join <- contact_campaign_report |>
  select(Campaign_Name_Contact_Report, 
         Mobile__Primary_Contact_Report, 
         Account_ID_Contact_Report) |>
  distinct(Account_ID_Contact_Report, .keep_all = TRUE)

no_latest_campaign <- performance_mkt_lead |>
  filter(is.na(Latest_Campaign_SFDC)) |>
  left_join(campaign_lead_report_join, by = c("Lead_ID_SFDC" = "Lead_ID_Campaign_Lead",
                                              "Mobile__Primary_SFDC"="Mobile__Primary_Campaign_Lead")) |> 
  left_join(contact_report_join,
            by = c(
              "Account_ID_SFDC" = "Account_ID_Contact_Report",
              "Mobile__Primary_SFDC" = "Mobile__Primary_Contact_Report")) |>
  mutate(campaign_name = coalesce(Latest_Campaign_SFDC,Latest_Campaign_Campaign_Lead, Campaign_Name_Contact_Report))


has_latest_campaign <- performance_mkt_lead |>
  filter(Latest_Campaign_SFDC != "-") |>
  mutate(
    Campaign_Name_Contact_Report = "",
    Latest_Campaign_Campaign_Lead ="",
    campaign_name = Latest_Campaign_SFDC)

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)

rm(campaign_lead_report_join, contact_report_join, has_latest_campaign, no_latest_campaign, campaign_lead_report, contact_campaign_report, performance_mkt_lead)


rm(performance_mkt_lead, campaign_lead_report, contact_campaign_report)

final_campaign_sfdc_lead <- final_campaign_sfdc_lead |> 
  select(flow, types, MQL, SQL, CW, Onboarded, campaign_name, Latest_Campaign_SFDC, Latest_Campaign_Campaign_Lead, Campaign_Name_Contact_Report, StateProvince_text_only_SFDC ,everything())


sheet_write(data = final_campaign_sfdc_lead, ss = "1atEwWjFjdpQxAy0LKTeiRpcD1Xrf08-wtWoK3XBfgew", sheet = "SFDC Leads")


