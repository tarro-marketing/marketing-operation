##################### Loadning Library ########################
library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(here)

########################## gseeht #############################


gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


inbound_call <- read_sheet("1ZSCgZYODPvh3Bw1hoS98PocYr6bLmuDx1wjNZzipb2E",
                                      sheet = "data")

#################### Loading Data ##########################
config <- yaml::read_yaml("C:/Users/skt/Documents/API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


performance_mkt_lead_sfdc <- "00OUo000001RZxBMAW"
performance_mkt_lead <- sf_run_report(performance_mkt_lead_sfdc)


# performance_mkt_lead  <- read_csv("~/Performance-Marketing/report1710370343794.csv")


campaign_lead_report_sfdc <- "00OUo000001KSsPMAW"
campaign_lead_report <- sf_run_report(campaign_lead_report_sfdc)

campaign_w_contacts_report_sfdc <- "00OUo0000017gpxMAA"
contact_campaign_report <- sf_run_report(campaign_w_contacts_report_sfdc)

rm(performance_mkt_lead_sfdc, campaign_w_contacts_report_sfdc, campaign_lead_report_sfdc, config)


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

performance_sfdc <- final_campaign_sfdc_lead

#########################################################################
first_join <- inbound_call %>%
  left_join(performance_sfdc, by = c("From Number" = "Mobile__Primary_SFDC"))

# Second join attempt with Business Phone for rows that didn't match in the first join
second_join <- inbound_call %>%
  filter(is.na(first_join$Lead_ID_SFDC)) %>%
  left_join(performance_sfdc, by = c("From Number" = "Business_Phone_SFDC"), relationship = "many-to-many")

# Combine rows from the first join with matched rows with the second join
# Also ensuring no duplicate rows from the first join make it into the final data
combined <- bind_rows(
  first_join %>% filter(!is.na(Lead_ID_SFDC)),
  second_join
)


combined |> 
  filter(Channel == "SMS") -> SMS_inbound_call_mobile

sheet_write(SMS_inbound_call_mobile, ss = "1aWD4PBs_y4K-7voUd9dUyFF_wXUr0UMVLszUdii2KBw", sheet = "Inbound Call")


