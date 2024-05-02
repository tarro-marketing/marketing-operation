library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring) # save config and password

#################### loading config ########################

gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


salesforce_username <- "youjia.chen@wondersco.com"
salesforce_password <- keyring::key_get(service = "salesforce", username = "password")
salesforce_security_token <- keyring::key_get(service = "salesforce", username = "security_token")

sf_auth(
  username = salesforce_username,
  password = salesforce_password,
  security_token = salesforce_security_token
)

#################### Loading Data ##########################
select_columns <- "Id, Lead_Type__c, LEAD_CATEGORY__C, lead_source__c, CreatedBy.Name, Owner.Name, MobilePhone, Phone, FirstName, LastName, Company, MENU_TYPE__C, State, Country, LAST_ACTIVITY_DATE__C, LATEST_CAMPAIGN__r.Name, CreatedDate, WECHAT_EXTERNAL_ID__C, Status, LEAD_STAGE__C, UNQUALIFIED_REASON__C, REJECTED_REASON__C, ConvertedOpportunityId, Promotions__c"

soql_query24 <- paste(
  "SELECT", select_columns,
  "FROM Lead",
  "WHERE CreatedDate = THIS_YEAR",
  "AND lead_source__c INCLUDES ('DM')",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  sep = " "
)

leads_2024 <- sf_query(soql_query24)


soql_query23 <- paste(
  "SELECT", select_columns,
  "FROM Lead",
  "WHERE CreatedDate = LAST_YEAR",
  "AND lead_source__c INCLUDES ('DM')",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  sep = " "
)
leads_2023 <- sf_query(soql_query23)
soql_query22 <- paste(
  "SELECT", select_columns,
  "FROM Lead",
  "WHERE CALENDAR_YEAR(CreatedDate) = 2022",
  "AND lead_source__c INCLUDES ('DM')",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  sep = " "
)
leads_2022 <- sf_query(soql_query22)

leads <- bind_rows(leads_2022, leads_2023, leads_2024)

opportunity_ids <- na.omit(leads$ConvertedOpportunityId)


chunk_vector <- function(v, n) {
  split(v, ceiling(seq_along(v) / n))
}

chunked_opportunity_ids <- chunk_vector(opportunity_ids, 200) # Adjust the chunk size as needed

query_opportunities_results <- list()

for (chunk in chunked_opportunity_ids) {
  ids_string <- paste0("'", paste(chunk, collapse = "','"), "'")
  query_chunk <- sf_query(paste0("SELECT Id, CreatedDate, live_date__c, StageName, restaurant_id__c, Account_Name__c,AccountId FROM Opportunity WHERE Id IN (", ids_string, ")"))
  query_opportunities_results[[length(query_opportunities_results) + 1]] <- query_chunk
}

query_opportunities_combined <- do.call(rbind, query_opportunities_results)


leads_alltime <- merge(leads, query_opportunities_combined, by.x = "ConvertedOpportunityId", by.y = "Id", all.x = TRUE)


dm_lead <- leads_alltime |>
  rename(
    "Opportunity ID" = "ConvertedOpportunityId",
    "Created By" = "CreatedBy.Name",
    "Last Name" = "LastName",
    "Lead Stage" = "Lead_Stage__c",
    "Lead Owner" = "Owner.Name",
    "State/Province (text only)" = "State",
    "Opportunity: Account" = "Account_Name__c",
    "Stage" = "StageName",
    "Lead ID" = "Id",
    "Created Date" = "CreatedDate.x",
    "Latest Campaign" = "Latest_Campaign__r.Name",
    "Lead Source" = "Lead_Type__c",
    "Business Phone" = "Phone", 
    "Lead Status" = "Status",
    "Opportunity: Created Date" = "CreatedDate.y",
    "Company / Account" = "Company",
    "First Name" = "FirstName",
    "Lead Category" = "Lead_Category__c",
    "Menu Type" = "Menu_Type__c",
    "Promotions" = "Promotions__c",
    "Unqualified Reason" = "Unqualified_Reason__c",
    "Placeholder Live Date" = "Live_Date__c",
    "Country (text only)" = "Country",
    "Latest Re-engaged Date" = "Last_Activity_Date__c",
    "Lead Channel" = "Lead_Source__c",
    "Mobile - Primary" = "MobilePhone",
    "Rejected Reason" = "Rejected_Reason__c",
    "WeChat External Id" = "WeChat_External_Id__c",
    "Restaurant ID" = "Restaurant_ID__c",
    "Account ID"="AccountId"
  )

rm(list = setdiff(ls(), c("dm_lead")))


campaign_lead_report_sfdc <- "00OUo000001KSsPMAW"
campaign_lead_report <- sf_run_report(campaign_lead_report_sfdc)

campaign_w_contacts_report_sfdc <- "00OUo0000017gpxMAA"
contact_campaign_report <- sf_run_report(campaign_w_contacts_report_sfdc)

rm(dm_lead_sfdc, campaign_w_contacts_report_sfdc, campaign_lead_report_sfdc, config)

#################### Cleaning Data ##########################


dm_lead <- dm_lead |>
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
      TRUE ~ FALSE),
  MEL = case_when(!(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate")) ~ TRUE,
    TRUE ~ FALSE)
  )


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


lead_na_counts <- map_df(dm_lead, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
lead_ducplicates <- map_df(dm_lead, ~ sum(duplicated(.))) |>
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

no_latest_campaign <- dm_lead |>
  filter(is.na(Latest_Campaign_SFDC)) |>
  left_join(campaign_lead_report_join, by = c("Lead_ID_SFDC" = "Lead_ID_Campaign_Lead",
                                              "Mobile__Primary_SFDC"="Mobile__Primary_Campaign_Lead")) |> 
  left_join(contact_report_join,
    by = c(
      "Account_ID_SFDC" = "Account_ID_Contact_Report",
      "Mobile__Primary_SFDC" = "Mobile__Primary_Contact_Report")) |>
  mutate(campaign_name = coalesce(Latest_Campaign_SFDC,Latest_Campaign_Campaign_Lead, Campaign_Name_Contact_Report))


has_latest_campaign <- dm_lead |>
  filter(Latest_Campaign_SFDC != "-") |>
  mutate(
    Campaign_Name_Contact_Report = "",
    Latest_Campaign_Campaign_Lead ="",
    campaign_name = Latest_Campaign_SFDC)

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)

rm(campaign_lead_report_join, contact_report_join, has_latest_campaign, no_latest_campaign, campaign_lead_report, contact_campaign_report, dm_lead)


rm(dm_lead, campaign_lead_report, contact_campaign_report)

final_campaign_sfdc_lead <- final_campaign_sfdc_lead |> 
  select(flow, types,MEL, MQL, SQL, CW, Onboarded, campaign_name, Latest_Campaign_SFDC, Latest_Campaign_Campaign_Lead, Campaign_Name_Contact_Report, StateProvince_text_only_SFDC ,everything()) |> 
  arrange(desc(Created_Date_SFDC))


write_csv(final_campaign_sfdc_lead, "overall-data/final_sfdc_lead.csv", na = "")

range_write(data=final_campaign_sfdc_lead, ss = "18viPByX4RQQx6D7PBC7COCnZwQSPB5bEjPBxLEDx8gU", sheet = "SFDC Leads + Campaigns", range = "A1")

rm(list = setdiff(ls(), c("final_campaign_sfdc_lead")))

