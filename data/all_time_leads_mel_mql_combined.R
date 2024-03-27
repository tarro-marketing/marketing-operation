library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(keyring) # save config and password
library(googlesheets4)

#################### Loading Data ############################

salesforce_username <- "youjia.chen@wondersco.com" # This can be hardcoded as it's not sensitive
salesforce_password <- keyring::key_get(service = "salesforce", username = "password")
salesforce_security_token <- keyring::key_get(service = "salesforce", username = "security_token")

sf_auth(
  username = salesforce_username,
  password = salesforce_password,
  security_token = salesforce_security_token
)

select_columns <- "Id, LeadSource, LEAD_CATEGORY__C, lead_source__c, CreatedById, OwnerId, MobilePhone, Phone, FirstName, LastName, Company, MENU_TYPE__C, State, Country, LAST_ACTIVITY_DATE__C, LATEST_CAMPAIGN__C, CreatedDate, WECHAT_EXTERNAL_ID__C, Status, LEAD_STAGE__C, UNQUALIFIED_REASON__C, REJECTED_REASON__C, ConvertedOpportunityId, Promotions__c"

soql_query24 <- paste(
  "SELECT", select_columns, 
  "FROM Lead",
  "WHERE CreatedDate = THIS_YEAR",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  "AND isdeleted = FALSE",
  "LIMIT 20000",  # Adjust the limit as needed
  sep=" "  # Separator for the paste function
)
leads_2024 <- sf_query(soql_query24)
soql_query23 <- paste(
  "SELECT", select_columns, 
  "FROM Lead",
  "WHERE CreatedDate = LAST_YEAR",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  "AND isdeleted = FALSE",
  "LIMIT 20000",  # Adjust the limit as needed
  sep=" "  # Separator for the paste function
)
leads_2023 <- sf_query(soql_query23)
soql_query22 <- paste(
  "SELECT", select_columns, 
  "FROM Lead",
  "WHERE CALENDAR_YEAR(CreatedDate) = 2022",
  "AND (",
  "(lead_type__c = 'AE Sourced' AND lead_source__c INCLUDES ('Marketing Referral'))",
  "OR (lead_type__c = 'Marketing Inbound')",
  ")",
  "AND isdeleted = FALSE",
  "LIMIT 20000",  # Adjust the limit as needed
  sep=" "  # Separator for the paste function
)
leads_2022 <- sf_query(soql_query22)

leads <- bind_rows(leads_2022, leads_2023, leads_2024)

opportunity_fields <- "Id, AccountId, CreatedDate, StageName,live_date__c,restaurant_id__c,accountid"
account_fileds <- "id, name"

rm(list=setdiff(ls(), c("leads", "opportunity_fields","account_fileds")))

write_csv(leads,"data/all_leads.csv")
