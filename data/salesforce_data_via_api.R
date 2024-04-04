library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(keyring) # save config and password
library(googlesheets4)

#################### Loading Data ############################

salesforce_username <- "youjia.chen@wondersco.com"
salesforce_password <- keyring::key_get(service = "salesforce", username = "password")
salesforce_security_token <- keyring::key_get(service = "salesforce", username = "security_token")

sf_auth(
  username = salesforce_username,
  password = salesforce_password,
  security_token = salesforce_security_token
)

select_columns <- "Id, Lead_Type__c, LEAD_CATEGORY__C, lead_source__c, CreatedById, OwnerId, MobilePhone, Phone, FirstName, LastName, Company, MENU_TYPE__C, State, Country, LAST_ACTIVITY_DATE__C, LATEST_CAMPAIGN__C, CreatedDate, WECHAT_EXTERNAL_ID__C, Status, LEAD_STAGE__C, UNQUALIFIED_REASON__C, REJECTED_REASON__C, ConvertedOpportunityId, Promotions__c "

soql_query24 <- paste(
  "SELECT", select_columns,
  "FROM Lead",
  "WHERE CreatedDate = THIS_YEAR",
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
  query_chunk <- sf_query(paste0("SELECT Id, CreatedDate, live_date__c, StageName, restaurant_id__c, Account_Name__c FROM Opportunity WHERE Id IN (", ids_string, ")"))
  query_opportunities_results[[length(query_opportunities_results) + 1]] <- query_chunk
}

query_opportunities_combined <- do.call(rbind, query_opportunities_results)


leads_alltime <- merge(leads, query_opportunities_combined, by.x = "ConvertedOpportunityId", by.y = "Id", all.x = TRUE)

rm(list = setdiff(ls(), c("leads_alltime")))

leads_alltime <- leads_alltime |>
  rename(
    "Opportunity ID" = "ConvertedOpportunityId",
    "Created By" = "CreatedById",
    "Last Name" = "LastName",
    "Lead Stage" = "Lead_Stage__c",
    "Lead Owner" = "OwnerId",
    "State/Province (text only)" = "State",
    "Opportunity: Account" = "Account_Name__c",
    "Stage" = "StageName",
    "Lead ID" = "Id",
    "Created Date" = "CreatedDate.x",
    "Latest Campaign" = "Latest_Campaign__c",
    "Lead Source" = "Lead_Type__c",
    "Business Phone" = "Phone", "Lead Status" = "Status",
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
    "Restaurant ID" = "Restaurant_ID__c"
  )


valid_us_states <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)


############### identify MEL/MQL + Cohorted across all #####################
leads_cleaning <- leads_alltime |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  mutate(
    `Mobile__Primary` = str_replace_all(`Mobile__Primary`, "[^\\d]", ""),
    Business_Phone = str_replace_all(str_replace_all(Business_Phone, "-", ""), "[^0-9]", ""),
    flow = case_when(
      Created_By %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow", TRUE ~ "inbound call"
    ),
    types = case_when(
      Created_By == "JotForm Integration User" ~ "jotform",
      Created_By == "Carlito Academia" ~ "qr scan",
      TRUE ~ "inbound call"
    ),
    Onboarded = case_when(Stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    CW = case_when(Stage == "Closed Won" ~ TRUE, TRUE ~ FALSE),
    SQL = case_when(CW == TRUE ~ TRUE, Opportunity_ID != "" ~ TRUE, TRUE ~ FALSE),
    MQL = case_when(
      SQL == TRUE ~ TRUE, CW == TRUE ~ TRUE, Menu_Type != "" & StateProvince_text_only != "" &
        (Created_By == "JotForm Integration User" | Lead_Status %in% c("Converted", "AE Assigned")
        ) & !(Unqualified_Reason %in% c("Current Client", "Duplicate", "Not a Restaurant")) ~ TRUE,
      TRUE ~ FALSE
    )
  )


write_csv(leads_cleaning, "data/sfdc_leads.csv")




final_campaign_lead <- leads_cleaning |>
  filter(StateProvince_text_only %in% valid_us_states) |>
  rename(State = StateProvince_text_only) |>
  mutate(across(where(is.character), ~ na_if(., "")))

priority_order <- c("Direct Mail", "SMS", "Paid Search (Google)", "Facebook/Newspaper/Radio", "Email", "SEO - Chinese (Getskt.com)", "SEO - English (Wondersco.com)", "Wechat/Wecom", "Redbook/Douyin/YouTube Channel/Influencer", "Referral", "Others(inc. Direct/Legacy Channel)")

gs4_auth_configure(path = "pw/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

google_sheet_grouping <- read_sheet(ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64", sheet = "Lead Channel Category")

unique_leads_count <- final_campaign_lead %>%
  distinct(Lead_ID) %>%
  count()


credited_sfdc_leads <- final_campaign_lead |>
  separate_rows(Lead_Channel, sep = ";") |>
  mutate(Lead_Channel = str_replace(Lead_Channel, "^\\s+", "")) |>
  left_join(google_sheet_grouping, by = c("Lead_Channel" = "lead_source")) |>
  mutate(
    Channel_Rank = match(lead_channel, priority_order),
    Lead_Channel = ifelse(is.na(Channel_Rank), "Others", Lead_Channel)
  ) |>
  arrange(Lead_ID, Channel_Rank) |>
  group_by(Lead_ID) |>
  mutate(
    Total_Channels = n(), # Count total channels per lead
    Lead_Credit = if_else(Total_Channels > 1, 0.5, 1) # Assign partial or full credit
  ) |>
  slice_head(n = 2) |> # Keep top 2 channels
  ungroup()


summary_by_state <- credited_sfdc_leads |> 
  group_by(State) |> 
  summarise(total = sum(Lead_Credit))
  
grand_total <- sum(summary_by_state$total)

  
write_csv(credited_sfdc_leads, "data/sfdc_leads_us_state.csv")




###################################################################################################
############################## opportunity table ##################################################
###################################################################################################

## I do not have access to lead channel/lead_source__c so used account.lead_source__c instead (not ideal though)
soql_query_oppt <- "SELECT CreatedDate, Id, Account_Name__c, Name, Product_Name__c, Lead_First_Touch_Date__c,
                    Lead_Created_Date__c, StageName, CloseDate, Live_Date__c, Lead_Type__c,Lead_Source__c,
                    Restaurant_ID__c, Lead_Category__c, Account.BillingState, Account.BillingCountry, Promotions__c
                    FROM Opportunity
                    WHERE Lead_Category__c = 'Inbound'
                    AND (Opportunity_Type__c = 'New Sale' OR Opportunity_Type__c = NULL)"

opportunity_alltime <- sf_query(soql_query_oppt)

# opportunity_description <- sf_describe_object_fields("Opportunity") # checking the variables/fileds in Opportunity API

opportunity_alltime <- opportunity_alltime |> 
  rename("Created Date" ="CreatedDate", 
         "Opportunity ID" ="Id", 
         "Account Name" ="Account_Name__c", 
         "Opportunity Name" ="Name", 
         "Product Name" ="Product_Name__c", 
         "Lead First Touch Date" ="Lead_First_Touch_Date__c", 
         "Lead Created Date" ="Lead_Created_Date__c", 
         "Stage" ="StageName", "Close Date" ="CloseDate", 
         "Placeholder Live Date" ="Live_Date__c", 
         "Lead Source" ="Lead_Type__c", 
         "Lead Channel" ="Lead_Source__c", 
         "Restaurant ID" ="Restaurant_ID__c", 
         "Lead Category" ="Lead_Category__c", 
         "Billing State/Province (text only)" ="Account.BillingState", 
         "Billing Country (text only)" ="Account.BillingCountry", 
         "Promotions" ="Promotions__c")

rm(soql_query_oppt)

######################## identify sql / cw #####################################



opportunity <- opportunity_alltime |> 
  mutate(SQL = TRUE,
         CW = if_else(Stage == "Closed Won", TRUE,FALSE),
         Onboarded = if_else(Stage == "Onboarded",TRUE,FALSE)
  )

opportunity |> 
  


write_csv(opportunity, "data/sfdc_opportunity.csv")


opportunity_us_state <- opportunity |>
  filter(`Billing State/Province (text only)` %in% valid_us_states) |>
  rename(State = `Billing State/Province (text only)`) |>
  mutate(across(where(is.character), ~ na_if(., "")))

opportunity_us_state <-opportunity_us_state |>  
  separate_rows(`Lead Channel`, sep = ";") |>
  mutate("Lead Channel" = str_replace("Lead Channel", "^\\s+", "")) |>
  left_join(google_sheet_grouping, by = c("Lead Channel" = "lead_source")) |>
  mutate(
    Channel_Rank = match(lead_channel, priority_order),
    Lead_Channel = ifelse(is.na(Channel_Rank), "Others", Lead_Channel)
  ) |>
  arrange(`Opportunity ID`, Channel_Rank) |>
  group_by(`Opportunity ID`) |>
  mutate(
    Total_Channels = n(), # Count total channels per lead
    Lead_Credit = if_else(Total_Channels > 1, 0.5, 1) # Assign partial or full credit
  ) |>
  slice_head(n = 2) |> # Keep top 2 channels
  ungroup()


summary_by_state <- opportunity_us_state |> 
  group_by(State) |> 
  summarise(total = sum(Lead_Credit))

grand_total <- sum(summary_by_state$total)


write_csv(opportunity_us_state, "data/sfdc_opportunity_us_state.csv")




rm(list = setdiff(ls(), c("credited_sfdc_leads", "opportunity_us_state")))



