library(tidyverse)


contact_campaign_report <- read_csv("funnel_update/contact_campaign_report.csv")
marketing_mel_mql_report <- read_csv("funnel_update/marketing_mel_mql_report.csv")

account_id_contact_campaign_report <- contact_campaign_report  |> 
  drop_na(`Account ID`)  |> 
  mutate(`Mobile - Primary`= str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |> 
  rename_with(~str_replace_all(.,"[:punct:]","")) |>
  rename_with(~ str_replace_all(., " ", "_"))  |>
  rename_with(~ paste0(., "_Contact_Report"))  


marketing_mel_mql_report <- marketing_mel_mql_report |> 
  mutate(`Mobile - Primary`= str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |> 
  rename_with(~str_replace_all(.,"[:punct:]","")) |>
  rename_with(~ str_replace_all(., " ", "_"))  |>
  rename_with(~ paste0(., "_SFDC"))

marketing_account_id <- marketing_mel_mql_report %>% 
  filter(is.na(Latest_Campaign_SFDC)) %>% 
  drop_na(Account_ID_SFDC) %>% 
  left_join(account_id_contact_campaign_report, 
            by = c("Account_ID_SFDC" = "Account_ID_Contact_Report", "Mobile__Primary_SFDC" = "Mobile__Primary_Contact_Report")) 

marketing_account_id |> 
  select(Latest_Campaign_SFDC,Campaign_Name_Contact_Report, everything()) |> 
  drop_na(Campaign_Name_Contact_Report)->x


marketing_phone_number <- marketing_mel_mql_report %>% 
  filter(is.na(Latest_Campaign_SFDC)) %>% 
  drop_na(Mobile__Primary_SFDC) %>% 
  left_join(account_id_contact_campaign_report, by = c("Mobile__Primary_SFDC" = "Mobile__Primary_Contact_Report"))

marketing_phone_number |> 
  select(Latest_Campaign_SFDC,Campaign_Name_Contact_Report, everything()) |> 
  drop_na(Campaign_Name_Contact_Report)->y



## matching account id and phone numbers match more than just phone numbers, 
## so will use the first one as truth of source for updating past event
## 

marketing_account_id_column_order <- marketing_account_id |> 
  select(Created_Date_SFDC, Created_By_SFDC, Lead_Channel_SFDC, Lead_Category_SFDC, Lead_Source_SFDC, Lead_Status_SFDC,Unqualified_Reason_SFDC, `Opportunity_Created_Date_SFDC`,Opportunity_ID_SFDC ,Lead_Stage_SFDC,everything() )
  
write_csv(marketing_account_id, "mkt_sfdc_lead_w_contact_report.csv")
 
