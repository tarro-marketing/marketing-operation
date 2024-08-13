library(tidyverse)

library(salesforcer)
library(yaml)
library(httr)

config <- yaml::read_yaml("../../API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


my_report_id <- "00OUo000000x6z3MAA"
marketing_mel_mql_report <- sf_run_report(my_report_id)



my_report_id2 <- "00OUo0000017gpxMAA"
contact_campaign_report <- sf_run_report(my_report_id2)

rm(my_report_id, my_report_id2, config)

# contact_campaign_report <- read_csv("funnel_update/contact_campaign_report.csv")
# marketing_mel_mql_report <- read_csv("funnel_update/marketing_mel_mql_report.csv")

contact_campaign_report <- contact_campaign_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Contact_Report"))

marketing_mel_mql_report <- marketing_mel_mql_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_SFDC"))




lead_na_counts <- map_df(marketing_mel_mql_report, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
lead_ducplicates <- map_df(marketing_mel_mql_report, ~ sum(duplicated(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")

camp_rep_na_counts <- map_df(contact_campaign_report, ~ sum(is.na(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")
camp_rep_duplicates <- map_df(contact_campaign_report, ~ sum(duplicated(.))) |>
  pivot_longer(cols = everything(), names_to = "column_name", values_to = "counts")




rm(lead_ducplicates)
rm(lead_na_counts)
rm(camp_rep_na_counts)
rm(camp_rep_duplicates)

################################ Combine List##################################

campaign_report_join <- contact_campaign_report |>
  select(Campaign_Name_Contact_Report, Mobile__Primary_Contact_Report, Account_ID_Contact_Report) |>
  distinct(Account_ID_Contact_Report, .keep_all = TRUE)

no_latest_campaign <- marketing_mel_mql_report |>
  filter(Latest_Campaign_SFDC == "-") |>
  left_join(campaign_report_join,
    by = c(
      "Account_ID_SFDC" = "Account_ID_Contact_Report",
      "Mobile__Primary_SFDC" = "Mobile__Primary_Contact_Report"
    )
  ) |>
  rename_with(~ str_replace_all(., " ", "_"))

has_latest_campaign <- marketing_mel_mql_report |>
  filter(Latest_Campaign_SFDC != "-") |>
  mutate(
    Campaign_Name_Contact_Report = ""
  )

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)

####################### Basic Format and Lead Source ##########################

sfdc_lead__final <- final_campaign_sfdc_lead |>
  filter(is.na(Latest_Campaign_SFDC) | is.na(Campaign_Name_Contact_Report) |
    str_detect(Latest_Campaign_SFDC, regex("DM", ignore_case = TRUE)) |
    str_detect(Campaign_Name_Contact_Report, regex("DM", ignore_case = TRUE))) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .))
  ) |>
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
  ))


###################### Funnel ########################
sfdc_leads <- sfdc_lead__final |>
  mutate(
    Onboarded = case_when(
      Stage_SFDC == "Onboarded" ~ TRUE,
      TRUE ~ FALSE
    ),
    CW = case_when(
      Stage_SFDC == "Closed Won" ~ TRUE,
      TRUE ~ FALSE
    ),
    SQL = case_when(
      CW == TRUE ~ TRUE,
      Opportunity_ID_SFDC != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    MQL = case_when(
      SQL == TRUE ~ TRUE,
      CW == TRUE ~ TRUE,
      Menu_Type_SFDC != "" &
        StateProvince_text_only_SFDC != "" &
        (
          Created_By_SFDC == "JotForm Integration User" |
            Lead_Status_SFDC %in% c("Converted", "AE Assigned")
        ) &
        !(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate", "Not a Restaurant")
        ) ~ TRUE,
      TRUE ~ FALSE
    )
  )



write_csv(sfdc_leads, "recalculation-2024/clean-data/final_campaign_sfdc_lead.csv", na = "")



sfdc_leads |> 
  filter(Created_Date_SFDC >= ymd("2024-01-01") & Created_Date_SFDC <= ymd("2024-02-02"),
         Opportunity_Created_Date_SFDC >= ymd("2024-01-01") & Opportunity_Created_Date_SFDC <= ymd("2024-02-02"),
         Placeholder_Live_Date_SFDC >= ymd("2024-01-01") & Placeholder_Live_Date_SFDC <= ymd("2024-02-02"),
         Stage_SFDC == "Onboarded") -> x
  



#########################


duplicated_ids <- contact_campaign_report %>%
  filter(duplicated(Account_ID_Contact_Report) |
    duplicated(Account_ID_Contact_Report, fromLast = TRUE)) %>%
  pull(Account_ID_Contact_Report) %>%
  unique()

# Step 2: Filter the original dataframe to only include rows with duplicated Account_ID_Contact_Report
duplicate_ids_contact <- contact_campaign_report %>%
  filter(Account_ID_Contact_Report %in% duplicated_ids)

rm(campaign_report_join, marketing_mel_mql_report)


write_csv(duplicate_ids_contact, "recalculation-2024/clean-data/duplicate_ids_contact.csv")

#########################


duplicated_ids <- contact_campaign_report %>%
  filter(duplicated(Account_ID_Contact_Report) |
    duplicated(Account_ID_Contact_Report, fromLast = TRUE)) %>%
  pull(Account_ID_Contact_Report) %>%
  unique()

# Step 2: Filter the original dataframe to only include rows with duplicated Account_ID_Contact_Report
duplicate_ids_contact <- contact_campaign_report %>%
  filter(Account_ID_Contact_Report %in% duplicated_ids)

rm(campaign_report_join, contact_campaign_report, marketing_mel_mql_report)


write_csv(duplicate_ids_contact, "recalculation-2024/clean-data/duplicate_ids_contact.csv")
