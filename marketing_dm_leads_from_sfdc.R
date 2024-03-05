library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)

#################### Loading Data ##########################
config <- yaml::read_yaml("../../API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


dm_lead_sfdc <- "00OUo000000x6z3MAA"
dm_lead <- sf_run_report(dm_lead_sfdc)

campaign_lead_report_sfdc <- "00OUo000001KSsPMAW"
campaign_lead_report <- sf_run_report(campaign_lead_report_sfdc)

campaign_w_contacts_report_sfdc <- "00OUo0000017gpxMAA"
contact_campaign_report <- sf_run_report(campaign_w_contacts_report_sfdc)

rm(dm_lead_sfdc, campaign_w_contacts_report_sfdc, campaign_lead_report_sfdc,config)


#################### Cleaning Data ##########################


dm_lead <- dm_lead |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_SFDC"))

campaign_lead_report <- campaign_lead_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_SFDC"))

contact_campaign_report <- contact_campaign_report |>
  mutate(`Mobile - Primary` = str_replace_all(`Mobile - Primary`, "[^\\d]", "")) |>
  rename_with(~ str_replace_all(., "[:punct:]", "")) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_Contact_Report"))

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

rm(lead_ducplicates,lead_na_counts,camp_rep_na_counts,camp_rep_duplicates,contact_rep_na_counts,contact_rep_duplicates)


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
  mutate(campaign_name = coalesce(Latest_Campaign_SFDC, Campaign_Name_Contact_Report))

has_latest_campaign <- marketing_mel_mql_report |>
  filter(Latest_Campaign_SFDC != "-") |>
  mutate(
    Campaign_Name_Contact_Report = "",
    campaign_name = Latest_Campaign_SFDC
  )

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)

write_csv(final_campaign_sfdc_lead, "clean-data/final_campaign_sfdc_lead.csv")

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


write_csv(duplicate_ids_contact, "clean-data/duplicate_ids_contact.csv")