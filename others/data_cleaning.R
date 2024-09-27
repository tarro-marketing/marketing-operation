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

opportunity_report <- "00OUo000001agjJMAQ"
opportunity <- sf_run_report(opportunity_report)

lead <- read_csv("data/sfdc_leads_us_state.csv")


#################### Cleaning Data ##########################


lead <- lead |>
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



rm(lead_ducplicates, lead_na_counts, camp_rep_na_counts, camp_rep_duplicates, contact_rep_na_counts, contact_rep_duplicates)


valid_us_states <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# Filter the dataset to keep only rows where the state column matches one of the valid US states
final_campaign_sfdc_lead <- final_campaign_sfdc_lead |>
  filter(StateProvince_text_only_SFDC %in% valid_us_states) |>
  rename(State = StateProvince_text_only_SFDC) |>
  mutate(across(where(is.character), ~ na_if(., ""))) |>
  drop_na(State)

priority_order <- c("Direct Mail", "SMS", "Paid Search (Google)", "Facebook/Newspaper/Radio", "Email", "SEO - Chinese (Getskt.com)", "SEO - English (Wondersco.com)", "Wechat/Wecom", "Redbook/Douyin/YouTube Channel/Influencer", "Referral", "Others(inc. Direct/Legacy Channel)")

gs4_auth_configure(path = "pw/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

google_sheet_grouping <- read_sheet(ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64", sheet = "Lead Channel Category")

unique_leads_count <- final_campaign_sfdc_lead %>%
  distinct(Lead_ID_SFDC) %>%
  count()


normalize_channel <- final_campaign_sfdc_lead |>
  separate_rows(Lead_Channel_SFDC, sep = ";") |>
  mutate(Lead_Channel_SFDC = str_replace(Lead_Channel_SFDC, "^\\s+", "")) |>
  left_join(google_sheet_grouping, by = c("Lead_Channel_SFDC" = "lead_source")) |>
  mutate(
    Channel_Rank = match(lead_channel, priority_order),
    Lead_Channel_SFDC = ifelse(is.na(Channel_Rank), "Others", Lead_Channel_SFDC)
  ) |>
  arrange(Lead_ID_SFDC, Channel_Rank) |>
  group_by(Lead_ID_SFDC) |>
  mutate(
    Total_Channels = n(), # Count total channels per lead
    Lead_Credit = if_else(Total_Channels > 1, 0.5, 1) # Assign partial or full credit
  ) |>
  slice_head(n = 2) |> # Keep top 2 channels
  ungroup()




write_csv(normalize_channel, "data/final_sfdc_lead.csv", na = "")
