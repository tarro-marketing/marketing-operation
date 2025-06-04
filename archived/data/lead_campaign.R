library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)


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
    range = "A:AH"
  ) |>
  clean_names()


lead <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Marketing MEL/MQL Report",
    range = "A:AK"
  ) |>
  clean_names()

campaign_lead_report <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Campaign Lead Report",
    range = "A:AC"
  ) |>
  clean_names()


#################### Cleaning Data ##########################


lead_c <- lead |>
  mutate(mobile_primary = str_replace_all(mobile_primary, "[^\\d]", "")) |>
  mutate(
    business_phone = str_replace_all(
      str_replace_all(business_phone, "-", ""),
      "[^0-9]", ""
    ),
    flow = case_when(
      created_by %in% c(
        "JotForm Integration User",
        "Carlito Academia"
      ) ~ "webflow",
      TRUE ~ "inbound call"
    )) |>
  mutate(
    onboarded = case_when(stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    cw = case_when(stage == "Closed Won" ~ TRUE, TRUE ~ FALSE),
    sql = case_when(
      onboarded == TRUE ~ TRUE, opportunity_id != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    mql = case_when(
      sql == TRUE ~ TRUE,
      !is.na(latest_mql_time_stamp) &
        !(unqualified_reason %in% c(
          "Current Client",
          "Duplicate",
          "Not a Restaurant"
        )) ~ TRUE,
      TRUE ~ FALSE
    ),
    mel = case_when(
      mql == TRUE ~ TRUE,
      !is.na(first_mel_timestamp) &
        !(unqualified_reason %in% c(
          "Current Client",
          "Duplicate"
        )) ~ TRUE, # Add '~ TRUE' here
      TRUE ~ FALSE
    )
  ) |>
  rename_with(~ paste0(., "_sfdc"), .cols = -c(flow, mel, mql, sql, cw, onboarded,first_mel_timestamp)) |>
  select(first_mel_timestamp, mel, mql, sql, onboarded, cw, latest_campaign_sfdc, lead_channel_sfdc, unqualified_reason_sfdc,rejected_reason_sfdc,opportunity_id_sfdc, everything())


campaign_lead_report2 <- campaign_lead_report |>
  mutate(mobile_primary = str_replace_all(mobile_primary, "[^\\d]", "")) |>
  rename_with(~ paste0(., "_cl")) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .))
  )

contact_campaign_report2 <- contact_campaign_report |>
  mutate(contact_mobile_primary = as.character(contact_mobile_primary)) |>
  mutate(contact_mobile_primary = str_replace_all(contact_mobile_primary, "[^\\d]", "")) |>
  rename_with(~ paste0(., "_cr")) |>
  mutate(
    across(where(is.character), ~ recode(., "-" = NA_character_)),
    across(where(is.character), ~ if_else(. == "", NA_character_, .))
  ) |>
  mutate(lead_mobile_primary_cr = as.character(lead_mobile_primary_cr))


#################### Joining Data ##########################

campaign_lead_report_join <- campaign_lead_report2 |>
  select(
    latest_campaign_cl,
    mobile_primary_cl,
    lead_id_cl
  ) |>
  distinct(lead_id_cl, .keep_all = TRUE)

contact_report_join <- contact_campaign_report2 |>
  select(
    campaign_name_cr,
    lead_mobile_primary_cr,
    lead_converted_account_account_id_cr
  ) |>
  distinct(lead_converted_account_account_id_cr, .keep_all = TRUE)

no_latest_campaign <- lead_c |>
  filter(is.na(latest_campaign_sfdc)) |>
  left_join(campaign_lead_report_join, by = c(
    "lead_id_sfdc" = "lead_id_cl",
    "mobile_primary_sfdc" = "mobile_primary_cl"
  )) |>
  left_join(contact_report_join,
            by = c(
              "account_id_sfdc" = "lead_converted_account_account_id_cr",
              "mobile_primary_sfdc" = "lead_mobile_primary_cr"
            )
  ) |>
  mutate(
    campaign_name =
      coalesce(
        latest_campaign_sfdc,
        latest_campaign_cl,
        campaign_name_cr
      )
  )


has_latest_campaign <- lead_c |>
  filter(latest_campaign_sfdc != "-") |>
  mutate(
    campaign_name_cr = "",
    latest_campaign_cl = "",
    campaign_name = latest_campaign_sfdc
  )

final_campaign_sfdc_lead <- rbind(no_latest_campaign, has_latest_campaign)



final_campaign_sfdc_lead <- final_campaign_sfdc_lead |>
  select(
    flow, mel, mql, sql, cw, onboarded, campaign_name,
    latest_campaign_sfdc, latest_campaign_cl,
    campaign_name_cr, state_province_text_only_sfdc,
    everything()
  ) |>
  arrange(desc(first_mql_time_stamp_sfdc))


rm(list = setdiff(ls(), c("final_campaign_sfdc_lead")))

