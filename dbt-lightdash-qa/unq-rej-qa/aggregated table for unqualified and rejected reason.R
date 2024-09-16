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

lead <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] Marketing MEL/MQL Report",
    range = "A:AM"
  ) |>
  clean_names() |>
  rename_with(~ paste0(.x, "_lead"), everything())

opportunity <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time]  Marketing Opportunity Report",
    range = "A:S"
  ) |>
  clean_names() |>
  rename_with(~ paste0(.x, "_opportunity"), everything())

channel <- read_sheet(
  ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64",
  sheet = "Lead Channel Category"
)

lead_opportunity <- lead |>
  left_join(opportunity,
    by = c("opportunity_id_lead" = "opportunity_id_opportunity")
  ) |>
  mutate(
    lead_channel_opportunity =
      str_remove_all(lead_channel_opportunity, "\\[|\\]|\""),
    lead_channel_opportunity =
      str_replace_all(lead_channel_opportunity, "\\,", "\\;"),
    lead_chnl_lead_opp = coalesce(lead_channel_opportunity, lead_channel_lead)
  ) |>
  select(lead_chnl_lead_opp, everything())

lead_stages <-
  lead_opportunity |>
  mutate(
    is_onboarded = case_when(stage_lead == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    is_sql = case_when(
      is_onboarded == TRUE ~ TRUE, opportunity_id_lead != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    is_mql = case_when(
      is_sql == TRUE ~ TRUE, is_onboarded == TRUE ~ TRUE,
      !(is.na(latest_mql_time_stamp_lead)) &
        !(unqualified_reason_lead %in%
          c(
            "Current Client",
            "Duplicate",
            "Not a Restaurant"
          )) ~ TRUE,
      TRUE ~ FALSE
    ),
    is_mel = case_when(
      is_sql == TRUE ~ TRUE,
      is_onboarded == TRUE ~ TRUE,
      is_mql == TRUE ~ TRUE,
      !(is.na(first_mel_timestamp_lead)) &
      !(unqualified_reason_lead %in% c("Current Client", "Duplicate")) ~ TRUE,
      TRUE ~ FALSE
    )
  )

lead_chnl_credits <- lead_stages |>
  mutate(id = row_number()) |>
  select(id, is_mel, is_mql, is_sql, is_onboarded, lead_chnl_lead_opp, everything()) |>
  mutate(lead_chnl_lead_opp_split = str_split(lead_chnl_lead_opp, ";\\s*")) |>
  unnest(lead_chnl_lead_opp_split) |>
  group_by(id) |>
  mutate(opp_lead_value = 1 / n()) |>
  ungroup() |>
  select(lead_chnl_lead_opp, lead_chnl_lead_opp_split, opp_lead_value, is_mel, is_mql, is_sql, is_onboarded , everything())

lead_chnl_category <-
  lead_chnl_credits |>
  left_join(channel, by = join_by(lead_chnl_lead_opp_split == lead_source)) |>
  select(lead_channel , everything())


write_sheet(lead_chnl_category, ss="1CoYSseTFLnT1m-aMl6RIi0zNW0OdLh3K2t-c9sRaqgI", sheet="lead_chnl_credits")

write_csv(lead_chnl_category, "dbt-lightdash-qa/unq-rej-qa/lead_stages.csv", na="NA")
