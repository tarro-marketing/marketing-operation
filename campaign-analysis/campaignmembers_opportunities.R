library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)
library(here)

setwd(here("campaign-analysis"))

client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path"
)
email <- keyring::key_get(service = "googlesheets4", username = "email")

# configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

################### loading data ###########################


campaign_members <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time] mkt campaign report",
    range = "A1:AL"
  ) |>
  clean_names()


opportunities <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time]  Marketing Opportunity Report",
    range = "A1:Y"
  ) |>
  clean_names()

ids_join <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "IDs",
    range = "A1:J") |>
  clean_names()

#################### Cleaning Data ##########################

opt_contact_ids <- ids_join |>
  select(opportunity_id,
         contact_id) |>
  distinct(contact_id, .keep_all = TRUE)

campaign_members_opportunities_id <-
  campaign_members |>
  left_join(opt_contact_ids, by = c("contact_contact_id" = "contact_id"))

rm(campaign_members, ids_join, opt_contact_ids, client_secret_path, email) ## clean environment

opt_stage_at_closed_lost <- opportunities |>
  select(opportunity_id,
         stage_at_closed_lost,
         date_time_closed_won) |>
  distinct(opportunity_id, .keep_all = TRUE)

campaign_members_opportunities <-
  campaign_members_opportunities_id |>
  left_join(opt_stage_at_closed_lost, by = c("opportunity_id" = "opportunity_id"))

rm(campaign_members_opportunities_id,opportunities,opt_stage_at_closed_lost) ## clean environment
