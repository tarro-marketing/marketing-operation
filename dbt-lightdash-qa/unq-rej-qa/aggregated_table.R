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


###############################################################################

lead_stages <- read_csv("dbt-lightdash-qa/unq-rej-qa/lead_stages.csv")

lead_stages

unq_counts <- lead_stages |>
  mutate(first_mql_created_month = format(first_mel_timestamp_lead, "%m-%Y")) |>
  filter(first_mql_created_month %in% c("06-2024", "07-2024", "08-2024")) |>
  group_by(first_mql_created_month, unqualified_reason_lead, lead_channel) |>
  summarise(total_mql = sum(is_mql == TRUE), .groups = "drop")

check <-
  unq_counts |>
  filter(unqualified_reason_lead == "Cannot Service Geography")

print(check)
