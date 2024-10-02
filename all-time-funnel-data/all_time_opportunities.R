
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

rm(client_secret_path, email)
################### loading data ###########################

all_opportunity__r <-
  read_sheet(
    ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
    sheet = "[all time]  Marketing Opportunity Report",
    range = "A:Z"
  ) |>
  clean_names()

#################### Cleaning Data ##########################

opportunity <- all_opportunity__r |> 
  mutate(SQL = TRUE,
         CW = if_else(stage == "Closed Won", TRUE,FALSE),
         Onboarded = if_else(stage == "Onboarded", TRUE,FALSE)
  )


rm(list = setdiff(ls(), c("opportunity")))



