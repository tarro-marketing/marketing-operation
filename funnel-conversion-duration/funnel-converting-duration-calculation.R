library(tidyverse)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


all_leads <- read_sheet(ss = "1xy2bw5ckuUod-5In2iZZuWxuIxvMqzI-fF_jODddGVg",
           sheet = "Copy of Marketing MEL/MQL Report",
           range = "A2:AE")

