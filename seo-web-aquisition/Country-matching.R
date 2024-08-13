library(tidyverse)
library(countrycode)
seo_web <- read_csv("seo-web-aquisition/Getskt.com Web Traffic Acquisition (1_1 - 5_29) - Country.csv")


# Map countries to regions using the countrycode function
seo_web$Region <- countrycode(seo_web$Country, "country.name", "region")

# Print the resulting data frame
print(seo_web)


library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

write_sheet(seo_web, ss="16t0SK29Vs8A-Dy5ddmk63pppZLwOPWaH8EjdwjwFywY", sheet = "Country")

