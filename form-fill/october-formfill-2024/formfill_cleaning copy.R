library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(here)


client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path" # path to google developer credential
)

email <- keyring::key_get(service = "googlesheets4", username = "email")

# configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


folder_path <- here("form-fill", "october-formfill-2024","10-1_to_10-16")
filenames <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

col_types <- cols(
  `手机号码` = col_character(),
  `Mobile Number` = col_character(),
  `utm_id` = col_character(),
  .default = col_guess()  # This will guess other column types
)

# Read and combine the files with consistent column types
combo_data <- purrr::map_df(filenames,
                            ~read_csv(.x, col_types = col_types) |>
                              mutate(filename = .x))




combo_data_clean <- combo_data  |>
  mutate(
    name = if ("Last Name" %in% colnames(combo_data)) {
      coalesce(`姓氏`, `Last Name`)
    } else {
      `姓氏`
    },
    phone_number = if ("Mobile Number" %in% colnames(combo_data)) {
      coalesce(`手机号码`, `Mobile Number`)
    } else {
      `手机号码`
    },
    state = if ("State" %in% colnames(combo_data)) {
      coalesce(`州`, `State`)
    } else {
      `州`
    },
    cuisine_type = if ("Cuisine Type" %in% colnames(combo_data)) {
      coalesce(`餐馆菜系`, `Cuisine Type`)
    } else {
      `餐馆菜系`
    }
  )  |>
  rename("email" = `您的邮箱地址`) |>
  # Dynamically select columns to remove only if they exist
  select(-c(
    if ("姓氏" %in% colnames(combo_data)) "姓氏" else NULL,
    if ("Last Name" %in% colnames(combo_data)) "Last Name" else NULL,
    if ("手机号码" %in% colnames(combo_data)) "手机号码" else NULL,
    if ("Mobile Number" %in% colnames(combo_data)) "Mobile Number" else NULL,
    if ("州" %in% colnames(combo_data)) "州" else NULL,
    if ("State" %in% colnames(combo_data)) "State" else NULL,
    if ("餐馆菜系" %in% colnames(combo_data)) "餐馆菜系" else NULL,
    if ("Cuisine Type" %in% colnames(combo_data)) "Cuisine Type" else NULL
  )) |>
  select(where(~ !all(is.na(.)))) |>
  filter(!str_detect(name, "(?i)marketing|test"))



ordered_columns <- c("name", "email", "phone_number", "resturantname", "state", "cuisine_type",
                     "utm_campaign", "utm_source", "utm_medium",
                     "Form Name (ID)", "Submission ID", "Created At", "User ID",
                     "User Agent", "User IP", "Referrer", "Terms and Conditions", "filename")

combo_data_ordered <- combo_data_clean  |>
  select(all_of(ordered_columns))


combo_data2 <- combo_data_ordered |>
  mutate(phone_number = str_replace_all(phone_number, "-", "") |>
           str_replace_all("[^0-9]", "")) |>
  mutate(
    channel = case_when(
      str_detect(filename, "(?i)SEM|Blog Pages Contact us") | str_detect(utm_source,"(?i)google") ~ "SEM",
      str_detect(filename, "(?i)SEO|Contact us") ~ "SEO",
      str_detect(filename, "(?i)SMS") ~ "SMS",
      str_detect(filename, "(?i)Brizo|(?i)DM") | str_detect(utm_medium,"(?i)DirectMail")~ "DM",
      str_detect(filename, "(?i)complete|(?i)guide") | str_detect(utm_medium,"(?i)complete-guide")~ "Complete Guide")) |>
  select(channel, everything()) |>
  rename(Create_Date = `Created At`) |>
  mutate(
    Create_Date = as_date(Create_Date), # Ensure Create_Date is in Date format
    week_start = floor_date(Create_Date, "week", week_start = 1),  # Start of the week (Monday)
    week_end = ceiling_date(Create_Date, "week", week_start = 1) - 1,  # End of the week (Sunday)
    week = paste0(format(week_start, "%m/%d"), "-", format(week_end, "%m/%d"))  # Format as MM/DD-MM/DD
  )  |>
  select(-week_start, -week_end) |>
  select(week,channel, everything())

combo_data2 |>
  group_by(channel) |>
  summarize(form_fill_numbers=n()) |>
  arrange(desc(form_fill_numbers)) -> summary_table

sheet_write(combo_data2, ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE", sheet = "October Formfill")

sheet_append(combo_data2, ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE", sheet = "All Formfills")


rm(list=ls())

