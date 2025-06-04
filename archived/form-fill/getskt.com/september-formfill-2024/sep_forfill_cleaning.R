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


folder_path <- here("form-fill", "september-formfill-2024","untitled folder")

sep_form <-
  list.files(folder_path,  pattern = "\\.csv$" , full.names = TRUE) |>
  map_df(~read_csv(.))


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



  sep_form <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE) |>
    map_df(~ {
      data <- read_csv(.x)
      data |>
        mutate(
          name = if (any(grepl("(?i)last name", colnames(data))) || any(grepl("(?i)姓氏", colnames(data)))) {
            coalesce(!!!syms(intersect(c("姓氏", "Last Name"), colnames(data))))
          },
          phone_number = if (any(grepl("(?i)mobile number", colnames(data))) || any(grepl("(?i)手机号码", colnames(data)))) {
            coalesce(!!!syms(intersect(c("手机号码", "Mobile Number"), colnames(data))))
          },
          state = if (any(grepl("(?i)state", colnames(data))) || any(grepl("(?i)州", colnames(data)))) {
            coalesce(!!!syms(intersect(c("州", "State"), colnames(data))))
          },
          cuisine_type = if (any(grepl("(?i)cuisine type", colnames(data))) || any(grepl("(?i)餐馆菜系", colnames(data)))) {
            coalesce(!!!syms(intersect(c("餐馆菜系", "Cuisine Type"), colnames(data))))
          },
          email = if (any(grepl("(?i)Email", colnames(data))) || any(grepl("(?i)您的邮箱地址", colnames(data)))) {
            coalesce(!!!syms(intersect(c("Email", "您的邮箱地址"), colnames(data))))
          },
          campaign = str_extract(data$Referrer, "(?<=getskt\\.com/)[^/]+")
        ) |>
        select(-c(
          if ("姓氏" %in% colnames(data)) "姓氏" else NULL,
          if ("Last Name" %in% colnames(data)) "Last Name" else NULL,
          if ("手机号码" %in% colnames(data)) "手机号码" else NULL,
          if ("Mobile Number" %in% colnames(data)) "Mobile Number" else NULL,
          if ("州" %in% colnames(data)) "州" else NULL,
          if ("State" %in% colnames(data)) "State" else NULL,
          if ("餐馆菜系" %in% colnames(data)) "餐馆菜系" else NULL,
          if ("Cuisine Type" %in% colnames(data)) "Cuisine Type" else NULL
        ))
    }) |>
    select(name, phone_number, campaign, `Form Name (ID)`, everything()) |>
    select(where(~ !all(is.na(.)))) |>
    filter(!str_detect(name, "(?i)marketing|test"))




  write_sheet(sep_form,
              ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE",
              sheet = "Sep Campaign")
