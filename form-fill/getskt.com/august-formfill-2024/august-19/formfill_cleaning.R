library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(here)
library(janitor)


client_secret_path <- keyring::key_get(
  service = "googlesheets4",
  username = "client_secret_path" # path to google developer credential
)

email <- keyring::key_get(service = "googlesheets4", username = "email")
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


folder_path <- here("form-fill", "august-formfill-2024", "august-19", "raw-data")
filenames <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

col_types <- cols(
  `手机号码` = col_character(),
  `Mobile Number` = col_character(),
  .default = col_guess() # This will guess other column types
)

# Read and combine the files with consistent column types
combo_data <- purrr::map_df(
  filenames,
  ~ read_csv(.x, col_types = col_types) |>
    mutate(filename = .x)
)



# Function to select the first non-missing column among multiple options
select_first_existing_column <- function(df, columns) {
  existing_col <- columns[columns %in% colnames(df)]
  if (length(existing_col) > 0) {
    return(df[[existing_col[1]]])
  } else {
    return(NA)
  }
}

combo_data_clean <- combo_data |>
  mutate(
    name = select_first_existing_column(combo_data, c("姓氏", "Last Name")),
    phone_number = select_first_existing_column(
      combo_data, c("手机号码", "Mobile Number")
    ),
    state = select_first_existing_column(combo_data, c("州", "State")),
    cuisine_type = select_first_existing_column(
      combo_data, c("餐馆菜系", "Cuisine Type")
    ),
    email = select_first_existing_column(
      combo_data, c("您的邮箱地址", "Email Address")
    ) # nolint # nolint
  ) |>
  select(-any_of(c(
    "手机号码", "Mobile Number", "州",
    "State", "餐馆菜系", "Cuisine Type",
    "姓氏", "Last Name", "您的邮箱地址", "Email Address"
  ))) |>
  select(where(~ !all(is.na(.)))) |>
  filter(!str_detect(name, "(?i)marketing|test")) |>
  clean_names()

colnames(combo_data_clean)

combo_data_ordered <- combo_data_clean %>%
  select(name, phone_number, resturantname, state, everything())


combo_data2 <- combo_data_ordered |>
  mutate(phone_number = str_replace_all(phone_number, "-", "") |>
    str_replace_all("[^0-9]", "")) |>
  mutate(
    channel = case_when(
      str_detect(filename, "(?i)SEM|Blog Pages Contact us") |
        str_detect(utm_source, "(?i)google") ~ "SEM",
      str_detect(filename, "(?i)SEO|Contact us") ~ "SEO",
      str_detect(filename, "(?i)SMS") ~ "SMS",
      str_detect(filename, "(?i)Brizo|(?i)DM") |
        str_detect(utm_medium, "(?i)DirectMail") ~ "DM",
      str_detect(filename, "(?i)complete|(?i)guide") |
        str_detect(utm_medium, "(?i)complete-guide") ~ "Complete Guide"
    )
  ) |>
  select(channel, everything()) |>
  rename(created_date = created_at) |>
  mutate(
    created_date = as_date(created_date),
    week_start = floor_date(created_date,
      "week",
      week_start = 1
    ), # Start of the week (Monday)
    week_end = ceiling_date(created_date,
      "week",
      week_start = 1
    ) - 1, # End of the week (Sunday)
    week = paste(
      format(week_start, "%m/%d"), "-",
      format(week_end, "%m/%d")
    ) # Format as MM/DD-MM/DD
  ) |>
  select(-week_start, -week_end) |>
  select(week, channel, everything())

summary_table <- combo_data2 |>
  group_by(channel) |>
  summarize(form_fill_numbers = n()) |>
  arrange(desc(form_fill_numbers))

sheet_write(combo_data2,
  ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE",
  sheet = "August"
)
