library(tidyverse)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

data <- read_sheet(ss = "1wiYSntRdY7lKOfkkqNheWdYrJeoMebQYzfCoZwn7VO0", sheet = "Sheet5")
glimpse(data)
channel <- read_sheet(ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64", sheet = "Lead Channel Category")


# Split the channels by `;` and calculate credit
data_split <- data %>%
  mutate(`Lead Channel List` = str_split(`Lead Channel`, ";\\s*")) %>%
  unnest(`Lead Channel List`) %>%
  group_by(`Lead Channel`, Group, `Total Leads`) %>%
  mutate(Channel_Count = n_distinct(`Lead Channel List`)) %>%
  ungroup() %>%
  mutate(`Credit` = `Total Leads` / Channel_Count) %>%
  select(`Lead Create Month`,`Lead Channel List`, Group, `Credit`) |>
  left_join(channel, by = join_by(`Lead Channel List` == lead_source)) |>
  select(-`Lead Channel List`) |>
  rename(`Lead Channel` = lead_channel)

# Summarize the data by channel and group
summary_data <- data_split %>%
  group_by( `Lead Create Month`,`Lead Channel`, Group) %>%
  summarize(`Total Leads` = sum(`Credit`), .groups = 'drop')


# View the result
print(summary_data)



data_final <- summary_data |>
  pivot_wider(names_from = Group, values_from = `Total Leads`) |>
  replace_na(list(`Rejected Lead` = 0, `Unqualified Lead` = 0))

data_percent <- data_final %>%
  mutate(Total = rowSums(select(., Other,`Rejected Lead`, `Unqualified Lead`), na.rm = TRUE)) %>%
  mutate(across(c(`Rejected Lead`, `Unqualified Lead`), ~ round(.x / Total * 100, 1), .names = "Percent_{col}")) |>
  mutate(across(starts_with("Percent_"), ~ paste0(.x, "%"))) |>
  mutate(across(everything(), ~ replace(.x, is.nan(.x), NA)))









# Verify the total credits
total_credit <- summary_data %>%
  summarize(Total_Credit = sum(`Total Leads`))


rejected <- data_percent |>
  select(`Lead Create Month`, `Lead Channel`, `Percent_Rejected Lead`) |>
  pivot_wider(names_from = `Lead Channel`, values_from = `Percent_Rejected Lead`)

total <- data_percent %>%
  select(`Lead Create Month`, `Lead Channel`, Total) %>%
  pivot_wider(names_from = `Lead Channel`, values_from = Total) %>%
  mutate(Total = rowSums(select(., -`Lead Create Month`), na.rm = TRUE)) |>
  select(`Lead Create Month`, Total)


unqualified <- data_percent |>
  select(`Lead Create Month`, `Lead Channel`, `Percent_Unqualified Lead`) |>
  pivot_wider(names_from = `Lead Channel`, values_from = `Percent_Unqualified Lead`)

print(total_credit)
