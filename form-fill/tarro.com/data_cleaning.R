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

###########################################################################
# update line 28
# update line 144

###########################################################################


setwd("~/marketing-operation")
folder_path <- here("form-fill", "tarro.com", "2024-11-11")
filenames <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)


col_types <- cols(
  `utm_campaign` = col_character(),
  `Phone Number 4` = col_character(),
  `Phone Number` = col_character()
)

# Read and combine the files with consistent column types
combo_data <- purrr::map_df(
  filenames,
  ~ read_csv(.x, col_types = col_types) |>
    mutate(filename = .x)
)


file_names <- as.data.frame(colnames(combo_data)) |>
  rename(column_names = `colnames(combo_data)`)

# Define the list of columns to combine for each field
fields <- list(
  phone_number_combined = c("Phone Number", "Phone Number 2", "Phone Number 4", 
                            "Mobile Number", "New Client’s Mobile Number", 
                            "Prospect’s Mobile Number"),
  email_combined = c("Email", "Email 6", "Email 8", "Email 10", "Email Address", "邮箱地址"),
  first_name_combined = c("First Name", "First Name 2", "First Name 4", 
                          "First Name 5", "First Name 8", "名"),
  last_name_combined = c("Last Name", "Last Name 2", "Last Name 5", 
                         "Last Name 7", "Last Name 8", "姓"),
  cuisine_type_combined = c("Cuisine Type", "Cuisine Type 2", "Cuisine Type 4", 
                            "Cuisine Type 6", "cuisine-type", "请选餐馆菜系"),
  restaurant_name_combined = c("Restaurant Name", "Restaurant Name 2", 
                               "Restaurant Name 3", "Restaurant Name 8", "餐馆名字")
)

# Loop through each field and use coalesce on available columns
for (field in names(fields)) {
  combo_data <- combo_data |>
    mutate(!!field := coalesce(!!!select(combo_data, any_of(fields[[field]]))))
}

combo_data__c  <- combo_data


# Select the coalesced columns and any additional necessary columns
final_data <- combo_data__c |>
  select(
    Date, filename, utm_campaign, phone_number_combined, email_combined,
    first_name_combined, last_name_combined, cuisine_type_combined,
    restaurant_name_combined
  )

# keywords that need to be filter out
email_keywords <- c("youjia", "tarro", "wondersco", "test", "ww@www", "ss@22")
fname_keywords <- c("test", "marketing", "fffadds", "dfsadf")
lname_keywords <- c("test", "marketing")
rname_keywords <- c("marketing", "test", "tarro")
# reformat the keywords
email_pattern <- paste(email_keywords, collapse = "|")
fname_pattern <- paste(fname_keywords, collapse = "|")
lname_pattern <- paste(lname_keywords, collapse = "|")
rname_pattern <- paste(rname_keywords, collapse = "|")


final_data_clean <- final_data |>
  mutate(
    phone_number_combined =
      str_replace_all(phone_number_combined, "[^\\w-]+", "")
  ) |>
  mutate(
    form_path = str_replace(
                          filename,
                          "^/Users/yukachen/marketing-operation/form-fill/tarro\\.com/\\d{4}-\\d{2}-\\d{2}/",
                          ""),
    form_path = str_replace(form_path, "-\\d{4}-\\d{2}-\\d{2}\\.csv$", ""),
    Date = mdy_hms(Date)
  ) |>
  select(form_path, everything()) |>
  select(-filename) |>
  filter(
    !str_detect(email_combined, regex(email_pattern, ignore_case = TRUE)),
    !str_detect(first_name_combined, regex(fname_pattern, ignore_case = TRUE)),
    !str_detect(last_name_combined, regex(lname_pattern, ignore_case = TRUE)),
    !str_detect(restaurant_name_combined,
                regex(rname_pattern, ignore_case = TRUE))
  ) |>
  distinct(phone_number_combined, email_combined,
           first_name_combined, last_name_combined, .keep_all = TRUE) |>
  mutate(
    week_start = floor_date(Date, "week", week_start = 1),
    week_end = ceiling_date(Date, "week", week_start = 1) - 1,
    week = paste0(format(week_start, "%m/%d"), "-", format(week_end, "%m/%d")),
    channel = str_remove(form_path, "^[ab]-") |>   # Remove 'a-' or 'b-' at the start
      str_replace("-form$", " form") |>            # Replace '-form' at the end with ' form'
      str_replace_all("_", " ")                    # Replace any underscores with spaces if necessary
  ) |>
  select(-week_start, -week_end) |>
  select(week, channel, form_path, phone_number_combined,
         email_combined, first_name_combined, last_name_combined, everything())



rm(list = setdiff(ls(), "final_data_clean"))




################################

summary_table <- final_data_clean |>
  group_by(week, channel) |>
  summarize(form_fill_numbers = sum(n()), .groups = "drop") |>
  arrange(desc(week))

# summary_table <- summary_table |>
#   group_by(week, channel) |>
#   summarize(value = sum(form_fill_numbers), .groups = "drop")



sheet_write(final_data_clean,
            ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE",
            sheet = "11/11")

## refresh/overwrite from A170 because everytime is exracting all forms
range_write(
  final_data_clean,
  ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE",
  sheet = "All Formfills",
  range = "A170",
  col_names = TRUE,
  reformat = FALSE
)

## refresh/overwrite from A5 because everytime is exracting all forms
range_write(
  summary_table,
  ss = "18y8li4QmbStR-6XFc-Vx8o9qzTDJrGIExAObVzG8RpE",
  sheet = "Summary",
  range = "A5",
  col_names = TRUE,
  reformat = FALSE
)
