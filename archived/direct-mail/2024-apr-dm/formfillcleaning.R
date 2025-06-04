
library(tidyverse)
library(purrr)
library(dplyr)
library(googlesheets4)

gs4_auth_configure(path = "~/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")
gs4_auth(email = "youjia.chen@wondersco.com", cache = TRUE)


folder_path <- '~/Performance-Marketing/direct-mail-2024/apr-dm-2024/data/form fill' 
filenames <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

combo_data <- purrr::map_df(filenames, 
                            ~read_csv(.x, col_types = cols(.default = col_character())) |> 
                              mutate(filename = .x))

common_path <- dirname(filenames[1])

all_form_fill <- combo_data |>
  mutate(filename = as.character(filename)) |>
  mutate(filename = str_replace(filename, common_path, ""),
         filename = str_remove(filename, "/")) |> 
  mutate(filename = str_replace_all(filename, "\\([0-9a-f]+\\)-\\d{4}-\\d{2}-\\d{2}\\.csv", "")) |>
  mutate("Last Name" = coalesce(姓氏,"Last Name")) |> 
  rename(phone_number = `手机号码`) |>
  mutate(phone_number = str_replace_all(phone_number, "-", "") |> 
           str_replace_all("[^0-9]", "")) |> 
  mutate(
    channel = case_when(
      str_detect(filename, "(?i)SEM|Blog Pages Contact us|google") ~ "Google",
      str_detect(Referrer, "(?i)SEM|google") ~ "Google",
      str_detect(filename, "(?i)SEO|Contact us") ~ "SEO",
      str_detect(Referrer, "(?i)sMS") ~ "SMS",
      str_detect(Referrer, "(?i)Brizo|(?i)DM") ~ "DM",
      str_detect(`Form Name (ID)`, "(?i)Brizo|(?i)DM") ~ "DM",
      str_detect(Referrer,"(?i)directmail")~"DM",
      str_detect(filename,"(?i)WeChat")~"WeChat",
      str_detect(filename,"(?i)Newsletter")~"Newsletter",
      str_detect(filename,"(?i)WeChat")~"WeChat",
      TRUE ~ "No Match")) |> 
  select(channel, everything(),-"...1",-"...10",-"...12") |> 
  select(-`User Agent`)

rm(list = setdiff(ls(), c("all_form_fill", "combo_data")))

# write_csv(combo_data,"apr-dm-2024/data/all_form_fill.csv", na = "")
# 
# 
# 
# 
# write_sheet(combo_data, ss = "https://docs.google.com/spreadsheets/d/1FDVTt96aIWLeAxHx9V6mmJe6rZMp-d0OvByPeaSvT98/edit#gid=0", sheet = "Form Fill")