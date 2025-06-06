library(tidyverse)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


A_Postcard_51744_70910_ <- read_csv("marketing-nurture-queue/A - Postcard (51744-70910).csv")
B_Bifold_70911_91230_ <- read_csv("marketing-nurture-queue/B - Bifold (70911-91230).csv")

mailable <- bind_rows(A_Postcard_51744_70910_, B_Bifold_70911_91230_)

rm("A_Postcard_51744_70910_", "B_Bifold_70911_91230_")

mnq <- read_csv("marketing-nurture-queue/report1717697433761.csv")


mnq <- mnq |>
  drop_na(Street) |>
  mutate(
    Street2 = str_remove_all(Street, "\\s"),
    `Company2` = str_remove_all(`Company / Account`, "\\s")
  )

mailable <- mailable |>
  mutate(
    Street2 = str_remove_all(Street, "\\s"),
    `Company2` = str_remove_all(`Company / Account`, "\\s")
  ) |>
  drop_na(Street)

match <- mnq |>
  semi_join(mailable, by = "Street2")

matchmatch <- match |>
  semi_join(mailable, by = "Company2") |>
  select(-c(Street2, Company2)) |>
  mutate(`State Group` = case_when(
    `State/Province (text only)` %in% c("CA", "NJ", "PA", "GA", "VA", "LA", "OH", "MI", "CO", "SC", 
                 "IN", "NV", "MN", "WI", "OK", "KY", "MS", "KS", "NM", "NH", 
                 "NE", "ID", "ME", "DE", "WY") ~ "Group A",
    `State/Province (text only)` %in% c("NY", "TX", "FL", "IL", "WA", "NC", "MA", "MD", "OR", "AZ", 
                 "TN", "MO", "CT", "AL", "UT", "AR", "IA", "RI", "DC", "WV", 
                 "MT", "VT", "SD", "ND") ~ "Group B",
    TRUE ~ "Unknown"
  ))

# nomatch <- matchmatch |>
#   anti_join(mailable, by = "Company2")
#
# no <- mailable |>
#   anti_join(match, by = "Street2")
#
# matchmatch <- mailable |>
#   semi_join(match, by = "Street2")

rm(mailable, match, mnq, client_secret_path, email)

final_mnq <- matchmatch |> 
  mutate(`Queue Enter Date` = coalesce(`Rejected Time`,`Dead Date`)) |> 
  select(`State Group`, `Queue Enter Date`, `Rejected Time`, `Dead Date`, `Create Date`, everything())

write_csv(final_mnq, "marketing-nurture-queue/mkt-nurture-que.csv", na = "")
