library(tidyverse)
library(googlesheets4)
library(salesforcer)
library(yaml)
library(httr)

config <- yaml::read_yaml("../../API/config.yml")

sf_auth(
  username = config$salesforce$username,
  password = config$salesforce$password,
  security_token = config$salesforce$security_token
)


Feb_Massager_Gift_List_SQL <- "00OUo000001Ho4fMAC"
Feb_Massager_Gift_List_SQL <- sf_run_report(Feb_Massager_Gift_List_SQL)



gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


Brizo_SQL_Gift <- read_sheet("1xMpiBGoLdWZz6r4J22RfAH8t22dpZ_PY_zN6b5dxgmQ", sheet = "Brizo - SQL Gift")
SMS_SQL_List <- read_sheet("1XdYrLC8yfP3avVwWtc4lr3vdkdUC35ib0bhAnxonqrc", sheet = "Group 1 (SQL Gift)")

Feb_Massager_Gift_List_SQL |> 
  mutate(`Business Phone` = str_remove_all(`Business Phone`,"[:punct:]|[:space:]"),
         `Mobile - Primary` = str_remove_all(`Mobile - Primary`,"[:punct:]|[:space:]")) -> feb_sql_salesforce


brizo_sql_gift_dm <- Brizo_SQL_Gift |> 
  mutate(Phone = str_remove_all(Phone,"[:punct:]|[:space:]"))

sms_sql_gift <- SMS_SQL_List |> 
  mutate(Phone = str_remove_all(Phone,"[:punct:]|[:space:]"))

rm(Feb_Massager_Gift_List_SQL, SMS_SQL_List, Brizo_SQL_Gift, config)

matching <- feb_sql_salesforce |>
  mutate(
    `Mobile # In Brizo` = if_else(`Mobile - Primary` %in% brizo_sql_gift_dm$Phone, TRUE, NA),
    `Business # In Brizo` = if_else(`Business Phone` %in% brizo_sql_gift_dm$Phone, TRUE, NA),
    `Mobile # In SMS` = if_else(`Mobile - Primary` %in% sms_sql_gift$Phone, TRUE, NA),
    `Business # In SMS` = if_else(`Business Phone` %in% sms_sql_gift$Phone, TRUE, NA)
  ) |>
  mutate(
    In_Brizo = coalesce(`Mobile # In Brizo`, `Business # In Brizo`),
    In_SMS = coalesce(`Mobile # In SMS`, `Business # In SMS`)
  ) |> 
  mutate(`Mobile - Primary` = as.character(`Mobile - Primary`), 
         `Business Phone` = as.character(`Business Phone`),
         
         `Mobile - Primary` = str_replace(`Mobile - Primary`, "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3"),
         `Business Phone`  = str_replace( `Business Phone` , "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3"))

write_csv(matching, "Feb-Massager-Gift-List-SQL/Feb Massager Gift List - SQL (matched).csv", na = "")



