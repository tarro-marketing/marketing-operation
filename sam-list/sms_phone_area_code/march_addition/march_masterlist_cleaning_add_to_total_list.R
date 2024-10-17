library(tidyverse)



March_Master_list_addon_Sheet1 <- read_csv("sms_phone_area_code/march_addition/March Master list addon - Sheet1.csv")
phone_area_code_cities <- read_csv("sms_phone_area_code/clean_data/information/phone_area_code_cities.csv",
                                   col_types = cols(`Area Code` = col_character()))|> 
  distinct()


March_Master_list_addon_Sheet1 <- March_Master_list_addon_Sheet1 |> 
  mutate(phone = as.character(phone),
         `Area Code` = str_extract(phone,"^\\d{3}")) |> 
  select(phone,`Area Code`, everything())

clean_master_march <- March_Master_list_addon_Sheet1 |> 
  left_join(phone_area_code_cities, by = "Area Code") |> 
  select(phone, `Area Code`, State,`State Abbreviation`, 
         Country, everything())

march_master_addition <- clean_master_march |> 
  select(phone, `Area Code`,`State Abbreviation`, Country) |>
  rename(Phone = phone, 
         State = `State Abbreviation`) |> 
  mutate(Phone = as.character(Phone), 
       Phone = str_replace(Phone, "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3"),
       list = "Masterlist")

phone_total <- read_csv("sms_phone_area_code/clean_data/phone_total.csv", 
                        col_types = cols(`Area Code` = col_character()))
march_phone_total <- bind_rows(march_master_addition, phone_total)


write_csv(march_phone_total,"sms_phone_area_code/march_addition/march_sms_list.csv")
