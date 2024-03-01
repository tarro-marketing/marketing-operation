library(tidyverse)

brizo <- read_csv("sms_phone_area_code/clean_data/clean_individual_list/brizo_list_sms.csv", 
                  col_types = cols(`Area Code` = col_character()))

printshop <- read_csv("sms_phone_area_code/clean_data/clean_individual_list/Print Shop.csv", 
                      col_types = cols(`Area Code` = col_character()))

inboudcall <- read_csv("sms_phone_area_code/clean_data/clean_individual_list/inboud_call_follow_up_new.csv")
wondersleadqueue_new <- read_csv("sms_phone_area_code/clean_data/clean_individual_list/wondersleadqueue_new.csv")

masterlist <- read_csv("sms_phone_area_code/clean_data/clean_individual_list/masterlist.csv")



################combine three list together #####################

inboud_call_match <- inboudcall |> 
  select(phone, `Area Code`, `State Abbreviation`,Country) |> 
  rename(Phone = phone,State = `State Abbreviation`) |> 
  mutate(list = "Inbound Call")

brizo_match <- brizo |> 
  select(-State) |> 
  rename(State = state_group) |> 
  select(Phone.x, `Area Code`, State,Country) |> 
  rename(Phone = Phone.x) |> 
  mutate(list = "Brizo")

printshop_match <- printshop |> 
  select(phone, `Area Code`,`State Abbreviation`,Country) |> 
  rename(State = `State Abbreviation`,
         Phone = phone) |> 
  mutate(list = "Print Shop")


masterlist_match <- masterlist |> 
  select(Phone, `Area Code`,State,Country) |> 
  mutate(list = "Masterlist")

wondersleadqueue_match <- wondersleadqueue_new |> 
  select(Phone, `Area Code`,State,Country) |> 
  mutate(list = "Wonders Lead Queue")

phone_total <- rbind(brizo_match, printshop_match, masterlist_match, wondersleadqueue_match,inboud_call_match)
phone_total$Phone <- as.character(phone_total$Phone)


phone_total <- phone_total %>% 
  mutate(Phone = str_remove_all(Phone, "[:punct:]|[:space:]"))

phone_total <- phone_total %>% 
  filter(Country == "United States") %>%
  mutate(State = toupper(State)) %>%
  distinct(Phone, .keep_all = TRUE)


phone_total <- phone_total %>%
  mutate(
    # Remove non-digit characters
    Phone = str_remove_all(Phone, "\\D"),
    # Check the length and handle accordingly
    Phone = ifelse(nchar(Phone) == 10, Phone, NA)
  )

phone_total <- phone_total %>% 
  mutate(Phone = as.character(Phone), 
         Phone = str_replace(Phone, "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3"))



write_csv(phone_total,"clean_data/phone_total.csv")




