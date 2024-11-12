library(tidyverse)

all_form_fill <- read_csv("~/PerformanceMarketing/Form-Submission/2024/January_2024/January_all/all_form_fill.csv")


all_form_fill |> 
  filter(str_detect(channel,"DM")) -> dm_form_fill

sam_list <- read_csv("clean_data/sam_list.csv", 
                     col_types = cols(snowball_map = col_character(), 
                                      Phone = col_character())) 
sam_list <- sam_list |> 
  mutate(snowball_map= sprintf("%05d", as.numeric(snowball_map))) |> 
  rename(
         sam_list_phone_number= Phone) |> 
  select(snowball_map, sam_list_phone_number, list_source)



flight_ticket_form_fill <- dm_form_fill |> 
  filter(str_detect(Referrer,"jan")) |> 
  mutate(snowball_map = sapply(Referrer, function(url) {
    id_match <- str_match(url, "utm_id=(\\d+)")
    
    if (is.na(id_match[1])) {
      return(NA)  # ID not found
    }
    
    id <- id_match[2]
    
    # Ensure the ID is 5 digits with leading zeros
    id <- sprintf("%05d", as.numeric(id))
    
    return(id)
  })) |> 
  left_join(sam_list, by = "snowball_map") |> 
  rename(form_fill_phone_number=phone_number) |> 
  select(-filename) |> 
  select(snowball_map,channel, list_source, form_fill_phone_number, sam_list_phone_number, everything())
  

write_csv(flight_ticket_form_fill,"clean_data/flight_ticket_form_fill.csv", na = "")
