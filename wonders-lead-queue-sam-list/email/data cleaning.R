library(tidyverse)

sam_list <- read_csv("data/February 2024 DM Mailing List SAM - Brizo - All.csv")
wonders_lead_queue <- read_csv("email/email.csv")



sam_list <- sam_list |> 
  mutate(Phone = str_remove_all(Phone, "-"),
         Phone = str_remove_all(Phone, "[^0-9]"),
         Street = tolower(Street),
         matching_id =  paste0(substr(Street, 1, 15),`Zip/Postal Code`),
         matching_id = str_remove_all(matching_id,"[:punct:]"),
         matching_id = str_remove_all(matching_id,"[:space:]"),
         matching_id = str_remove_all(matching_id,"[:blank:]"))
sam_list_match <- sam_list|> 
  group_by(Phone) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(matching_id, Phone, Street, everything())


wonders_lead_queue <- wonders_lead_queue |> 
  mutate(`Business Phone` = str_remove_all(`Business Phone`, "-"),
         `Business Phone` = str_remove_all(`Business Phone`, "[^0-9]"),
         `Mobile - Primary` = str_remove_all(`Mobile - Primary`, "-"),
         `Mobile - Primary` = str_remove_all(`Mobile - Primary`, "[^0-9]"),
         Street = tolower(Street),
         matching_id =  paste0(substr(Street, 1, 15),`Zip/Postal Code`),
         matching_id = str_remove_all(matching_id,"[:punct:]"),
         matching_id = str_remove_all(matching_id,"[:space:]"),
         matching_id = str_remove_all(matching_id,"[:blank:]"),
         Phone = case_when(
           is.na(`Business Phone`) ~ `Mobile - Primary`,
           TRUE ~ `Business Phone`
         )) |> 
  mutate(row_id = row_number())

wonders_lead_queue_match <- wonders_lead_queue |> 
  group_by(Phone) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(matching_id, Phone, Street, everything())



a.overlap <- semi_join(wonders_lead_queue, sam_list_match, by = "Phone")

a.overlap_row_id_match <- as.list(a.overlap$row_id)
a.overlap_row_id_match <- unlist(a.overlap_row_id_match)
a.no_match <- wonders_lead_queue %>%
  filter(!row_id %in% a.overlap_row_id_match)

a.overlap_real <- wonders_lead_queue %>%
  filter(row_id %in% a.overlap_row_id_match)

a.overlap_real <- a.overlap_real |> 
  select("Create Date","Company / Account","Street","State/Province (text only)",
         "Rating","Lead Owner","Lead Source","Lead Channel","Business Phone",
         "Mobile - Primary","Zip/Postal Code","Email", "Phone", "matching_id") |> 
  mutate(across(c("Phone",`Business Phone`, `Mobile - Primary`), 
                ~str_replace_all(., "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3")))

a.no_match <- a.no_match |> 
  select("Create Date","Company / Account","Street","State/Province (text only)",
         "Rating","Lead Owner","Lead Source","Lead Channel","Business Phone",
         "Mobile - Primary","Zip/Postal Code","Email", "Phone", "matching_id") |> 
  mutate(across(c("Phone",`Business Phone`, `Mobile - Primary`), 
                ~str_replace_all(., "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3")))


a.overlap_real |> 
drop_na(Email) -> count_email

a.no_match |> 
  drop_na(Email) -> count_email2
################save csv ###########################################

write_csv(count_email2,"email/no_match_wonders_lead_queue.csv", na = "")
write_csv(count_email, "email/overlap_wonder_lead_queue.csv", na = "")