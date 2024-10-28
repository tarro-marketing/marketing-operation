library(tidyverse)

sam_list <- read_csv("data/February 2024 DM Mailing List SAM - Brizo - All.csv")
wonders_lead_queue <- read_csv("data/Wonders Lead Queue (SMS&DM) Filtered (3).csv")



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
         "Mobile - Primary","Zip/Postal Code","City", "Phone", "matching_id") |> 
  mutate(across(c("Phone",`Business Phone`, `Mobile - Primary`), 
                ~str_replace_all(., "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3")))

a.no_match <- a.no_match |> 
  select("Create Date","Company / Account","Street","State/Province (text only)",
         "Rating","Lead Owner","Lead Source","Lead Channel","Business Phone",
         "Mobile - Primary","Zip/Postal Code","City", "Phone", "matching_id") |> 
  mutate(across(c("Phone",`Business Phone`, `Mobile - Primary`), 
                ~str_replace_all(., "^(\\d{3})(\\d{3})(\\d{4})$", "(\\1) \\2-\\3")))

################save csv ###########################################

write_csv(a.no_match,"cleaned_data/no_match_wonders_lead_queue.csv", na = "")
write_csv(a.overlap_real, "cleaned_data/overlap_wonder_lead_queue.csv", na = "")

##################### b##############################################

b.overlap <- semi_join(sam_list, wonders_lead_queue_match, by = "Phone")
b.no_match <- anti_join(sam_list,wonders_lead_queue_match, by = "Phone")

b.overlap <- b.overlap |> 
  select("Snowball Map","Dedupe Key","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name",
         "Last Name","Title","Role","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range", "matching_id") |> 
  mutate(Phone = str_replace_all(
    Phone,"^(\\d{3})(\\d{3})(\\d{4})$","(\\1) \\2-\\3")) 

b.no_match <- b.no_match |> 
  select("Snowball Map","Dedupe Key","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name",
         "Last Name","Title","Role","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range", "matching_id") |> 
  mutate(Phone = str_replace_all(
    Phone,"^(\\d{3})(\\d{3})(\\d{4})$","(\\1) \\2-\\3")) 
################save csv ###########################################

b.overlap_snow_match <- as.list(b.overlap$`Snowball Map`)
b.overlap_snow_match <- unlist(b.overlap_snow_match)
b.no_match <- sam_list %>%
  filter(!`Snowball Map` %in% b.overlap_snow_match)
b.no_match <- b.no_match |> 
  select("Snowball Map","Dedupe Key","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name",
         "Last Name","Title","Role","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range", "matching_id") |> 
  mutate(Phone = str_replace_all(
    Phone,"^(\\d{3})(\\d{3})(\\d{4})$","(\\1) \\2-\\3")) 
write_csv(b.overlap, "cleaned_data/overlap_sam_list.csv", na = "")
write_csv(b.no_match,"cleaned_data/no_match_sam_list.csv", na = "")






sam_list_match_id <- sam_list |> 
  group_by(matching_id) %>%
  filter(row_number() == 1) %>%
  ungroup()


wonders_lead_queue_match_id <- wonders_lead_queue |> 
  group_by(matching_id) %>%
  filter(row_number() == 1) %>%
  ungroup()


c.overlap <- semi_join(wonders_lead_queue_match_id, sam_list_match_id, by = "matching_id")
c.no_match <- anti_join(wonders_lead_queue_match_id, sam_list_match_id, by = "matching_id")


d.overlap <- semi_join(sam_list_match_id, wonders_lead_queue_match_id, by = "matching_id")
d.no_match <- anti_join(sam_list_match_id,wonders_lead_queue_match_id, by = "matching_id")

