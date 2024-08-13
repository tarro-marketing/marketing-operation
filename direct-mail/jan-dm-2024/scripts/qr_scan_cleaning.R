library(tidyverse)

google_qr_scan <- read_csv("raw_data/qr_scan_list_mapping_doc/google_qr_scan.csv")
sam_list <- read_csv("clean_data/sam_list.csv", col_types = cols(snowball_map = col_character())) |> 
  mutate(snowball_map = sprintf("%05d", as.numeric(snowball_map)))

google_qr_scan <- google_qr_scan |> 
  mutate(snowball_map = sapply(`Page location`, function(url) {
    id_match <- str_match(url, "utm_id=(\\d+)")
    
    if (is.na(id_match[1])) {
      return(NA)  # ID not found
    }
    
    id <- id_match[2]
    
    # Ensure the ID is 5 digits with leading zeros
    id <- sprintf("%05d", as.numeric(id))
    
    return(id)
  }),
  Date = ymd(Date)) |> 
  select(snowball_map, Date,`Page location`, Sessions) |> 
  left_join(sam_list, by = "snowball_map") |> 
  distinct(snowball_map, .keep_all = TRUE)

write_csv(google_qr_scan, "clean_data/qr_scan.csv", na = "")


