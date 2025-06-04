library(tidyverse)

dm_scan_dec <- read_csv("Raw-Data/raw_scan_december.csv")


dm_scan_dec <- 
  dm_scan_dec %>%
  mutate(language_test = ifelse(str_detect(`Page location`, "\\b2023christmas-eng\\b"), "English Version",
                                ifelse(
                                  str_detect(`Page location`, "\\b2023christmas6\\b|\\b2023christmas4\\b|\\b2023christmas2\\b|\\b2023christmas5\\b|\\b2023christmas3\\b|\\b2023christmas\\b"), 
                                  "Chinese Version", NA))
  )

dm_scan_dec <- dm_scan_dec %>%
  mutate(drop_group = 
           ifelse(str_detect(`Page location`, "\\b2023christmas-eng\\b"), NA,
                  ifelse(str_detect(`Page location`, "\\b2023christmas8\\b|\\b2023christmas9\\b|\\b2023christmas10\\b"), "Drop 2 - US",
                         ifelse(str_detect(`Page location`, "\\b2023christmas5\\b|\\b2023christmas3\\b|\\b2023christmas\\b"), "Drop 1 - US", 
                                ifelse(str_detect(`Page location`, "\\b2023christmas6\\b|\\b2023christmas4\\b|\\b2023christmas2\\b"), "Drop 1 - Canada", 
                                       "Value_if_false")))))



dm_scan_dec <- 
  dm_scan_dec |> 
  mutate(Date = ymd(Date))


dm_scan_dec <- dm_scan_dec %>%
  mutate(snowball_id = sapply(`Page location`, function(url) {
    id_match <- str_match(url, "utm_id=(\\d+)")
    
    if (is.na(id_match[1])) {
      return(NA)  # ID not found
    }
    
    id <- id_match[2]
    
    # Ensure the ID is 5 digits with leading zeros
    id <- sprintf("%05d", as.numeric(id))
    
    return(id)
  })) |> 
  drop_na(snowball_id)



dm_scan_dec <- 
  dm_scan_dec |> 
  mutate(
    list_source = case_when(
      str_detect(`Page location`, "printshop") ~ "PrintShop",
      str_detect(`Page location`, "brizo") ~ "Brizo",
      str_detect(`Page location`, "oldlist") ~ "OldList",
      TRUE ~ NA_character_
    )
  )


dm_scan_dec <- 
  dm_scan_dec |> 
  mutate(country = case_when(
    str_detect(drop_group, "US") ~ "United States",
    str_detect(drop_group, "Canada") ~ "Canada"
  ))

dm_scan_dec <- 
  dm_scan_dec %>%
  mutate(format = ifelse(str_detect(`Page location`, "\\b2023christmas-eng\\b"), "Trifold",
                                ifelse(
                                  str_detect(`Page location`, "\\b2023christmas6\\b|\\b2023christmas4\\b|\\b2023christmas2\\b|\\b2023christmas5\\b|\\b2023christmas3\\b|\\b2023christmas\\b"), 
                                  "3D card", NA))
  )

write_csv(dm_scan_dec, "Clean-Data/all_scan_dec.csv")


unique_id_scan <- 
dm_scan_dec |> 
  distinct(snowball_id, .keep_all = TRUE)


write_csv(unique_id_scan, "Clean-Data/unique_scan.csv")
