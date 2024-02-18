library(tidyverse)


form_submission = read_csv("Raw-Data/dm_form_submission.csv")

form_submission <- 
  form_submission %>%
  mutate(language_test = ifelse(str_detect(URL, "\\b2023christmas-eng\\b"), "English Version",
                                ifelse(
                                  str_detect(URL, "\\b2023christmas6\\b|\\b2023christmas4\\b|\\b2023christmas2\\b|\\b2023christmas5\\b|\\b2023christmas3\\b|\\b2023christmas\\b"), 
                                  "Chinese Version", NA))
  )


form_submission <- 
  form_submission %>%
  mutate(drop_group = ifelse(str_detect(URL, "\\b2023christmas8\\b|\\b2023christmas9\\b|\\b2023christmas10\\b"), "Drop 2 - US",
                                ifelse(
                                  str_detect(URL, "\\b2023christmas5\\b|\\b2023christmas3\\b|\\b2023christmas\\b"), "Drop 1 - US", 
                                  ifelse(str_detect(URL,"\\b2023christmas6\\b|\\b2023christmas4\\b|\\b2023christmas2\\b"),"Drop 1 - Canada", NA))))


form_submission <- 
  form_submission |> 
  mutate(Phone = gsub("\\D", "", Phone),
         Created_Date = as_date(Created_Date)) 

write_csv(form_submission,"Clean-Data/clean_form_submission.csv")
