library(tidyverse)

sms_list <- read_csv("clean-data/sms-list.csv", 
                     col_types = cols(Phone = col_character(), 
                                      `Area Code` = col_character()))

marketing_nurture_queue <- read_csv("clean-data/marketing_nurture_queue.csv", 
                                    col_types = cols(Phone = col_character()))

sms_list <- sms_list |> 
  mutate(Phone = map_chr(str_extract_all(Phone, "\\d"), ~ paste(unlist(.x), collapse = "")))

marketing_nurture_queue |> 
  mutate(Phone=as.character(Phone))-> marketing_nurture_queue

semi_join(sms_list, marketing_nurture_queue,by = "Phone")-> x

semi_join(marketing_nurture_queue, sms_list,by = "Phone")-> y


write_csv(x,"clean-data/overlap_sms_list.csv",na = "")
write_csv(y,"clean-data/overlap_marketing_nurture_queue.csv",na = "")


anti_join(sms_list, marketing_nurture_queue,by = "Phone")-> z

anti_join(marketing_nurture_queue, sms_list,by = "Phone")-> a


write_csv(z,"clean-data/unique_sms_list.csv",na = "")
write_csv(a,"clean-data/unique_marketing_nurture_queue.csv",na = "")