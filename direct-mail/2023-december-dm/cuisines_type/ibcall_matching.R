library(tidyverse)
clean_inbound_call <- read_csv("Documents/PerformanceMarketing/DirectMail/2023/December-DM/Clean-Data/clean_inbound_call.csv",
                               col_types = cols(Phone = col_character()))
brizo_list <- read_csv("Documents/PerformanceMarketing/DirectMail/2023/December-DM/Clean-Data/brizo_list.csv")



brizo_list |> 
  select(Phone, `Price Range`,State, Cuisines) -> price_range_matching


clean_inbound_call |> 
  select(Date, Phone, State, `Cuisine Type`, `SF Link`) |> 
  left_join(price_range_matching, by = "Phone") -> ib_c_matching


write_csv(ib_c_matching, "~/PerformanceMarketing/DirectMail/2023/December-DM/cuisines_type/ib_call_matching.csv",na="")


