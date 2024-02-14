library(tidyverse)


List_Brizo_US_a <- read_csv("Raw-Data/List - Brizo - US a.csv")
Print_Shop_US_a <- read_csv("Raw-Data/list source/Print Shop - US (Chinese 56184-80086).csv")
Old_List_US_a <- read_csv("Raw-Data/list source/Old List - US (80423-96269).csv")


Old_List_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Old List")->Old_List_US_a

Print_Shop_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Printshop")->Print_Shop_US_a

List_Brizo_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Brizo")->List_Brizo_US_a


listcombine = rbind(List_Brizo_US_a, Print_Shop_US_a)

Drop_1 = rbind(listcombine, Old_List_US_a)


write_csv(Drop_1, "Raw-Data/drop1.csv")

Drop_2 <- read_csv("Raw-Data/Drop 2.csv")
Drop_2 |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone", "List") ->Drop_2

only_1_drop <-anti_join(Drop_1,Drop_2,by="Snowball Map")

### this do not remove the duped so it is more than on the google sheet
### 
### 


write_csv(only_1_drop,"Clean-Data/list_a_only_once.csv")
