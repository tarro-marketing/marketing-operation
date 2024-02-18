library(tidyverse)

## Brizo
brizo_us_zh <- read_csv("december-dm-2023/raw-data/list source/Brizo - US (Chinese 1-41968).csv")
brizo_us_eng <- read_csv("december-dm-2023/raw-data/list source/New folder/Brizo - US (English 49079-56183).csv")
brizo_can <- read_csv("december-dm-2023/raw-data/list source/New folder/Brizo - CAN (Chinese 41969-49078).csv")

cols_brizo_us_zh <- colnames(brizo_us_zh)
cols_brizo_us_eng <- colnames(brizo_us_eng)
cols_brizo_can <- colnames(brizo_can)
common_cols <- Reduce(intersect, list(cols_brizo_us_zh, cols_brizo_can))

rm(cols_brizo_us_zh,cols_brizo_us_eng,cols_brizo_can,common_cols)

brizo_us_eng <- brizo_us_eng |> 
  rename(Phone=`Business Phone`) |> 
  select("Snowball Map","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name","Last Name","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range") |> 
  mutate(Languge = "English",
         Format = "Tridfold")

brizo_can <- brizo_can |> 
  select("Snowball Map","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name","Last Name","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range") |> 
  mutate(Languge = NA,
         Format = "3D Card")

brizo_us_zh <- brizo_us_zh |> 
  select("Snowball Map","ID","Name","Street","City/Town","State/Province Code","Zip/Postal Code","Country","First Name","Last Name","Contact Email","Contact Phone","Established","Phone","Most Common Email","Cuisines (Regional)","Price Range") |> 
  mutate(Languge = "Chinese",
         Format = "3D Card")


brizo <- rbind(brizo_can,brizo_us_eng,brizo_us_zh)
rm(brizo_can,brizo_us_eng,brizo_us_zh)

brizo <- brizo |> 
  mutate(Drop = "Drop 1")

## Print Shop
printshop_us <- read_csv("december-dm-2023/raw-data/list source/Print Shop - US (Chinese 56184-80086).csv")
printshop_can <- read_csv("december-dm-2023/raw-data/list source/New folder/Print Shop - CAN (80087-80422).csv")

cols_printshop_us <- colnames(printshop_us)
cols_printshop_can <- colnames(printshop_can)

common_cols <- Reduce(intersect, list(cols_printshop_can, cols_printshop_us))




oldlist_can <- read_csv("december-dm-2023/raw-data/list source/New folder/Old List - CAN (96270-101360).csv")
oldlist_us <- read_csv("december-dm-2023/raw-data/list source/Old List - US (80423-96269).csv")

##
cols_oldlist_can <- colnames(oldlist_can)
cols_oldlist_us <- colnames(oldlist_us)

# Find common column names
common_cols <- Reduce(intersect, list(cols_brizo_us_zh, cols_brizo_us_eng, cols_brizo_can, cols_printshop_us, cols_printshop_can, cols_oldlist_can, cols_oldlist_us))

# Print the common column names
print(common_cols)



# remember to group drop
Old_List_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Old List")->Old_List_US_a

Print_Shop_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Printshop")->Print_Shop_US_a

List_Brizo_US_a |> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"Zipcode",	"State",	"Phone") |> 
  mutate(List = "Brizo")->List_Brizo_US_a