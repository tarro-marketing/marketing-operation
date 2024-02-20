library(tidyverse)

brizo_canada <- read_csv("october-dm-2023/data/October-Target-SAM-List-CAN-All.csv")
brizo_us <- read_csv("october-dm-2023/data/October-Target-SAM-List-US-All.csv")

brizo_us<- brizo_us |> 
  mutate(`Zip/Postal Code`= as.character(`Zip/Postal Code`))

sam_list_oct <- bind_rows(brizo_canada, brizo_us)



rm(brizo_canada, brizo_us)


sam_list_oct <- sam_list_oct |> 
  select(-c("Address Check", "Name + City Check", "Contact Count", "Website Technology Vendors")) |> 
  drop_na("Snowball Map"  )

write_csv(sam_list_oct, "recalculation-2024/sam-list/sam_list_october.csv")
