library(tidyverse)




brizo <- read_csv("Raw-Data/list source/Brizo - US (Chinese 1-41968).csv", 
                  col_types = cols(`Snowball Map` = col_character(), 
                                   Phone = col_character()))
brizo_eng <- read_csv("Raw-Data/list source/New folder/Brizo - US (English 49079-56183).csv")
brizo_can <- read_csv("Raw-Data/list source/New folder/Brizo - CAN (Chinese 41969-49078).csv") |> 
  select(-`Business Type`)


colname_brizo = colnames(Brizo_CAN)
brizo_eng |> 
  rename(Phone = `Business Phone`) |> 
  select(all_of(colname_brizo)) ->brizo_eng

brizo_combined = rbind(brizo_eng, brizo_can)
brizo_combined= brizo_combined |> 
  select(-c(Established, `Contact Email`,`Most Common Email`,Name,`Contact Phone`)) |> 
  rename(City = `City/Town`,
         State=`State/Province Code`, 
         Zipcode=`Zip/Postal Code`,
         Cuisines = `Cuisines (Regional)`)
colname_brizo = colnames(brizo_combined)


brizo <- brizo |> 
  rename(Cuisines = `Cuisines(Regional)`,
         `Price Range`=PriceRange) |> 
  select(-ContactEmail) |> 
  select(all_of(colname_brizo))



brizo_all = rbind(brizo,brizo_combined) 

write_csv(brizo_all, "Clean-Data/brizo_list.csv")