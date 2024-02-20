library(tidyverse)
library(googlesheets4)


gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

################################## Brizo #######################################

brizo_us <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Brizo - US (1-47669)")
brizo_can <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Brizo - CAN (47670-54533)")

cols_brizo_us <- colnames(brizo_us)
cols_brizo_can <- colnames(brizo_can)
common_cols <- Reduce(intersect, list(cols_brizo_us, cols_brizo_can))

diff_cols_can_not_in_us <- setdiff(cols_brizo_can, cols_brizo_us)
diff_cols_us_not_in_can <- setdiff(cols_brizo_us, cols_brizo_can)

brizo_us <- brizo_us |>
  select(names(brizo_can)) |>
  mutate(`Zip/Postal Code` = as.character(`Zip/Postal Code`))

brizo_can <- brizo_can |>
  mutate(`Snowball Map` = as.character(`Snowball Map`))

brizo <- bind_rows(brizo_can, brizo_us)
rm(cols_brizo_us, cols_brizo_can, common_cols, brizo_can, brizo_us, diff_cols_can_not_in_us, diff_cols_us_not_in_can)



############################### printshop ######################################


printshop_us <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Print Shop - US (54534-88975)")
printshop_can <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Print Shop - CAN (88976-89501)")


printshop_us |> 
  rename("Snowball Map"=Snowball) -> printshop_us

cols_printshop_us <- colnames(printshop_us)
cols_printshop_can <- colnames(printshop_can)

common_cols <- Reduce(intersect, list(cols_printshop_can, cols_printshop_us))


diff_cols_can_not_in_us <- setdiff(cols_printshop_us, cols_printshop_can)
diff_cols_us_not_in_can <- setdiff(cols_printshop_can, cols_printshop_us)


printshop <- bind_rows(printshop_can, printshop_us)

rm(printshop_us,printshop_can, cols_printshop_can, cols_printshop_us, common_cols, diff_cols_can_not_in_us, diff_cols_us_not_in_can)


printshop |> 
rename(
  Phone2 = "...9", Phone3 = "...10",
  Phone4 = "...11", Phone5 = "...12",
  Phone6 = "...13", Phone7 = "...14",
  Phone8 = "...15", Phone9 = "...16",
  Phone1 = `Phone Number(s)`
) |> 
  mutate(Country = recode(Country,
                          CAN = "Canada",
                          US = "United States"
  ))-> printshop

################################ combine two list ##############################


cols_printshop <- colnames(printshop)
cols_brizo <- colnames(brizo)

common_cols <- Reduce(intersect, list(cols_printshop, cols_brizo))


diff_cols_printhsop_not_in_brizo <- setdiff(cols_printshop, cols_brizo)
diff_cols_brizo_not_in_printshop <- setdiff(cols_brizo,cols_printshop)

rm(cols_printshop,cols_brizo, common_cols, diff_cols_brizo_not_in_printshop, diff_cols_printhsop_not_in_brizo)

brizo <- brizo |> 
  rename("City" = "City/Town",
         "Zipcode" = "Zip/Postal Code",
         "Restaurant Name" = Name,
         "Address" = Street,
         "State" = "State/Province Code") |> 
  mutate("Phone2" = NA, "Phone5" = NA, "Phone8" = NA, 
         "Phone3" = NA, "Phone4" = NA, "Phone6" = NA,	
         "Phone7" = NA, "Phone9" = NA,	"Phone1" = NA)


printshop <-  printshop |> 
  mutate("ID" = NA,
         "Opening Hours" = NA,
         "First Name" = NA,
         "Last Name" = NA,	"Contact Email" = NA,
         "Contact Phone" = NA,	"Established" = NA,
         "Estimated Employees" = NA,	"Business Phone" = NA,
         "Contact Count" = NA,	"Most Common Email" = NA,
         "Cuisines (Regional)" = NA,	"Price Range" = NA,
         `Snowball Map` = as.character(`Snowball Map`))

sam_list <- bind_rows(brizo, printshop)

rm(brizo, printshop)


write_csv(sam_list, "recalculation-2024/sam-list/sam_list_november.csv")

