library(tidyverse)
library(googlesheets4)


gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

################################## Brizo #######################################

brizo_us_zh <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - US (Chinese 1-41968)")
brizo_us_eng <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - US (English 49079-56183)")
brizo_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Brizo - CAN (Chinese 41969-49078)")
cols_brizo_us_zh <- colnames(brizo_us_zh)
cols_brizo_us_eng <- colnames(brizo_us_eng)
cols_brizo_can <- colnames(brizo_can)
common_cols <- Reduce(intersect, list(cols_brizo_us_zh, cols_brizo_can))

rm(cols_brizo_us_zh, cols_brizo_us_eng, cols_brizo_can, common_cols)

brizo_us_eng <- brizo_us_eng |>
  rename(Phone = `Business Phone`) |>
  select("Snowball Map", "ID", "Name", "Street", "City/Town", "State/Province Code", "Zip/Postal Code", "Country", "First Name", "Last Name", "Contact Email", "Contact Phone", "Established", "Phone", "Most Common Email", "Cuisines (Regional)", "Price Range") |>
  mutate(
    Languge = "English",
    Format = "Tridfold"
  )

brizo_can <- brizo_can |>
  select("Snowball Map", "ID", "Name", "Street", "City/Town", "State/Province Code", "Zip/Postal Code", "Country", "First Name", "Last Name", "Contact Email", "Contact Phone", "Established", "Phone", "Most Common Email", "Cuisines (Regional)", "Price Range") |>
  mutate(
    Languge = NA,
    Format = "3D Card"
  )

brizo_us_zh <- brizo_us_zh |>
  select("Snowball Map", "ID", "Name", "Street", "City/Town", "State/Province Code", "Zip/Postal Code", "Country", "First Name", "Last Name", "Contact Email", "Contact Phone", "Established", "Phone", "Most Common Email", "Cuisines (Regional)", "Price Range") |>
  mutate(
    Languge = "Chinese",
    Format = "3D Card"
  )


brizo <- rbind(brizo_can, brizo_us_eng, brizo_us_zh)
rm(brizo_can, brizo_us_eng, brizo_us_zh)

brizo <- brizo |>
  mutate(Drop = "Drop 1")

################################ Print Shop ####################################

printshop_us <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Print Shop - US (Chinese 56184-80086)")
printshop_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Print Shop - CAN (80087-80422)")



cols_printshop_us <- colnames(printshop_us)
cols_printshop_can <- colnames(printshop_can)

common_cols <- Reduce(intersect, list(cols_printshop_can, cols_printshop_us))


printshop_can |> 
  rename(Phone2 ="...12",Phone3 ="...13",
         Phone4 ="...14",Phone5 ="...15",
         Phone6 ="...16",Phone7 ="...17",
         Phone8 ="...18",Phone9 ="...19",
         Phone1 = `Phone Number(s)`)|> 
  select(-c(Phone2,Phone3,Phone4,Phone5,Phone6,Phone7,Phone8,Phone9)) -> printshop_can

printshop_us |> 
  rename(Phone1 = `Phone Number(s)`) |> 
  mutate(Zipcode = as.character(Zipcode)) -> printshop_us


common_cols <- Reduce(intersect, list(printshop_can, printshop_us))

rm(cols_printshop_can, cols_printshop_us, common_cols)

printshop <-  rbind(printshop_can, printshop_us)

rm(printshop_can, printshop_us)

printshop <- printshop |> 
  mutate(Country = recode(Country, CAN = "Canada",
                           US = "United States"))

############################## old list ########################################

oldlist_can <- read_csv("december-dm-2023/raw-data/list source/New folder/Old List - CAN (96270-101360).csv")
oldlist_us <- read_csv("december-dm-2023/raw-data/list source/Old List - US (80423-96269).csv")
oldlist_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - CAN (96270-101360)")
oldlist_us <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - US (80423-96269)")

##
cols_oldlist_can <- colnames(oldlist_can)
cols_oldlist_us <- colnames(oldlist_us)

# Find common column names
common_cols <- Reduce(intersect, list(cols_brizo_us_zh, cols_brizo_us_eng, cols_brizo_can, cols_printshop_us, cols_printshop_can, cols_oldlist_can, cols_oldlist_us))

# Print the common column names
print(common_cols)







# remember to group drop
Old_List_US_a |>
  select("Snowball Map", "Restaurant Name", "Address", "City", "Zipcode", "State", "Phone") |>
  mutate(List = "Old List") -> Old_List_US_a

Print_Shop_US_a |>
  select("Snowball Map", "Restaurant Name", "Address", "City", "Zipcode", "State", "Phone") |>
  mutate(List = "Printshop") -> Print_Shop_US_a

List_Brizo_US_a |>
  select("Snowball Map", "Restaurant Name", "Address", "City", "Zipcode", "State", "Phone") |>
  mutate(List = "Brizo") -> List_Brizo_US_a



