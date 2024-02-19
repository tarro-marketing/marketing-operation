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
         Phone1 = `Phone Number(s)`) -> printshop_can

printshop_us |> 
  rename(Phone1 = `Phone Number(s)`) |> 
  mutate(Zipcode = as.character(Zipcode),
         Phone2 = NA,
         Phone3 = NA,
         Phone4 = NA,
         Phone5 = NA,
         Phone6 = NA,
         Phone7 = NA,
         Phone8 = NA,
         Phone9 = NA ) -> printshop_us


common_cols <- Reduce(intersect, list(printshop_can, printshop_us))

rm(cols_printshop_can, cols_printshop_us, common_cols)

printshop <-  rbind(printshop_can, printshop_us)

rm(printshop_can, printshop_us)

printshop <- printshop |> 
  mutate(Country = recode(Country, CAN = "Canada",
                           US = "United States"))

############################## old list ########################################

oldlist_can <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - CAN (96270-101360)")
oldlist_us <- read_sheet("1KrqQVRliHxPxnyELyejgy7_2ZvHsPdDglLoPOwksEIs", sheet = "Old List - US (80423-96269)")

##
cols_oldlist_can <- colnames(oldlist_can)
cols_oldlist_us <- colnames(oldlist_us)

print(cols_oldlist_can)
print(cols_oldlist_us)

# Find common column names
common_cols <- Reduce(intersect, list(cols_oldlist_can, cols_oldlist_us))

# Print the common column names
print(common_cols)


oldlist_can2 <- oldlist_can %>%
  rename(
    Last_Name = "CBE Contact Last Name",
    First_Name = "CBE Contact First Name",
    Address = Street,  # Assuming "Street" should match "Mailing Address"
    Phone_Number = "Phone Number",
    ZipCode = Zip,
    Ethnicity_Description = "CBE Vendor Ethnicity Description",
    Restaurant_Name = Name)

oldlist_us2 <- oldlist_us %>%
  rename(
    Last_Name = `Contact Last Name`,
    First_Name = `Contact First Name`,
    Address = `Mailing Address`,
    Phone_Number = TELEPHONENUMBER,
    Zip_Code = `Zip Code`,
    Ethnicity_Description = `Ethnic Code Desc`,
    City = `City Name`,
    Restaurant_Name=`Restaurant Name`,
    Category = "Ethnic Code Desc",
    ZipCode = `Zip Code`
    
  )

rm(oldlist_can, oldlist_us)

# Assuming 'Year Established' is a column only in oldlist_us and needs to be added to oldlist_can
oldlist_can3 <- oldlist_can2 %>%
  mutate(`Year Established` = NA)

# Assuming 'CBE Location Employment Size Description' is only in oldlist_can and needs to be added to oldlist_us
oldlist_us3 <- oldlist_us2 %>%
  mutate(`CBE Location Employment Size Description` = NA,
         Price = NA,
         Ethnicity_Description = Category,
         Restaurant_Name = as.character(Restaurant_Name),
         ZipCode = as.character(ZipCode))

rm(oldlist_can2, oldlist_us2)

oldlist_us4 <- oldlist_us3 %>% select(names(oldlist_can3))

oldlist <- bind_rows(oldlist_can3, oldlist_us4)

rm(oldlist_can3, oldlist_us3,oldlist_us4)



########################### lists combined - sam list ##########################

  
cols_brizo = colnames(brizo)
cols_printshop = colnames(printshop)
cols_list = colnames(oldlist)
  
  
commom_cols = Reduce(intersect, list(cols_brizo, cols_printshop, cols_list))
  
  
library(dplyr)

# Renaming for 'brizo'
brizo2 <- brizo %>%
  rename(
    RestaurantName = Name,
    Address = Street,
    City = `City/Town`,
    State = `State/Province Code`,
    ZipCode = `Zip/Postal Code`,
    FirstName = `First Name`,
    LastName = `Last Name`,
    PhoneNumber = Phone,  
    Email = `Most Common Email`,
    `Established` = `Year Established`
  ) %>%
  mutate(
    `Dupe (Y/N)` = NA,  
    `Match Address` = NA   
  ) |> 
  select(-c("Contact Email","Contact Phone"))


brizo2 <- brizo |> 
  rename(`Established` = `Year Established`) |> 
  

# Renaming for 'printshop'
printshop2 <- printshop %>%
  rename(
    RestaurantName = `Restaurant Name`,
    ZipCode = Zipcode,
    PhoneNumber = Phone1  # Assuming the primary phone number is 'Phone1'
  ) %>%
  mutate(
    FirstName = NA,
    LastName = NA,
    Email = NA,
    `Year Established` = NA  # Adding missing columns for alignment
  )

# Renaming for 'oldlist'
oldlist2 <- oldlist %>%
  rename(
    RestaurantName = Restaurant_Name,
    LastName = Last_Name,
    FirstName = First_Name,
    PhoneNumber = Phone_Number,
    EthnicityDescription = Ethnicity_Description
  ) %>%
  mutate(
    `Dupe (Y/N)` = NA,  # Assuming to add for alignment
    `Match Address` = NA,
    Email = NA  # Assuming 'oldlist' doesn't have an email column
  )

# Aligning column order of 'printshop' and 'oldlist' to 'brizo'
printshop_aligned <- printshop %>% select(names(brizo))
oldlist_aligned <- oldlist %>% select(names(brizo))

  
combined_dataset <- bind_rows(brizo, printshop_aligned, oldlist_aligned)






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



