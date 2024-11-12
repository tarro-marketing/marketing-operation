library(tidyverse)

Brizo_CAN <- read_csv("raw_data/sam_list/Brizo_CAN.csv") |> 
  select("Snowball Map",	"Name",	"Street",	"City/Town",	"State/Province Code",	"Country","Zip/Postal Code",	"Cuisines (Regional)",	"Phone") |> 
  mutate(List = "Brizo_CAN")|>
  rename(`Restaurant Name` = Name,
         Address = Street,
         City = `City/Town`,
         State = `State/Province Code`,
         Zipcode = `Zip/Postal Code`,
         `Cuisine Type` = `Cuisines (Regional)`)

Brizo_US <- read_csv("raw_data/sam_list/Brizo_US.csv")|> 
  select("Snowball Map",	"Name",	"Street",	"City/Town",	"State/Province Code",	"Country",	"Zip/Postal Code",	"Cuisines (Regional)",	"Phone") |>
  mutate(List = "Brizo_US") |> 
  rename(`Restaurant Name` = Name,
         Address = Street,
         City = `City/Town`,
         State = `State/Province Code`,
         Zipcode = `Zip/Postal Code`,
         `Cuisine Type` = `Cuisines (Regional)`)

Old_List_CAN <- read_csv("raw_data/sam_list/Old_List_CAN.csv")|> 
  select("Snowball Map",	"Name",	"Street",	"City",	"State",	"Zip",	"Category",	"Phone Number") |> 
  mutate(List = "Oldlist_CAN",
         Country = "Canada") |> 
  rename(`Restaurant Name` = Name,
         Address = Street,
         Zipcode = `Zip`,
         `Cuisine Type` = `Category`,
         Phone=`Phone Number`)



Old_List_US <- read_csv("raw_data/sam_list/Old_List_US.csv")|> 
  select("Snowball Map",	"Restaurant Name",	"Mailing Address",	"City Name",	"State",	"Zip Code",	"TELEPHONENUMBER") |> 
  mutate(List = "Old_List_US",
         Country = "United States",
         `Cuisine Type` = NA) |> 
  rename(Address = `Mailing Address`,
         City = `City Name`,
         Zipcode = `Zip Code`,
         Phone = TELEPHONENUMBER)


Print_Shop_CAN <- read_csv("raw_data/sam_list/Print_Shop_CAN.csv")|> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"State",	"Country",	"Zipcode",	"Cuisine Type",	"Phone Number(s)") |> 
  mutate(List = "Print_Shop_CAN") |> 
  rename(Phone = `Phone Number(s)`)


Print_Shop_US <- read_csv("raw_data/sam_list/Print_Shop_US.csv")|> 
  select("Snowball Map",	"Restaurant Name",	"Address",	"City",	"State",	"Country",	"Zipcode",	"Cuisine Type",	"Phone Number(s)") |> 
  mutate(List = "Print_Shop_US") |> 
  rename(Phone = `Phone Number(s)`)


sam_list = rbind(Brizo_US, Brizo_CAN, Print_Shop_CAN, Print_Shop_US, Old_List_CAN, Old_List_US) 

sam_list= sam_list |> 
  rename(snowball_map = `Snowball Map`,
         list_source = List) |> 
  mutate(Phone=str_replace_all(str_replace_all(Phone, "-", ""), "[^0-9]", "")) 


write_csv(sam_list,"clean_data/sam_list.csv", na="")

