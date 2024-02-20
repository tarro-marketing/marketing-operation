library(tidyverse)

library(googlesheets4)


gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")


inbound_call <- read_sheet("1WPDjz4PYMIpy278YNE3zScqtFnFdYivP3AXAtEalpnA", sheet = "Inbound Call Notes - Raw Data (Linked)")


inboundcall <- inbound_call |>
  mutate(Date = as.character(Date)) |>
  filter(str_count(Date) == 10) |>
  mutate(Date = as.POSIXct(as.numeric(Date), origin = "1970-01-01", tz = "UTC")) |>
  filter(Date >= as.POSIXct("2023-10-01", tz = "UTC") & Date <= as.POSIXct("2024-02-15", tz = "UTC")) |>
  mutate(
    Date = format(Date, "%m-%d-%Y"),
    Date = mdy(Date)
  ) |>
  arrange(desc(Date))

rm(inbound_call)

inboundcall <- inboundcall |>
  filter((str_detect(`Extension Channel`, "DM") | str_detect(`Channel Collected`, "DM")) & `Lead Status` != "Internal") |>
  mutate(
    Campaign_by_Month = case_when(
      Date >= mdy("10-01-2023") & Date <= mdy("10-17-2023") ~ "oct-drop1",
      Date >= mdy("10-18-2023") & Date <= mdy("11-04-2023") ~ "oct-drop2",
      Date >= mdy("11-05-2023") & Date <= mdy("11-22-2023") ~ "nov-drop1",
      Date >= mdy("11-23-2023") & Date <= mdy("12-04-2023") ~ "nov-drop1",
      Date >= mdy("12-05-2023") & Date <= mdy("12-13-2023") ~ "dec-drop1",
      Date >= mdy("12-14-2023") & Date <= mdy("12-27-2023") ~ "dec-drop2",
      Date >= mdy("12-28-2023") & Date <= mdy("01-31-2024") ~ "January",
      Date >= mdy("02-01-2024") & Date <= mdy("02-15-2024") ~ "Febuary",
      TRUE ~ NA_character_
    ),
    Campaign_Tags = case_when(
      str_detect(`Campaign Sent`, "October") ~ "oct_freetrail",
      str_detect(`Campaign Sent`, "AirfryerEnvelope") ~ "nov-drop1env",
      str_detect(`Campaign Sent`, "AirfryerV6") ~ "nov-drop2",
      # Repeated condition for "AirfryerV6" removed
      str_detect(`Campaign Sent`, "Airfryer") ~ "nov-drop1tridfold",
      str_detect(`Campaign Sent`, "ChristmasUSDrop1") ~ "dec-Drop1",
      str_detect(`Campaign Sent`, "ChristmasUSDrop2") ~ "dec-Drop2", # Presumably meant dec-Drop2
      str_detect(`Campaign Sent`, "ChristmasV2") ~ "dec-drop1can",
      str_detect(`Campaign Sent`, "DecemberEng") ~ "dec-drop1eng",
      str_detect(`Campaign Sent`, "JanV1") ~ "jan_brizo_us",
      str_detect(`Campaign Sent`, "JanV2") ~ "jan_oldlist_us",
      str_detect(`Campaign Sent`, "JanV3") ~ "jan_printshop_us",
      str_detect(`Campaign Sent`, "JanV4") ~ "jan_brizo_can",
      str_detect(`Campaign Sent`, "JanV5") ~ "jan_oldlist_can",
      str_detect(`Campaign Sent`, "JanV6") ~ "jan_print_can",
      TRUE ~ NA_character_
    )
  ) |>
  select(Date, `Extension Channel`, `Channel Collected`, `Campaign Sent`, Campaign_by_Month, Campaign_Tags, `Extension Number`, Phone, everything())

inboundcall <- inboundcall |> 
  mutate(Phone = str_replace_all(Phone, "[^\\d]", "")) |> 
  mutate(Phone = na_if(Phone, "")) |> 
  mutate(`Extension Number` = as.character(`Extension Number`),
    `Extension Number` = recode(`Extension Number`, 
                                     "NULL" = NA_character_))
  

  


write_csv(inboundcall, "recalculation-2024/clean_data/inbound_call.csv")
