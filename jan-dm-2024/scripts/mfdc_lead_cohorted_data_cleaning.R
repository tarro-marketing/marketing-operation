library(tidyverse)

sfdc_lead_jan <- read_csv("jan-dm-2024/clean_data/january_leads.csv")






inboundcall_dm_jan <- read_csv("jan-dm-2024/clean_data/inboundcall_dm_jan.csv",
  col_types = cols(
    Phone = col_character(),
    `Extension Number` = col_character()
  )
)


sfdc_lead_jan <- sfdc_lead_jan |>
  mutate(
    Mobile__Primary_SFDC =
      str_replace_all(str_replace_all(Mobile__Primary_SFDC, "-", ""), "[^0-9]", ""),
    Business_Phone_SFDC =
      str_replace_all(str_replace_all(Business_Phone_SFDC, "-", ""), "[^0-9]", "")
  )

sfdc_january_dm <- sfdc_lead_jan |>
  mutate(List = case_when(
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV1" ~ "Brizo_US",
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV2" ~ "Old_List_US",
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV3" ~ "Print_Shop_US",
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV4" ~ "Brizo_CAN",
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV5" ~ "Oldlist_CAN",
    Latest_Campaign_SFDC == "Mkt_DM_Snowflake_2024JanV6" ~ "Print_Shop_CAN",
    TRUE ~ NA
  ))

inboundcall_select <- inboundcall_dm_jan |>
  select(Phone, list_source) |>
  rename(List_IBcall = list_source)

sfdc_january_dm |>
  left_join(inboundcall_select, by = c(Mobile__Primary_SFDC = "Phone")) |>
  left_join(inboundcall_select, by = c(Business_Phone_SFDC = "Phone")) |>
  mutate(list_manual = case_when(
    Business_Phone_SFDC == "5169189535" ~ "Print_Shop_US",
    Mobile__Primary_SFDC == "1885200811" ~ "Brizo_US",
    Mobile__Primary_SFDC == "5105202176" ~ "SEM",
    Mobile__Primary_SFDC == "9145140096" ~ "SMS",
    Mobile__Primary_SFDC == "6265962768" ~ "SEO",
    Mobile__Primary_SFDC == "7183136997" ~ "SEO",
    Mobile__Primary_SFDC == "8604808458" ~ "SMS",
    Mobile__Primary_SFDC == "2028340920" ~ "SEO",
    Mobile__Primary_SFDC == "9172081674" ~ "SMS",
    Mobile__Primary_SFDC == "6462875511" ~ "SMS",
    Mobile__Primary_SFDC == "8314198588" ~ "SMS",
    Mobile__Primary_SFDC == "4257661628" ~ "Print_Shop_US",
    Mobile__Primary_SFDC == "1267938118" ~ "Brizo_US",
    Mobile__Primary_SFDC == "3036387757" ~ "Brizo_US",
    Mobile__Primary_SFDC == "8284596699" ~ "SEM",
  )) |>
  mutate(list_source = coalesce(List, List_IBcall.x, List_IBcall.y, list_manual)) |>
  rename(
    list_ibcall_mobile = List_IBcall.x,
    list_ibcall_business = List_IBcall.y
  ) |>
  select(-c(List, list_ibcall_mobile, list_ibcall_business, list_manual)) |>
  select(flow, types, MQL, SQL, CW, list_source, everything()) |>
  filter(list_source != "SMS" | is.na(list_source)) -> test2

write_csv(test2, "jan-dm-2024/clean_data/sfdc_jan_leads_UPDATE.csv", na = "")
