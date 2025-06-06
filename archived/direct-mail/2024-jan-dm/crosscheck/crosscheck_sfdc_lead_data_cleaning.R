library(tidyverse)

sfdc_lead_jan <- read_csv("crosscheck/sfdc_dm_leads_cross_check.csv")


inboundcall_dm_jan <- read_csv("clean_data/inboundcall_dm_jan.csv", 
                               col_types = cols(Phone = col_character(), 
                                                `Extension Number` = col_character()))


sfdc_lead_jan <- sfdc_lead_jan |>
  mutate(`Mobile - Primary` = 
           str_replace_all(str_replace_all(`Mobile - Primary`, "-", ""), "[^0-9]", ""),
         `Business Phone`= 
           str_replace_all(str_replace_all(`Business Phone`, "-", ""), "[^0-9]", "")) |> 
  mutate(flow = case_when(`Created By` %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
                          TRUE ~ "inbound call")) |> 
  mutate(types = case_when(`Created By` == "JotForm Integration User" ~ "jotform",
                           `Created By` == "Carlito Academia" ~ "qr scan",
                           TRUE ~ "inbound call"))  ### create mql, sql, cw label

sfdc_january_dm <- sfdc_lead_jan |> 
  filter(str_detect(`Latest Campaign`, "Mkt_DM_Snowflake_2024Jan") |
           is.na(`Latest Campaign`)&`Created Date` <= "12/26/2023"&`Created Date`>="1/31/2024") |> 
  mutate(mql = case_when(`Menu Type` != "" & `State/Province (text only)` != "" & 
      (`Created By` == "JotForm Integration User" | `Lead Status` %in% c("Converted", "AE Assigned")) & 
      !(`Unqualified Reason` %in% c("Current Client",  "Duplicate", "Not a Restaurant")) ~ "TRUE", TRUE ~ "FALSE"
  )) |> 
  mutate(sql = case_when(`Opportunity ID`!="" ~ "TRUE", TRUE ~ "FALSE")) |> 
  mutate(cw = case_when(Stage == "Onboarded" ~ "TRUE",
                        TRUE ~ "FALSE")) |> 
  mutate(mql = case_when(sql=="TRUE"~"TRUE",
                         TRUE ~ mql)) |> 
  mutate(List = case_when(`Latest Campaign` == "Mkt_DM_Snowflake_2024JanV1"~"Brizo_US",
                          `Latest Campaign` == "Mkt_DM_Snowflake_2024JanV2"	~"Old_List_US",
                          `Latest Campaign` == "Mkt_DM_Snowflake_2024JanV3"	~"Print_Shop_US",
                          `Latest Campaign` == "Mkt_DM_Snowflake_2024JanV4"	~"Brizo_CAN",
                          `Latest Campaign` == "Mkt_DM_Snowflake_2024JanV5"	~"Oldlist_CAN",
                          `Latest Campaign` == "Mkt_DM_Snowflake_2024JanV6"	~"Print_Shop_CAN",
                          TRUE ~ NA))
  
inboundcall_select = inboundcall_dm_jan |> select(Phone, list_source) |> 
  rename(List_IBcall = list_source)
sfdc_january_dm |> 
  left_join(inboundcall_select, by = c( `Mobile - Primary`="Phone")) |> 
  left_join(inboundcall_select, by = c( `Business Phone` = "Phone")) |> 
  mutate(list_manual = case_when(`Business Phone`=="5169189535"~"Print_Shop_US",
                                 `Mobile - Primary`=="1885200811"~"Brizo_US",
                                 `Mobile - Primary`=="5105202176"~"SEM",
                                 `Mobile - Primary`=="9145140096"~"SMS",
                                 `Mobile - Primary`=="6265962768"~"SEO",
                                 `Mobile - Primary`=="7183136997"~"SEO",
                                 `Mobile - Primary`=="8604808458"~"SMS",
                                 `Mobile - Primary`=="2028340920"~"SEO",
                                 `Mobile - Primary`=="9172081674"~"SMS",
                                 `Mobile - Primary`=="6462875511"~"SMS",
                                 `Mobile - Primary`=="8314198588"~"SMS",
                                 `Mobile - Primary`=="4257661628"~"Print_Shop_US",
                                 `Mobile - Primary`=="1267938118"~"Brizo_US",
                                 `Mobile - Primary`=="3036387757"~"Brizo_US",
                                 `Mobile - Primary`=="8284596699"~"SEM",
                                 )) |> 
  mutate(list_source = coalesce(List, List_IBcall.x, List_IBcall.y,list_manual)) |> 
  rename(list_ibcall_mobile = List_IBcall.x,
         list_ibcall_business = List_IBcall.y) |> 
  select(-c(List,list_ibcall_mobile,list_ibcall_business,list_manual)) |> 
  select(flow, types, mql, sql, cw, list_source, everything()) |> 
  filter(list_source != "SMS" | is.na(list_source))-> test2

write_csv(test2, "crosscheck/sfdc_cross_check.csv", na = "")
