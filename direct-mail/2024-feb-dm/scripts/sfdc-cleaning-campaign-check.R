library(tidyverse)


sfdc_lead <- read_csv("raw-data/Feb-DM-Performance-Overview-Report.csv")
Brizo <- read_csv("raw-data/Brizo.csv")


group1 <- c("SC", "WI", "MS", "IN", "PA", "KY", "MI", "NJ", "CO", "ID", "NM", "IA", "CA", "MN", "VT", "KS", "NE")
group2 <- c("MD", "DE", "WY", "OH", "NV", "TN", "WV", "AL", "UT", "IL", "MT", "NY", "OK", "NH", "TX", "SD")

group3 <- c("FL", "AR", "RI", "CT", "NC", "DC", "GA", "VA", "MA", "WA", "LA", "MO", "OR", "AZ", "ND", "ME")


Brizo <- Brizo |> 
rename(State = `State/Province Code`) |> 
mutate( Gift_Group = case_when(
  State %in% group1 ~ "SQL Gift",
  State %in% group2 ~ "CW Gift",
  State %in% group3 ~ "No Promotion",
  TRUE ~ "Error"))


date_columns <- names(sfdc_lead)[str_detect(names(sfdc_lead), "Date")]



sfdc_lead_clean <- sfdc_lead |> 
  mutate(across(all_of(date_columns), mdy)) |> 
  filter(`Created Date` >= as_date('2024-01-27')&
           (str_detect(`Lead Channel`, "DM") | str_detect(`Lead Channel`, "SMS")| 
              str_detect(`Lead Channel`, "Google") |str_detect(`Lead Channel`, "Email"))&
           (!str_detect(`Latest Campaign`,"Jan") | is.na(`Latest Campaign`))) |> 
  rename(State = `State/Province (text only)`) |> 
  mutate( gift_group = case_when(
    str_detect(`Latest Campaign`,regex("SQL", ignore_case = TRUE)) ~ "SQL Gift",
    str_detect(`Latest Campaign`,regex("CW", ignore_case = TRUE)) ~ "CW Gift",
    str_detect(`Latest Campaign`,regex("nopromo", ignore_case = TRUE)) ~ "No Promotion",
    str_detect(`Latest Campaign`,regex("SQL", ignore_case = TRUE)) ~ "SQL Gift",
    str_detect(`Latest Campaign`,"Mkt_PaidSearch_Google_AllCampaigns") ~ "Google_AllCampaigns",
    str_detect(`Latest Campaign`,regex("WeChat", ignore_case = TRUE)) ~ "WeChat",
    str_detect(`Latest Campaign`,regex("SMS_Internal_General", ignore_case = TRUE)) ~ "SMS_Internal",
    State %in% group1 ~ "SQL Gift",
    State %in% group2 ~ "CW Gift",
    State %in% group3 ~ "No Promotion",
    TRUE ~ "Error")) |> 
  mutate(funnel = case_when(
    Stage == "Onboarded" ~ "CW",
    `Opportunity ID`!="" ~ "SQL",
    `Menu Type` != "" &  State != "" & (`Created By` == "JotForm Integration User" | 
                                          `Lead Status` %in% c("Converted", "AE Assigned")) &
      !(`Unqualified Reason` %in% c("Current Client","Duplicate","Not a Restaurant")) ~ "MQL",
    TRUE ~ "MEL")) |> 
mutate(`Mobile - Primary` = str_replace_all(
    str_replace_all(`Mobile - Primary`, "-", ""), "[^0-9]", ""),
         `Business Phone`= str_replace_all(
           str_replace_all(`Business Phone`, "-", ""), "[^0-9]", ""),
    flow = case_when(`Created By` %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
                          TRUE ~ "inbound call"),
    types = case_when(`Created By` == "JotForm Integration User" ~ "jotform",
                           `Created By` == "Carlito Academia" ~ "qr scan",
                           TRUE ~ "inbound call")) |> 
  select(gift_group, `Latest Campaign`, State,funnel,flow, types, everything())



write_csv(sfdc_lead_clean,"temp/sfdc_clean.csv", na = "")








