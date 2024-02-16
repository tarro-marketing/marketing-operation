library(tidyverse)


sfdc_lead <- read_csv("~/PerformanceMarketing/direct-mail-2023/funnel_update/clean-data/final_campaign_sfdc_lead.csv")


sfdc_lead |> 
  group_by(Latest_Campaign_SFDC) |> 
  summarise(counts=n()) -> campaign
rm(campaign)


sfdc_lead <- sfdc_lead |> 
  mutate(Created_Date_SFDC = mdy(Created_Date_SFDC)) |> 
  filter(str_detect(`Latest Campaign`, "(?i)christmas") | Created_Date_SFDC >= mdy(12-15-12-31) ) -> december_dm_lead


december_dm_lead = december_dm_lead %>%
  mutate(drop_group = case_when(
    `Latest Campaign` %in% c("Mkt_DM_Snowflake_2023ChristmasUSDrop1", 
                             "Mkt_DM_Snowflake_2023ChristmasV1", 
                             "Mkt_DM_Snowflake_2023ChristmasV3", 
                             "Mkt_DM_Snowflake_2023ChristmasV5") ~ "Drop 1 - US",
    `Latest Campaign` %in% c("Mkt_DM_Snowflake_2023ChristmasCanada", 
                             "Mkt_DM_Snowflake_2023ChristmasV2", 
                             "Mkt_DM_Snowflake_2023ChristmasV4", 
                             "Mkt_DM_Snowflake_2023ChristmasV6") ~ "Drop 1 - Canada",
    `Latest Campaign` %in% c("Mkt_DM_Snowflake_2023ChristmasUSDrop2", 
                             "Mkt_DM_Snowflake_2023ChristmasV8", 
                             "Mkt_DM_Snowflake_2023ChristmasV9", 
                             "Mkt_DM_Snowflake_2023ChristmasV10") ~ "Drop 2 - US",
    TRUE ~ NA_character_)) |> 
  mutate(language_test = case_when(
    `Latest Campaign` %in% c("Mkt_DM_Snowflake_2023ChristmasUSDrop1", 
                             "Mkt_DM_Snowflake_2023ChristmasV1", 
                             "Mkt_DM_Snowflake_2023ChristmasV3", 
                             "Mkt_DM_Snowflake_2023ChristmasV5",
                             "Mkt_DM_Snowflake_2023ChristmasCanada", 
                             "Mkt_DM_Snowflake_2023ChristmasV2", 
                             "Mkt_DM_Snowflake_2023ChristmasV4", 
                             "Mkt_DM_Snowflake_2023ChristmasV6") ~ "Chinese Version",
    TRUE ~ NA_character_)) |> 
  mutate(`Mobile - Primary` = str_replace_all(str_replace_all(`Mobile - Primary`, "-", ""), "[^0-9]", ""),
         `Business Phone`= str_replace_all(str_replace_all(`Business Phone`, "-", ""), "[^0-9]", "")) |> 
  mutate(flow = case_when(`Created By` %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
                          TRUE ~ "inbound call")) |> 
  mutate(types = case_when(`Created By` == "JotForm Integration User" ~ "jotform",
                           `Created By` == "Carlito Academia" ~ "qr scan",
                           TRUE ~ "inbound call"))  ### create mql, sql, cw label

december_dm_lead <- december_dm_lead |> 
  mutate(mql = case_when(
    `Menu Type` != "" & 
      `State/Province (text only)` != "" & 
      (`Created By` == "JotForm Integration User" | 
         `Lead Status` %in% c("Converted", "AE Assigned")) & 
      !(`Unqualified Reason` %in% c("Current Client", "Duplicate", "Not a Restaurant")) ~ "TRUE",
    TRUE ~ "FALSE"
  )) |> 
  mutate(sql = case_when(`Opportunity ID`!="" ~ "TRUE",
                         TRUE ~ "FALSE")) |> 
  mutate(cw = case_when(Stage == "Closed Won" | Stage == "Onboarded" ~ "TRUE",
                        TRUE ~ "FALSE"))




write_csv(december_dm_lead, "Clean-Data/december_dm_lead.csv")




