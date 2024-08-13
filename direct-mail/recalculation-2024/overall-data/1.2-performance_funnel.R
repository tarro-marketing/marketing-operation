library(tidyverse)
sfdc_leads <- read_csv("recalculation-2024/clean-data/final_campaign_sfdc_lead.csv")


####################Campaign #######################

sfdc_leads_c <- sfdc_leads |>
  filter(str_detect(Lead_Channel_SFDC, "DM")) |>
  mutate(
    Campaign_by_Month = case_when(
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
        Created_Date_SFDC >= mdy("10-03-2023") &  Created_Date_SFDC <= mdy("10-17-2023") ~ "October-drop1", # october
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("10-18-2023") & Created_Date_SFDC <= mdy("11-02-2023") ~ "October-drop2", # october
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("11-03-2023") & Created_Date_SFDC <= mdy("11-22-2023") ~ "November-drop1", # NOVEMBER
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("11-23-2023") & Created_Date_SFDC <= mdy("12-04-2023") ~ "November-drop1", # NOVEMBER
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("12-05-2023") & Created_Date_SFDC <= mdy("12-13-2023") ~ "December-drop1", # DECEMBER
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("12-14-2023") & Created_Date_SFDC <= mdy("12-31-2023") ~ "December-drop2", # DECEMBER
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("01-01-2023") & Created_Date_SFDC <= mdy("01-31-2024") ~ "January", # JANUARY
      is.na(Latest_Campaign_SFDC) & is.na(Campaign_Name_Contact_Report) & 
      Created_Date_SFDC >= mdy("02-02-2024") & Created_Date_SFDC <= mdy("02-19-2024") ~ "Febuary", # FEBUARY
      TRUE ~ NA_character_
    ),
    Latest_Campaign_SFDC = recode(Latest_Campaign_SFDC,
                                  "-" = NA_character_),
    Campaign_Tags = coalesce(Latest_Campaign_SFDC, Campaign_Name_Contact_Report, Campaign_by_Month)) |> 
  drop_na(Campaign_Tags)


rm(sfdc_leads)

write_csv(sfdc_leads_c, "recalculation-2024/clean-data/final_campaign_sfdc_lead_v2.csv")

