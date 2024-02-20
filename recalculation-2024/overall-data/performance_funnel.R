library(tidyverse)
setwd("~/PerformanceMarketing/direct-mail-2023/recalculation-2024")
final_campaign_sfdc_lead <- read_csv("clean_data/final_campaign_sfdc_lead.csv")

################### Basic Format and Lead Source ######################
sfdc_lead__final <- final_campaign_sfdc_lead |>
  mutate(
    Mobile__Primary_SFDC = str_replace_all(str_replace_all(Mobile__Primary_SFDC, "-", ""), "[^0-9]", ""),
    Business_Phone_SFDC = str_replace_all(str_replace_all(Business_Phone_SFDC, "-", ""), "[^0-9]", "")
  ) |>
  mutate(flow = case_when(
    Created_By_SFDC %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
    TRUE ~ "inbound call"
  )) |>
  mutate(types = case_when(
    Created_By_SFDC == "JotForm Integration User" ~ "jotform",
    Created_By_SFDC == "Carlito Academia" ~ "qr scan",
    TRUE ~ "inbound call"
  ))  |> 
  mutate(across(where(is.character), ~recode(., "-" = NA_character_)))


rm(final_campaign_sfdc_lead)

###################### Funnel ########################
sfdc_leads <- sfdc_lead__final |>
  mutate(
    CW = case_when(
      Stage_SFDC == "Closed Won" | Stage_SFDC == "Onboarded" ~ TRUE,
      TRUE ~ FALSE
    ),
    SQL = case_when(
      CW == TRUE ~ TRUE,
      Opportunity_ID_SFDC != "" ~ TRUE,
      TRUE ~ FALSE
    ),
    MQL = case_when(
      SQL == TRUE ~ TRUE,
      CW == TRUE ~ TRUE,
      Menu_Type_SFDC != "" &
        StateProvince_text_only_SFDC != "" &
        (
          Created_By_SFDC == "JotForm Integration User" |
            Lead_Status_SFDC %in% c("Converted", "AE Assigned")
        ) &
        !(Unqualified_Reason_SFDC %in% c("Current Client", "Duplicate", "Not a Restaurant")
        ) ~ TRUE,
      TRUE ~ FALSE
    )
  )


rm(sfdc_lead__final)



####################Campaign #######################

sfdc_leads_c <- sfdc_leads |>
  filter(str_detect(Lead_Channel_SFDC, "DM")) |>
  mutate(
    Campaign_by_Month = case_when(
      Created_Date_SFDC >= mdy("10-01-2023") & Created_Date_SFDC <= mdy("10-17-2023") ~ "oct-drop1",
      Created_Date_SFDC >= mdy("10-18-2023") & Created_Date_SFDC <= mdy("11-04-2023") ~ "oct-drop2",
      Created_Date_SFDC >= mdy("11-05-2023") & Created_Date_SFDC <= mdy("11-22-2023") ~ "nov-drop1",
      Created_Date_SFDC >= mdy("11-23-2023") & Created_Date_SFDC <= mdy("12-04-2023") ~ "nov-drop1",
      Created_Date_SFDC >= mdy("12-05-2023") & Created_Date_SFDC <= mdy("12-13-2023") ~ "dec-drop1",
      Created_Date_SFDC >= mdy("12-14-2023") & Created_Date_SFDC <= mdy("12-27-2023") ~ "dec-drop2",
      Created_Date_SFDC >= mdy("12-28-2023") & Created_Date_SFDC <= mdy("01-31-2024") ~ "January",
      Created_Date_SFDC >= mdy("02-01-2024") & Created_Date_SFDC <= mdy("02-19-2024") ~ "Febuary",
      TRUE ~ NA_character_
    ),
    Latest_Campaign_SFDC = recode(Latest_Campaign_SFDC,
                                  "-" = NA_character_),
    Campaign_Tags = coalesce(Latest_Campaign_SFDC, Campaign_Name_Contact_Report, Campaign_by_Month)) |> 
  drop_na(Campaign_Tags)


rm(sfdc_leads)

write_csv(sfdc_leads_c, "clean_data/final_campaign_sfdc_lead_v2.csv")

