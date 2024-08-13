library(tidyverse)

marketing_campaigns_with_contacts_report <- read_csv("crosscheck/marketing_campaigns_with_contacts_report.csv") |> 
  mutate(`Mobile - Primary` = str_replace_all(str_replace_all(`Mobile - Primary`, "-", ""), "[^0-9]", ""))

sfdc_cross_check <- read_csv("crosscheck/sfdc_cross_check.csv",
                             col_types = cols(`Mobile - Primary` = col_character(),
                                              `Business Phone` = col_character()))

marketing_campaigns_with_contacts_report |> 
  filter(str_detect(`Campaign Name`, "Jan")) |> 
  group_by(`Campaign Name`) |> 
  summarise(counts=n())

mkt_campain_report <- marketing_campaigns_with_contacts_report |> 
  select(`Contact ID`, `Campaign Name`)

matching = left_join(sfdc_cross_check,mkt_campain_report, by = "Contact ID")

write_csv(matching,"crosscheck/matched_mkt_campaign.csv")


matching |> 
  drop_na(`Campaign Name`) |> 
  select(`Campaign Name`, `Latest Campaign`, everything()) -> matching

