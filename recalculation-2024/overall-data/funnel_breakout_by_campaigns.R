library(tidyverse)

sfdc_leads <- read_csv("clean_data/final_campaign_sfdc_lead_v2.csv")

sfdc_leads <- sfdc_leads |> 
  select(-campaign_name)

october_leads <- sfdc_leads |> 
  filter(str_detect(Campaign_Tags, regex("october", ignore_case = TRUE)) | 
           (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) & 
              str_detect(Campaign_by_Month, regex("oct", ignore_case = TRUE))))




november_leads <- sfdc_leads |> 
  filter(str_detect(Campaign_Tags, regex("Airfryer", ignore_case = TRUE)) | 
           (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) & 
              str_detect(Campaign_by_Month, regex("nov", ignore_case = TRUE))))



december_leads <- sfdc_leads |> 
  filter(str_detect(Campaign_Tags, regex("Christmas", ignore_case = TRUE)) | 
           (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) & 
              str_detect(Campaign_by_Month, regex("dec", ignore_case = TRUE))))


