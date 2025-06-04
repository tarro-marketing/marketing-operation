library(tidyverse)
library(googlesheets4)

gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

final_sfdc_lead <- read_csv("overall-data/final_sfdc_lead.csv",
                            col_types = cols(Mobile__Primary_SFDC = col_character(),
                                             Business_Phone_SFDC = col_character(),
                                             WeChat_External_Id_SFDC = col_character(),
                                             Created_Date_SFDC = col_date(format = "%Y-%m-%d"),
                                             Opportunity_Created_Date_SFDC = col_date(format = "%Y-%m-%d"),
                                             Oppt_Close_Date_SFDC = col_date(format = "%Y-%m-%d"),
                                             Placeholder_Live_Date_SFDC = col_date(format = "%Y-%m-%d")))

feb_leads <- final_sfdc_lead |>
  filter(str_detect(campaign_name,"(?i)feb") |
           (Created_Date_SFDC >= "2024-02-01"& Created_Date_SFDC <=	"2024-02-29" ))


feb_campaign_leads <- final_sfdc_lead |> filter(str_detect(campaign_name, "(?i)feb"))


feb_date_leads <- final_sfdc_lead |>
  filter(Created_Date_SFDC >= "2024-02-01" & Created_Date_SFDC <= "2024-02-29")


sheet_write(data = feb_date_leads, ss = "1_XQ9PDwznId3Udq53RcSIThjUWxQMK39be3Nf7ZqsA0", sheet = "Leads")
