library(tidyverse)

sfdc_leads <- read_csv("recalculation-2024/clean-data/final_campaign_sfdc_lead_v2.csv",
  col_types = cols(
    Mobile__Primary_SFDC = col_character(),
    Business_Phone_SFDC = col_character()
  )
)
inboundcall <- read_csv("recalculation-2024/clean-data/inbound_call.csv",
  col_types = cols(
    `Extension Number` = col_character(),
    Phone = col_character()
  )
)


sfdc_leads_c <- sfdc_leads |>
  select(-campaign_name)

inboundcall_c <- inboundcall |>
  select(
    Phone, `Extension Channel`, `Channel Collected`,
    `Campaign Sent`, `Campaign Sent`, Campaign_Tags,
    `Extension Number`, Date, Campaign_by_Month
  ) |>
  rename_with(~ str_replace_all(., " ", "_")) |>
  rename_with(~ paste0(., "_IB_Call"))



############## october ###############
inboundcall_oct <- inboundcall_c |>
  filter(str_detect(Campaign_Tags_IB_Call, "oct"))

rm(sfdc_leads, inboundcall)

# sfdc_leads_c |>
# left_join(inboundcall_c, by = c("Mobile__Primary_SFDC" = "Phone_IB_Call"))

sam_list_october <- read_csv("recalculation-2024/october-dm-data/sam_list_october.csv")


october_leads <- sfdc_leads_c |>
  filter(str_detect(Campaign_Tags, regex("october", ignore_case = TRUE)) |
    (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) &
      str_detect(Campaign_by_Month, regex("oct", ignore_case = TRUE)))) |>
  mutate(
    SAM_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% sam_list_october$`Business Phone`,
    SAM_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% sam_list_october$`Business Phone`,
    IBC_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% inboundcall_oct$Phone_IB_Call,
    IBC_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% inboundcall_oct$Phone_IB_Call
  ) |>
  left_join(inboundcall_oct, by = c("Mobile__Primary_SFDC" = "Phone_IB_Call"))


rm(sam_list_october, inboundcall_oct)


october_leads <- october_leads |> 
  mutate(Drop_Group = case_when(Created_Date_SFDC<=mdy("10/18/2023") ~ "DM1",
                                      TRUE ~ "DM2"),
         Campaign_Group = "October Leads") |>   
  select( "Campaign_Group", "Drop_Group", "flow", "types", "CW", "SQL", "MQL", "Campaign_Tags","Campaign_by_Month", everything())



write_csv(october_leads, "recalculation-2024/october-dm-data/october_leads.csv", na = "")

rm(october_leads)

############# november ###################

inboundcall_nov <- inboundcall_c |>
  filter(str_detect(Campaign_Tags_IB_Call, "nov")) |> 
  distinct(Phone_IB_Call, .keep_all = TRUE)

sam_list_november <- read_csv("recalculation-2024/november-dm-data/sam_list_november.csv")


november_leads <- sfdc_leads_c |>
  filter(str_detect(Campaign_Tags, regex("Airfryer", ignore_case = TRUE)) |
    (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) &
      str_detect(Campaign_by_Month, regex("nov", ignore_case = TRUE)))) |> 
  mutate(
    SAM_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% sam_list_november$`Business Phone`,
    SAM_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% sam_list_november$`Business Phone`,
    IBC_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% inboundcall_nov$Phone_IB_Call,
    IBC_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% inboundcall_nov$Phone_IB_Call
  ) |>
  left_join(inboundcall_nov, by = c("Mobile__Primary_SFDC" = "Phone_IB_Call"))

rm(sam_list_november, inboundcall_nov)

november_leads <- november_leads |> 
  mutate(Campaign_Group = "November Leads")

november_leads <- november_leads |> 
  mutate(Drop_Group = case_when(Created_Date_SFDC<=mdy("10/17/2023") ~ "DM1",
                                TRUE ~ "DM2"),
         Campaign_Group = "November Leads") |>   
  select( "Campaign_Group", "Drop_Group", "flow", "types", "CW", "SQL", "MQL", "Campaign_Tags","Campaign_by_Month", everything())


write_csv(november_leads, "recalculation-2024/november-dm-data/november_leads.csv", na = "")


rm(november_leads)
############# december ###################

inboundcall_dec <- inboundcall_c |>
  filter(str_detect(Campaign_Tags_IB_Call, "dec")) |> 
  distinct(Phone_IB_Call, .keep_all = TRUE)

sam_list_december <- read_csv("recalculation-2024/december-dm-data/sam_list_december.csv")

december_leads <- sfdc_leads_c |>
  filter(str_detect(Campaign_Tags, regex("Christmas", ignore_case = TRUE)) |
    (!str_detect(Campaign_Tags, regex("DM", ignore_case = TRUE)) &
      str_detect(Campaign_by_Month, regex("dec", ignore_case = TRUE)))) |> 
  mutate(
    SAM_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% sam_list_december$PhoneNumber,
    SAM_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% sam_list_december$PhoneNumber,
    IBC_MATCH_Mobile = !is.na(Mobile__Primary_SFDC) & Mobile__Primary_SFDC %in% inboundcall_dec$Phone_IB_Call,
    IBC_MATCH_Business = !is.na(Business_Phone_SFDC) & Business_Phone_SFDC %in% inboundcall_dec$Phone_IB_Call
  ) |>
  left_join(inboundcall_dec, by = c("Mobile__Primary_SFDC" = "Phone_IB_Call"))

december_leads <- december_leads |> 
  mutate(Campaign_Group = "December Leads")



rm(sam_list_december, inboundcall_dec)



rm(inboundcall_c, sfdc_leads_c)


rm(inboundcall_dec, inboundcall_nov, inboundcall_oct)

write_csv(december_leads, "recalculation-2024/december-dm-data/december_leads.csv", na = "")

rm(inboundcall_c, sfdc_leads_c)


