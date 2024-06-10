library(tidyverse)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

sfdc_leads <- read_csv("rejected_unqualified/sfdc_leads.csv")

sfdc_leads <- sfdc_leads |>
  mutate(
    Onboarded = case_when(Stage == "Onboarded" ~ TRUE, TRUE ~ FALSE),
    CW = case_when(Stage == "Closed Won" ~ TRUE, TRUE ~ FALSE),
    SQL = case_when(CW == TRUE ~ TRUE, 
                    `Opportunity ID` != "" ~ TRUE, TRUE ~ FALSE),
    MQL = case_when(
      SQL == TRUE ~ TRUE, 
      CW == TRUE ~ TRUE, 
      `Menu Type` != "" & 
        `State/Province (text only)` != "" 
      &
        (`Created By` == "JotForm Integration User" 
         | 
           `Lead Status` %in% c("Converted", "AE Assigned")
        ) 
      & 
        !(`Unqualified Reason`
          %in% c("Current Client", "Duplicate", "Not a Restaurant")) ~ TRUE,
      TRUE ~ FALSE
    ),
    MEL = case_when(
      !(`Unqualified Reason`%in% c("Current Client", "Duplicate")) ~ TRUE,
      TRUE ~ FALSE
    )
  )



  
sfdc_leads_added <- sfdc_leads |>
  mutate(`Created Date` = mdy(`Created Date`)) |>
  filter(`Created Date` >= as.Date("2024-01-01") & `Created Date`<= as.Date("2024-05-31")) |>
  mutate(
    `Rejected / Unqualified Reason` = coalesce(`Rejected Reason`, `Unqualified Reason`),
    `Lead Create Month` = month(`Created Date`, label = TRUE, abbr = TRUE),
    Group = case_when(
      !is.na(`Rejected Reason`) ~ "Rejected Lead",
      !is.na(`Unqualified Reason`) ~ "Unqualified Lead",
      TRUE ~ "Other"  )) |>
  replace_na(list(`Rejected / Unqualified Reason` = "Others"))





write_sheet(sfdc_leads_added, ss = "1wiYSntRdY7lKOfkkqNheWdYrJeoMebQYzfCoZwn7VO0", 
            sheet = "Data")

aggregated_summary <- sfdc_leads_added %>%
  group_by(`Lead Create Month`, Group, `Rejected / Unqualified Reason`) %>%
  summarize(Total_Count = n(), .groups = 'drop')

aggregated_summary22 <- aggregated_summary |> 
  filter(Group == "Rejected Lead") |> 
  select(-Group) |> 
  pivot_wider(names_from = `Rejected / Unqualified Reason`, values_from = Total_Count)

aggregated_summary22 <- aggregated_summary |> 
  filter(Group == "Unqualified Lead") |> 
  select(-Group) |> 
  pivot_wider(names_from = `Rejected / Unqualified Reason`, values_from = Total_Count)









ggplot(aggregated_summary, aes(x = `Lead Create Month`, 
                               y = Total_Count, fill = Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Bar Chart of Lead Create Month",
       x = "Lead Create Month",
       y = "Total Count") +
  theme_minimal()

aggregated_summary_2 <- sfdc_leads_added %>%
  group_by(`Lead Create Month`, Group, `Rejected / Unqualified Reason`) %>%
  summarize(Total_Count = n(), .groups = 'drop')

pivoted_data <- aggregated_summary %>%
  pivot_wider(names_from = Group, values_from = Total_Count, 
              values_fill = list(Total_Count = 0))



range_write(
  data = pivoted_data, 
  ss = "1wiYSntRdY7lKOfkkqNheWdYrJeoMebQYzfCoZwn7VO0", 
  sheet = "M/M Trends", 
  range = paste0("E", "1"), 
  col_names = TRUE
)



range_write(
  data = aggregated_summary, 
  ss = "1wiYSntRdY7lKOfkkqNheWdYrJeoMebQYzfCoZwn7VO0", 
  sheet = "M/M Trends", 
  range = paste0("E", "1"), 
  col_names = TRUE
)
sheet_append(aggregated_summary, 
             ss = "1wiYSntRdY7lKOfkkqNheWdYrJeoMebQYzfCoZwn7VO0", 
             sheet = "M/M Trends")
