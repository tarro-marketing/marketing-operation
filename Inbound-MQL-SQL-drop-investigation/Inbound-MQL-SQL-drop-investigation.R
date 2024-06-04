
#########################################################################
################# loading necessary package and info ####################
#########################################################################
# Retrieve and use credentials from keyring

library(tidyverse)
library(reactable)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)

campaign_report <- read_csv("Inbound-MQL-SQL-drop-investigation/Mkt Campaigns Leads (Lead + Cotact).csv.csv")


# Ensure the `Member First Associated Date` is in date format
campaign_report <- campaign_report |>
  mutate(`Member First Associated Date` = as.Date(`Member First Associated Date`, format = "%m/%d/%Y")) 

# Add week information
campaign_report_add_week <- campaign_report |>
  mutate(Week = floor_date(`Member First Associated Date`, "week", week_start = 1)) |>
  mutate(Week = paste0(format(Week, "%m/%d"), " - ", format(Week + days(6), "%m/%d")))



campaign_report_add_metric <- campaign_report_add_week |>
  mutate(
    `Funnel Stage` = case_when(
      `Lead: Converted Opportunity: Stage` %in% c("Onboarded", "Closed Won") ~ "CW",
      !is.na(`Lead: Converted Opportunity: Created Date`) ~ "SQL",
      !is.na(`Lead: First MQL TimeStamp`) &
        !`Lead: Unqualified Reason` %in% c("Duplicate", "Current Client", "Not a Restaurant", "Incorrect Phone Number") &
        !is.na(`Lead: State/Province (text only)`) &
        !is.na(`Lead: Menu Type`) ~ "MQL",
      !is.na(`Lead: First MEL Timestamp`) &
        !`Lead: Unqualified Reason` %in% c("Duplicate", "Current Client") ~ "MEL",
      TRUE ~ "Unqualified MEL" # Default case if none of the above conditions are met
    )
  )

campaign_report_add_flow <- campaign_report_add_metric |>
  mutate(Flow = case_when(
    `Created By: Full Name` == "JotForm Integration Users" ~ "JotForm",
    `Created By: Full Name` == "ClickSend Site Site Guest User" ~ "SMS",
    `Created By: Full Name` %in% c("Public Site Guest User", "Email; Raw Data Upload") ~ "Email",
    (`Lead Channel` %in% c("Wechat", "Douyin") & !`Created By: Full Name` %in% c("JotForm Integration Users", "ClickSend Site Site Guest User", "Public Site Guest User")) |
      `Lead: Converted Opportunity: Account Name` == "WeCom Integration" ~ "Wecom",
    `Created By: Full Name` == "Revenue.io Admin" ~ "IB Call",
    TRUE ~ "IB Call"
  ))


campaign_report_combine <- campaign_report_add_flow |> 
  mutate(`Reject & Unqualified Reason` = coalesce(`Lead: Rejected Reason`, `Lead: Unqualified Reason`),
         Campaign = case_when(is.na(`Parent Campaign: Campaign Name`) ~ `Campaign Name`,
         TRUE ~ `Parent Campaign: Campaign Name`))

rm(list = setdiff(ls(),"campaign_report_combine"))

start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-05-31")

# Filter data for the past two weeks
campaign_data_past_two_weeks <- campaign_report_combine %>%
  filter(`Member First Associated Date` >= start_date & `Member First Associated Date` <= Sys.Date())

# Calculate total MQL and SQL counts
conversion_data <- campaign_data_past_two_weeks %>%
  group_by(Week, Flow, `Lead Channel`, `Campaign Name`, `Lead: Rejected Reason`, `Lead: Unqualified Reason`) %>%
  summarise(
    total_MQL = sum(`Funnel Stage` %in% c("MQL", "SQL")),
    total_SQL = sum(`Funnel Stage` == "SQL"),
    .groups = "drop"
  ) %>%
  mutate(conversion_rate = if_else(total_MQL > 0,
    paste0(round((total_SQL / total_MQL) * 100, 1), "%"),
    "-"
  ))

#########################################################################
#################    upload data to google sheet     ####################
#########################################################################

# Write conversion data to Google Sheets
write_sheet(conversion_data, ss = "1RKu9GHt2l0QQRvO4xmZE8JoiNUNJ4GxWywiX8PAjaMQ", sheet = "conversion_data")

#########################################################################
#################           conversion data          ####################
#########################################################################

# Convert Week to Date
conversion_data <- conversion_data %>%
  mutate(week_start = as.Date(sub(" - .*", "", Week), format = "%m/%d")) %>%
  mutate(conversion_rate = ifelse(conversion_rate == "-", NA, as.numeric(sub("%", "", conversion_rate)) / 100)) %>%
  mutate(calculated_conversion_rate = total_SQL / total_MQL) %>%
  mutate(conversion_rate = ifelse(is.na(conversion_rate), calculated_conversion_rate, conversion_rate))

rm(list = setdiff(ls(), "conversion_data"))


#########################################################################
########################   weekly data    ###############################
#########################################################################
# Summarize data by week_start if necessary
weekly_data <- conversion_data %>%
  group_by(week_start) %>%
  summarise(
    total_MQL = sum(total_MQL),
    total_SQL = sum(total_SQL),
    conversion_rate = total_SQL / total_MQL
  )

# Plot conversion rates over time with y-axis in percentage format
ggplot(weekly_data, aes(x = week_start, y = conversion_rate)) +
  geom_line() +
  geom_point() +
  labs(
    title = "MQL to SQL Conversion Rate Over Time",
    x = "Week Start Date",
    y = "Conversion Rate"
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#########################################################################
######################## flow data       ###############################
#########################################################################



# Aggregating data by Flow
flow_data <- conversion_data %>%
  group_by(week_start, Flow) %>%
  summarise(
    total_MQL = sum(total_MQL),
    total_SQL = sum(total_SQL),
    conversion_rate = mean(total_SQL / total_MQL, na.rm = TRUE)
  )





# Plot conversion rates over time by Flow
ggplot(flow_data, aes(x = week_start, y = conversion_rate, color = Flow)) +
  geom_line(na.rm = TRUE) +
  geom_point() +
  labs(
    title = "MQL to SQL Conversion Rate Over Time by Flow",
    x = "Week Start Date",
    y = "Conversion Rate"
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom", legend.title = element_blank())






#########################################################################
######################## channel data ###################################
#########################################################################



google_sheet_grouping <- read_sheet(ss = "1T29Vg97cuI2lX5FzEki51IsnbYTzjHse36Xfzl4It64", sheet = "Lead Channel Category")

channel_data <- conversion_data %>%
  group_by(week_start, `Lead Channel`) %>%
  summarise(
    total_MQL = sum(total_MQL),
    total_SQL = sum(total_SQL),
    conversion_rate = mean(total_SQL / total_MQL, na.rm = TRUE), .groups = "drop"
  ) %>%
  left_join(google_sheet_grouping, by = c("Lead Channel" = "lead_source")) %>%
  drop_na(lead_channel) %>%
  complete(
    week_start = seq.Date(min(week_start), max(week_start), by = "week"),
    lead_channel, fill = list(total_MQL = 0, total_SQL = 0, conversion_rate = NA)
  ) %>%
  arrange(lead_channel, week_start) |> 
  drop_na()

# Plot conversion rates over time by Lead Channel
ggplot(channel_data, aes(x = week_start, y = conversion_rate, color = lead_channel, group = lead_channel)) +
  geom_line() + # Connect lines even if there are missing values
  geom_point() +
  labs(
    title = "MQL to SQL Conversion Rate Over Time by Lead Channel",
    x = "Week Start Date",
    y = "Conversion Rate"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%d") + # Show month and day
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~lead_channel, scales = "free_y") +
  theme(legend.position = "none", legend.title = element_blank())



#########################################################################
########################campaign data  ###############################
#########################################################################

campaign_rejected <- conversion_data |> 
  filter(!is.na())


# Aggregating data by Campaign
campaign_data <- campaign_rejected %>%
  mutate(campaign_clean = gsub("_[^_]+$", "", `Campaign Name`)) %>%
  group_by(week_start,`Lead: Rejected Reason`, `Lead: Unqualified Reason` ,`Campaign Name`) %>%
  summarise(
    count = n()
  ) 

# Plotting the conversion rates over time by campaign
ggplot(campaign_data, aes(x = week_start, y = count, color = `Lead: Unqualified Reason`)) +
  geom_line() +
  geom_point() +
  facet_wrap(~campaign_clean, scales = "free_y") + # Facet by campaign
  labs(
    title = "MQL to SQL Conversion Rate Over Time by Campaign",
    x = "Week Start Date",
    y = "Conversion Rate"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%d") + # Show month and day
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") # Remove legend to avoid clutter