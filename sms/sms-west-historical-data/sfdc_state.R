source("/Users/yukachen/marketing-operation/sms/sms-west-historical-data/sms_historical_data_west_area_code.R")

library(ggplot2)
library(maps)

# Load and map state names correctly, ensure we ignore "District of Columbia"
sfdc_state_aggregated_table$state <- tolower(state.name[match(sfdc_state_aggregated_table$state_abbreviation, state.abb)])

# Load US map data
us_map <- map_data("state")

# Check the column names in the original data frame
print(colnames(sfdc_state_aggregated_table))

# Create a template row with 0 for MEL, MQL, SQL, Onboarded, and NA for other columns
template_row <- setNames(data.frame(matrix(ncol = ncol(sfdc_state_aggregated_table), nrow = 1)), colnames(sfdc_state_aggregated_table))
template_row$MEL <- 0
template_row$MQL <- 0
template_row$SQL <- 0
template_row$Onboarded <- 0

# Add "north dakota" to the dataset
template_row$state <- "north dakota"
if (!"north dakota" %in% sfdc_state_aggregated_table$state) {
  sfdc_state_aggregated_table <- rbind(sfdc_state_aggregated_table, template_row)
}

# Repeat for "district of columbia"
template_row$state <- "district of columbia"
if (!"district of columbia" %in% sfdc_state_aggregated_table$state) {
  sfdc_state_aggregated_table <- rbind(sfdc_state_aggregated_table, template_row)
}

# Merge the map data with your dataset
map_data <- merge(us_map, sfdc_state_aggregated_table, by.x = "region", by.y = "state", all.x = TRUE)

# Fill missing MEL values with 0 for missing states
map_data$MEL[is.na(map_data$MEL)] <- 0

# Save MEL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = MEL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#D3D3D3", high = "#6317cb", na.value = "white") +
  labs(title = "MEL Heat Map by State", fill = "MEL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/MEL_Heat_Map.png", width = 10, height = 8)

# Save MQL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = MQL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#D3D3D3", high = "#6317cb", na.value = "white") +
  labs(title = "MQL Heat Map by State", fill = "MQL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/MQL_Heat_Map.png", width = 10, height = 8)

# Save SQL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = SQL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#D3D3D3", high = "#6317cb", na.value = "white") +
  labs(title = "SQL Heat Map by State", fill = "SQL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/SQL_Heat_Map.png", width = 10, height = 8)

# Save Onboarded Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = Onboarded)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#D3D3D3", high = "#6317cb", na.value = "white") +
  labs(title = "Onboarded Heat Map by State", fill = "Onboarded") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/Onboarded_Heat_Map.png", width = 10, height = 8)



