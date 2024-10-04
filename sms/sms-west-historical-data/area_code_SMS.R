source("/Users/yukachen/marketing-operation/sms/sms-west-historical-data/sms_historical_data_west_area_code.R")

library(ggplot2)
library(maps)

# Load and map state names correctly, ensure we ignore "District of Columbia"
state_aggregated_table_sms$state <- tolower(state.name[match(state_aggregated_table_sms$state_abbreviation, state.abb)])

# Load US map data
us_map <- map_data("state")

# Check the column names in the original data frame
print(colnames(state_aggregated_table_sms))

add_missing_states <- function(missing_states, data, template_row) {
  for (state in missing_states) {
    if (!state %in% data$state) {
      template_row$state <- state
      data <- rbind(data, template_row)
    }
  }
  return(data)
}

# Example usage:
# missing_states is the list of states that are missing
# state_aggregated_table_sms is your dataset
# template_row is your predefined row with default values

# Find missing states
missing_states <- setdiff(us_map$region, unique(state_aggregated_table_sms$state))

# Create the template row (0 for MEL, MQL, SQL, Onboarded)
template_row <- setNames(data.frame(matrix(ncol = ncol(state_aggregated_table_sms), nrow = 1)), colnames(state_aggregated_table_sms))
template_row$MEL <- 0
template_row$MQL <- 0
template_row$SQL <- 0
template_row$Onboarded <- 0

# Add missing states to the dataset using the function
state_aggregated_table_sms <- add_missing_states(missing_states, state_aggregated_table_sms, template_row)


# Merge the map data with your dataset
map_data <- merge(us_map, state_aggregated_table_sms, by.x = "region", by.y = "state", all.x = TRUE)

# Fill missing MEL values with 0 for missing states
map_data$MEL[is.na(map_data$MEL)] <- 0







# Save MEL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = MEL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#e0b3ff", high = "#6317cb", na.value = "grey80") +
  labs(title = "MEL Heat Map by Area Code State (SMS)", fill = "MEL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/_area_code_MEL_Heat_Map_sms.png", width = 10, height = 8)

# Save MQL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = MQL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#e0b3ff", high = "#6317cb", na.value = "grey80") +
  labs(title = "MQL Heat Map by Area Code State (SMS)", fill = "MQL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/_area_code_MQL_Heat_Map_sms.png", width = 10, height = 8)

# Save SQL Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = SQL)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#e0b3ff", high = "#6317cb", na.value = "grey80") +
  labs(title = "SQL Heat Map by Area Code State (SMS)", fill = "SQL") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/_area_code_SQL_Heat_Map_sms.png", width = 10, height = 8)

# Save Onboarded Heat Map
ggplot(map_data, aes(x = long, y = lat, group = group, fill = Onboarded)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "#e0b3ff", high = "#6317cb", na.value = "grey80") +
  labs(title = "Onboarded Heat Map by Area Code State (SMS)", fill = "Onboarded") +
  theme_minimal()
ggsave("sms/sms-west-historical-data/_area_code_Onboarded_Heat_Map_sms.png", width = 10, height = 8)



