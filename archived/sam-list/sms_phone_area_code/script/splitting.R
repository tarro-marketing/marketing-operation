library(tidyverse)

phone_total <- read_csv("clean_data/phone_total.csv")

group1 <- c("SC", "WI", "MS", "IN", "PA", "KY", "MI", "NJ", "CO", "ID", "NM", "IA", "CA", "MN", "VT", "KS", "NE")

group2 <- c("MD", "DE", "WY", "OH", "NV", "TN", "WV", "AL", "UT", "IL", "MT", "NY", "OK", "NH", "TX", "SD")

group3 <- c("FL", "AR", "RI", "CT", "NC", "DC", "GA", "VA", "MA", "WA", "LA", "MO", "OR", "AZ", "ND", "ME")


group1_data <- phone_total %>% filter(State %in% group1)
group2_data <- phone_total %>% filter(State %in% group2)
group3_data <- phone_total %>% filter(State %in% group3)

write_csv(group1_data, "clean_data/group_splits/group1.csv")
write_csv(group2_data, "clean_data/group_splits/group2.csv")
write_csv(group3_data, "clean_data/group_splits/group3.csv")


rbind(group1_data, group2_data, group3_data) -> groups_data


write_csv(groups_data, "clean_data/sms_list.csv")
