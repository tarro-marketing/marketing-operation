library(tidyverse)
library(googlesheets4)


gs4_auth_configure(path = "pw/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")
gs4_auth(email = "youjia.chen@wondersco.com", cache = TRUE)

# Read data from Google Sheet
restaurants <- read_sheet(ss = "1SrZOxlHDxLAjS9A6YJAv0kmzNrLkZvx3m5KAK_UB2Bo", sheet = "Restaurant")

# 
# colnames <- colnames(restaurants)
# 
# 
# restaurants %>%
#   filter(`Restaurants Status` == "active") %>%
#   group_by(`Restaurants State`) %>%
#   summarize(`Number of Clients` = n()) %>%
#   arrange(desc(`Number of Clients`))


write_csv(restaurants,"data/restaurants.csv")


restaurants %>%
  group_by(`Restaurants State`) %>%
  summarise(Average_GMV = mean(`Restaurants GMV Voice Platform Sales`, na.rm = TRUE), .groups = 'drop')
