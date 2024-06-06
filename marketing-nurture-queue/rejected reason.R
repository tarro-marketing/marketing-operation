library(tidyverse)
library(googlesheets4)
library(keyring)

client_secret_path <- keyring::key_get(service = "googlesheets4", username = "client_secret_path")
email <- keyring::key_get(service = "googlesheets4", username = "email")

# Configure and authenticate using retrieved credentials
gs4_auth_configure(path = client_secret_path)
gs4_auth(email = email, cache = TRUE)


mnq <- read_csv("marketing-nurture-queue/mkt-nurture-que.csv")

sum(is.na(mnq$`Rejected Reason`))




mnq_has_reason = mnq |> 
  filter(!is.na(`Rejected Reason`))

rejected_reason_summary <- mnq_has_reason |> 
  group_by(`Rejected Reason`) |> 
  summarise(Counts = n()) |> 
  arrange(desc(Counts)) 

#sheet_append(rejected_reason_summary, ss = "1KYKqlVJ-ZFEteVgX4B7ycsSqwOIcU7gEH2mGjlzBEzg", sheet = "Summary")

mnq_rejcted_date_90_days <- mnq %>%
  mutate(`Rejected Time` = mdy_hm(`Rejected Time`, tz = "America/New_York"),
         `Create Date` = mdy(`Create Date`, tz = "America/New_York")) |> 
  mutate(datediff_rejected_date = round(as.numeric(difftime(today(), `Rejected Time`, units = "days"))),
         datediff_created_date =  round(as.numeric(difftime(today(), `Create Date`, units = "days")))) |> 
  select(`State Group`, datediff_rejected_date, datediff_created_date, everything()) 


rejected_reason_summary <- mnq_rejcted_date_90_days |> 
  filter(datediff_created_date > 90)

sumaary <- rejected_reason_summary |> 
  group_by(`Rejected Reason`) |> 
  summarise(Counts = n()) |> 
  arrange(desc(Counts)) 




sheet_append(sumaary, ss = "1KYKqlVJ-ZFEteVgX4B7ycsSqwOIcU7gEH2mGjlzBEzg", sheet = "Summary")


write_sheet(mnq_rejcted_date_90_days, ss = "1XU-Ndmtf54w5IAg6E_7eeGCJ_t2e-Q-J16DK8LafUPg",sheet ="MNQ - ALL (Updated)")

write_csv(mnq_rejcted_date_90_days,"marketing-nurture-queue/mnq.csv")
