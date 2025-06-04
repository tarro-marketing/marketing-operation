library(tidyverse)


october_leads <- read_csv("recalculation/report1706143422219.csv")



october_leads <- october_leads |> 
  drop_na(`Latest Campaign`) |> 
  mutate(`Created Date` = mdy(`Created Date`),
         `Mobile - Primary` = str_replace_all(str_replace_all(`Mobile - Primary`, "-", ""), "[^0-9]", ""),
         `Business Phone`= str_replace_all(str_replace_all(`Business Phone`, "-", ""), "[^0-9]", ""),
         flow = case_when(`Created By` %in% c("JotForm Integration User", "Carlito Academia") ~ "webflow",
                          TRUE ~ "inbound call"),
         types = case_when(`Created By` == "JotForm Integration User" ~ "jotform",
                           `Created By` == "Carlito Academia" ~ "qr scan",
                           TRUE ~ "inbound call"),
         mql = case_when(`Menu Type` != "" & `State/Province (text only)` != "" 
                         & (`Created By` == "JotForm Integration User" | `Lead Status` %in% c("Converted", "AE Assigned")) & 
                           !(`Unqualified Reason` %in% c("Current Client", "Duplicate", "Not a Restaurant")) ~ "TRUE",
                         TRUE ~ "FALSE"),
         sql = case_when(`Opportunity ID`!="" ~ "TRUE",
                         TRUE ~ "FALSE"),
         cw = case_when(Stage == "Closed Won" | Stage == "Onboarded" ~ "TRUE",
                        TRUE ~ "FALSE"),
         dm_drop = case_when(`Created Date`<=mdy("10/18/2023") ~ "DM1",
                             TRUE ~ "DM2"))


write_csv(october_leads,"recalculation/october_dm_leads_updated.csv", na="")


october_leads |> 
  group_by(dm_drop) |> 
  summarise(counts=n()) |> 
  pivot_wider(names_from = dm_drop, values_from = counts) |> 
  mutate(lead_category = "MEL") -> MEL


october_leads |> 
  filter(mql == TRUE) |> 
  group_by(dm_drop) |> 
  summarise(counts=n()) |> 
  pivot_wider(names_from = dm_drop, values_from = counts) |> 
  mutate(lead_category = "MQL") -> MQL

october_leads |> 
  filter(mql == TRUE& sql == TRUE) |> 
  group_by(dm_drop) |> 
  summarise(counts=n()) |> 
  pivot_wider(names_from = dm_drop, values_from = counts) |> 
  mutate(lead_category = "SQL") -> SQL

october_leads |> 
  filter(mql == TRUE&cw == TRUE) |> 
  group_by(dm_drop) |> 
  summarise(counts=n()) |> 
  pivot_wider(names_from = dm_drop, values_from = counts) |> 
  mutate(lead_category = "CW",
         DM2="0") -> CW



rbind(MEL, MQL, SQL, CW) -> summary_table

write_csv(summary_table,"recalculation/summary_table_october_updates.csv")
