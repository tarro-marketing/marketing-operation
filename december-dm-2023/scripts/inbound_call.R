library(tidyverse)

inbound_call =  read_csv("Raw-Data/inbound_call_notes.csv")

inbound_call <- inbound_call %>%
  drop_na(Date) |> 
  mutate(Date = try(as.Date(Date, format = "%m/%d/%Y"), silent = TRUE)) %>%
  filter(Date >= as.Date('2023-12-01') & Date <= as.Date('2024-01-05') &
           `Extension Number` 
         %in% c(8885200811, 8882353802, 8885753251, 8882353718, 8885200811) &
           `Lead Status` != "Internal") %>%
  mutate(
    language_test = ifelse(`Extension Number` == 8882353718, "English Version",
                              ifelse(`Extension Number` == 8885200811 | `Extension Number` == 8882353802, "Chinese Version", NA)),
    drop_group = ifelse(`Extension Number` == 8885200811, "Drop 1 - US",
                        ifelse(`Extension Number` == 8882353802, "Drop 1 - Canada",
                               ifelse(`Extension Number` == 8885753251, "Drop 2 - US", NA)))
  )




 write_csv(inbound_call,"Clean-Data/clean_inbound_call.csv")
 
 
 