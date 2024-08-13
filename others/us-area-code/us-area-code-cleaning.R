library(tidyverse)

phone_area_code_cities <- read_csv("~/PerformanceMarketing/list-matching/us-area-code/phone_area_code_cities.csv")


phone_area_code_cities |> 
  distinct(`Area Code`, .keep_all = TRUE) |> 
  rename(State_Area = `State Abbreviation`,
         State_Name = State,
         Area_Code = `Area Code`) -> area_code


new_row407 <- data.frame(
  "Area_Code" = 407,
  "State_Name" = "Georgia",
  "State_Area" = "GA",
  "Country" = "United States"
)

new_row250<- data.frame(
  "Area_Code" = 250,
  "State_Name" = "British Columbia",
  "State_Area" = "BC",
  "Country" = "Canada"
)

area_code = rbind(area_code, new_row407,new_row250)


area_code=area_code |> 
  distinct(`Area_Code`, .keep_all = TRUE)

write_csv(area_code, "~/PerformanceMarketing/list-matching/us-area-code/area_code.csv")
