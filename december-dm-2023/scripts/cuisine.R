library(tidyverse)

brizo_list <- read_csv("Clean-Data/brizo_list.csv", 
                       col_types = cols(`Snowball Map` = col_character())) |> 
  rename(snowball_id = `Snowball Map`) |> 
  mutate(snowball_id = as.character(snowball_id),  # Ensure snowball_id is a character
         snowball_id = str_pad(snowball_id, width = 5, pad = "0"))
  
unique_scan <- read_csv("Clean-Data/unique_scan.csv") |> 
  filter(list_source == "Brizo")

all_info <- unique_scan |> 
  left_join(brizo_list, by = "snowball_id") |> 
  select(Cuisines,`Price Range`, snowball_id, Date, `Page location`, everything() )


# write_csv(all_info, "cuisines_type/qr_scan_w_brizo_info.csv", na = "")


all_info_corrected <- all_info %>%
  group_by(Cuisines) |> 
  summarise(Count=n()) |> 
  # Split the 'Cuisines' column into a list of cuisines
  mutate(Cuisines = str_split(Cuisines, ";\\s*"),
         # Calculate the number of cuisines to determine how to split the count
         NumCuisines = map_dbl(Cuisines, length)) %>%
  # Calculate the count per cuisine by dividing the total count by the number of cuisines
  rowwise() %>%
  mutate(SplitCount = Count / NumCuisines) %>%
  # Expand the data frame so each cuisine gets its own row, along with the evenly split count
  unnest(Cuisines) %>%
  # Now that we have one row per cuisine per original entry, drop the now unnecessary columns
  select(-NumCuisines) %>%
  # Group by the individual cuisines and sum up the split counts for the final total count per cuisine
  group_by(Cuisines) %>%
  summarize(TotalCount = sum(SplitCount)) %>%
  ungroup() |> 
  mutate(TotalCount=round(TotalCount, digits = 2))

#   write_csv("cuisines_type/summarytable_qr_scan.csv")


december_form_fill <- read_csv("cuisines_type/december_form_fill.csv", na = "") |> 
  rename(snowball_id = `Snowball`)

form_fill_brizo <- december_form_fill |> 
  left_join(brizo_list, by = "snowball_id") |> 
  filter(str_detect(Referrer,"(?i)brizous")) |> 
  rename(Cuisines_FormFill = Cuisines.x,
         Cuisines_Brizo = Cuisines.y,
         Date = `Created Date`) |> 
  mutate(Cuisines = case_when(is.na(Cuisines_FormFill)~Cuisines_Brizo,
                              TRUE ~ Cuisines_FormFill)) |> 
  select(Cuisines,-Cuisines_FormFill,-Cuisines_Brizo, `Price Range`, snowball_id, Date, Referrer, everything()) |> 
  write_csv("cuisines_type/form_fill_brizo.csv", na = "") 





form_fill_brizo_info <- form_fill_brizo %>%
  group_by(Cuisines) |> 
  summarise(Count=n()) |> 
  mutate(Cuisines = str_split(Cuisines, ";\\s*"),
         NumCuisines = map_dbl(Cuisines, length)) %>%
  rowwise() %>%
  mutate(SplitCount = Count / NumCuisines) %>%
  unnest(Cuisines) %>%
  select(-NumCuisines) %>%
  group_by(Cuisines) %>%
  summarize(TotalCount = sum(SplitCount)) %>%
  ungroup() |> 
  mutate(TotalCount=round(TotalCount, digits = 2)) |> 
  write_csv("cuisines_type/summary_table_form_fill_brizo.csv", na = "")






form_fill_all <- december_form_fill |> 
  left_join(brizo_list, by = "snowball_id") |> 
  rename(Cuisines_FormFill = Cuisines.x,
         Cuisines_Brizo = Cuisines.y,
         Date = `Created Date`) |> 
  mutate(Cuisines = case_when(is.na(Cuisines_FormFill)~Cuisines_Brizo,
                              TRUE ~ Cuisines_FormFill)) |> 
  select(Cuisines,Cuisines_FormFill,Cuisines_Brizo, `Price Range`, snowball_id, Date, Referrer, everything()) |> 
  write_csv("cuisines_type/form_fill_all.csv", na="") 




