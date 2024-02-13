library(tidyverse)


folder_path <- '~/Performance Analysis/Form Submission/2024/Jan8_to_Jan14/Data'
filenames <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

combo_data <- purrr::map_df(filenames, 
                            ~read.csv(.x, stringsAsFactors = FALSE) %>% mutate(filename = .x))


combo_data <- combo_data %>% 
  mutate(
    channel = case_when(
      str_detect(filename, "(?i)SEM|Blog Pages Contact us") ~ "SEM",
      str_detect(filename, "(?i)SEO|Contact us") ~ "SEO",
      str_detect(filename, "(?i)sMS") ~ "SMS",
      str_detect(filename, "(?i)Brizo|(?i)DM") ~ "DM"))

combo_data <- combo_data |> 
  rename(channel = utm_medium)


combo_data |> 
  group_by(channel) |> 
  summarize(form_fill_numbers=n()) |> 
  arrange(desc(form_fill_numbers)) -> summary_table

write_csv(summary_table, "form_fill_counts_by_channel.csv")
write_csv(combo_data,"all_form_fill.csv")