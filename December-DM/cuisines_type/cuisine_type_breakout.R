library(tidyverse)


# Define a function to apply your series of operations
process_cuisines <- function(data) {
  data %>%
    group_by(Cuisines) %>%
    summarise(Count = sum(count), .groups = 'drop') %>%
    mutate(Cuisines = str_split(Cuisines, ";\\s*"),
           NumCuisines = map_dbl(Cuisines, length)) %>%
    rowwise() %>%
    mutate(SplitCount = Count / NumCuisines) %>%
    unnest(Cuisines) %>%
    select(-NumCuisines) %>%
    group_by(Cuisines) %>%
    summarize(TotalCount = sum(SplitCount), .groups = 'drop') %>%
    ungroup() %>%
    mutate(TotalCount = round(TotalCount, digits = 2))
}



mel = tibble(Cuisines=c("Cajun","Chinese","Chinese; Cajun","Chinese; Japanese","Chinese; Other","Chinese; Thai","Japanese","Other","Wings"),
count = c(11,52,1,5,1,1,28,8,1))

mel_counts <- process_cuisines(mel)

mql = tibble(
  Cuisines=c("Cajun","Chinese","Chinese; Cajun","Chinese; Japanese","Chinese; Other","Japanese","Other","Wings"),
  count=c(2,35,1,4,1,10,8,1))



mql_counts <- process_cuisines(mql)


sql = tibble(Cuisines=c("Cajun","Chinese","Chinese; Cajun","Chinese; Japanese",
"Chinese; Other","Japanese","Other","Wings"),
             count=c(2,18,1,2,1,7,4,1))


sql_counts <- process_cuisines(sql)

cw = tibble(Cuisines=c("Chinese","Chinese; Other"),
            count=c(3,1))

cw_counts <- process_cuisines(cw)




inbound_call= tibble(Cuisines=c("American Chinese","Asian","buffet","Cafe","Cajun","Chinese",
         "Chinese; Japanese","Chinese; buffet","Chinese; Cajun",
         "Chinese;Other","Hawaiian","Japanese","Japanese; Asian",
         "NA","Sichuan","West"),
                     count=c(10,1,1,1,2,46,9, 2,1, 2,3,20,1,4,1,1))

inbound_call_count = process_cuisines(inbound_call)
