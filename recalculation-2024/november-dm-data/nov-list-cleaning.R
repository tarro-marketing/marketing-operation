library(tidyverse)
library(googlesheets4)


gs4_auth_configure(path = "C:/Users/skt/Documents/API/client_secret_1063101091245-a8k1e24l8h2aukvjthrbq0gbneu878su.apps.googleusercontent.com.json")

################################## Brizo #######################################

brizo_us <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Brizo - US (1-47669)")
brizo_can <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Brizo - CAN (47670-54533)")

cols_brizo_us <- colnames(brizo_us)
cols_brizo_can <- colnames(brizo_can)
common_cols <- Reduce(intersect, list(cols_brizo_us, cols_brizo_can))

diff_cols_can_not_in_us <- setdiff(cols_brizo_can, cols_brizo_us)
diff_cols_us_not_in_can <- setdiff(cols_brizo_us, cols_brizo_can)

brizo_us <- brizo_us |>
  select(names(brizo_can)) |>
  mutate(`Zip/Postal Code` = as.character(`Zip/Postal Code`))

brizo_can <- brizo_can |>
  mutate(`Snowball Map` = as.character(`Snowball Map`))

brizo <- bind_rows(brizo_can, brizo_us)
rm(cols_brizo_us, cols_brizo_can, common_cols, brizo_can, brizo_us, diff_cols_can_not_in_us, diff_cols_us_not_in_can)



############################### printshop ######################################


printshop_us <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Print Shop - US (54534-88975)")
printshop_can <- read_sheet("1SyrXpjN3_mzWUGTgfijyVqVxqQev8BIitxJoRy1gV70", sheet = "Print Shop - CAN (88976-89501)")



cols_printshop_us <- colnames(printshop_us)
cols_printshop_can <- colnames(printshop_can)

common_cols <- Reduce(intersect, list(cols_printshop_can, cols_printshop_us))
