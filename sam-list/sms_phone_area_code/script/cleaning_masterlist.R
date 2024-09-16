library(tidyverse)


Masterlist1 <- read_csv("raw_data_added/0126Masterlist A.csv")
Masterlist2 <- read_csv("raw_data_added/0127MasterlistB.csv")
Masterlist3 <- read_csv("raw_data_added/0128Masterlist B.csv")
Masterlist4 <- read_csv("raw_data_added/0128MasterlistA.csv")


Masterlist <- rbind(Masterlist1,Masterlist2,Masterlist3,Masterlist4)


write_csv(Masterlist,"raw_data_added/masterlist.csv")
