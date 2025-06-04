library(tidyverse)
library(salesforcer)
library(yaml)
library(httr)
library(googlesheets4)
library(keyring)
library(janitor)
library(here)

setwd(here("campaign-analysis"))
source("campaignmembers_opportunities.R")

################### loading data ###########################

campaign_information <-
  read_sheet(
    ss = "1IYMZ0a-yL7LQUgCsy6EZnSjHLBKXC48P9QRE9Kh8Jzo",
    sheet = "DM",
    range = "A:E"
  )
