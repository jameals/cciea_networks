library(here)
library(tidyverse)
library(foreign)
library(lubridate)

## output directory for confidential data
out_conf_dir <- "/Users/jameal.samhouri/Documents/CCIEA Networks/processed/"

tix_2019 <- read_csv(paste0(out_conf_dir, "/fish_tickets_crab2019_processed_for_networks.csv"))
glimpse(tix_2019)


tix_2019 %>%
  filter(
    agid == "O"
  ) %>%
  filter(
    spid == "DCRB"
  ) %>%
  summarise(
    tot_rev = sum(revenue),
    tot_adj_rev = sum(adj_revenue)
  )