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

# weird albacore trip
tix_2019 %>%
  filter(
    trip_id == 605944705
  )

tix_2019[which(tix_2019$trip_id == 605944705),]$revenue

# check out Astoria, based on email from Dan Holland on 2/9/21
tix_2019 %>%
  filter(
    IOPAC == "Astoria"
  ) %>%
  filter(
    SPGRPN2 == 1 # DTS
  ) %>%
  summarise(
    tot_rev = sum(revenue),
    tot_adj_rev = sum(adj_revenue),
    num_vessels = length(unique(drvid))
  )

