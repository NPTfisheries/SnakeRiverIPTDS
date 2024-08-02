# -----------------------
# Author: Mike Ackerman
# Purpose: Summarize start/end dates for each IPTDS including the years each was in operation
#   based on info from PTAGIS.
# 
# Created: April 29, 2024
#   Last Modified: August 2, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)
library(here)

# query all interrogation site metadata
iptds_meta = queryInterrogationMeta() %>%
  clean_names()

# snake river interrogation site metadata
sr_iptds_meta = iptds_meta %>%
  filter(str_starts(rkm, "522"),
         site_type %in% c("Instream Remote Detection System",
                          "Adult Fishway"),
         # removes the four Snake River dams
         !operations_organization_code == "PSMFC") %>%
  select(site_code,
         name,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         last_file_opened_on,
         last_file_closed_on,
         operation_period,
         operations_organization_code,
         rkm,
         site_type,
         latitude,
         longitude) ; rm(iptds_meta)

# summarize iptds operational dates
iptds_ops = sr_iptds_meta %>%
  # get the latest date for each site
  group_by(site_code) %>%
  mutate(last_date = max(last_date, last_file_opened_on, last_file_closed_on, na.rm = T)) %>%
  ungroup() %>%
  select(site_code,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date) %>%
  mutate(first_date = as.Date(first_date),
         last_date = as.Date(last_date)) %>%
  # These sites don't have first_date and/or last_date; grabbed from PTAGIS
  mutate(first_date = if_else(site_code == "BED", as.Date("2024-02-15"), first_date)) %>%
  mutate(first_date = if_else(site_code == "UG3", as.Date("2024-05-23"), first_date)) %>%
  mutate(last_date  = if_else(site_code == "UG3", as.Date(Sys.Date()),   last_date))  %>%
  mutate(first_date = if_else(site_code == "UG4", as.Date("2024-05-23"), first_date)) %>%
  mutate(first_date = if_else(site_code == "COU", as.Date("2024-02-28"), first_date))

# sequence of years that each site was in operation
site_yrs = iptds_ops %>%
  group_by(site_code) %>%
  summarise(year = list(seq(min(year(first_date)), max(year(last_date)), by = 1))) %>%
  unnest(year)

# summarize the years and days that each iptds was installed per year
iptds_ops %<>%
  left_join(site_yrs, by = "site_code") %>%
  group_by(site_code, year) %>%
  summarise(
    days = sum(pmax(0, pmin(last_date, as.Date(paste0(year, "-12-31"))) - pmax(first_date, as.Date(paste0(year, "-01-01"))) + 1))
  ) %>%
  spread(year, days, fill = 0) %>%
  left_join(iptds_ops, by = "site_code") %>%
  select(site_code,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         everything())

# save iptds operational dates data frame
save(iptds_ops,
     site_yrs,
     file = here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))

# which iptds sites are in biologic?
source(here("keys/biologic_login.txt"))
library(fisheR)
biologic_login(email, password)
biologic_sites = get_biologic_sites() # all biologic sites that i have permission for

snake_biologic_sites = sr_iptds_meta %>%
  filter(site_code %in% biologic_sites) %>%
  pull(site_code)

### END SCRIPT