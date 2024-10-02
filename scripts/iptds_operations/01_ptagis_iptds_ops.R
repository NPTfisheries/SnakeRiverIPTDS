# -----------------------
# Author: Mike Ackerman
# Purpose: Summarize start/end dates for each IPTDS including the years each was in operation
#   based on info from PTAGIS.
# 
# Created: April 29, 2024
#   Last Modified: October 2, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)
library(here)

# load configuration files
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240927.rda") ; rm(configuration, flowlines, crb_sites_sf, parent_child)

# list of snake river interrogation sites
sr_int_sites = sr_site_pops %>%
  filter(site_type == "INT") %>%
  pull(site_code)

# snake river interrogation site metadata
sr_iptds_meta = queryInterrogationMeta() %>%
  clean_names() %>%
  filter(site_code %in% sr_int_sites) %>%
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
         longitude)

# summarize iptds operational dates
iptds_op_dates = sr_iptds_meta %>%
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
  # these sites don't have first_date; grabbed from PTAGIS
  mutate(first_date = if_else(site_code == "BED", as.Date("2024-02-15"), first_date)) %>%
  mutate(first_date = if_else(site_code == "UG3", as.Date("2024-05-23"), first_date)) %>%
  mutate(first_date = if_else(site_code == "UG4", as.Date("2024-05-23"), first_date)) %>%
  mutate(first_date = if_else(site_code == "COU", as.Date("2024-02-28"), first_date))

# sequence of years that each site was in operation
site_yrs = iptds_op_dates %>%
  group_by(site_code) %>%
  summarise(year = list(seq(min(year(first_date)), max(year(last_date)), by = 1))) %>%
  unnest(year)

# a function to generate a sequence of dates for a given year range
gen_dates = function(start_year, end_year) {
  seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
}

# expand the data frame to include all dates for each site code
ptagis_ops = iptds_op_dates %>%
  # expand the data frame to include all dates from first_year to last_year for each site_code
  rowwise() %>%
  mutate(date = list(gen_dates(first_year, last_year))) %>%
  unnest(date) %>%
  # was the site operational for a given day, according to ptagis
  mutate(op = (date >= first_date & date <= last_date)) %>%
  select(site_code,
         date,
         operational = op) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  # set chinook spawning season (June 1 - September 15)
  mutate(chnk = case_when(month == 6 & day >= 1 |
                            month %in% 7:8 |
                            month == 9 & day <= 15 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  # set steelhead spawning season (March 1 - May 31)
  mutate(sthd = case_when(month %in% 3:5 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  # set coho spawning season (October 1 - December 31)
  mutate(coho = case_when(month %in% 10:12 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  select(-month, -day) %>%
  # summarize the proportion of days that each site was in operation by site_code and year, according to ptagis
  group_by(site_code, year) %>%
  summarize(
    p_days_ptagis_yr = sum(operational) / n(),
    chnk_p_days = sum(operational & chnk) / sum(chnk),
    sthd_p_days = sum(operational & sthd) / sum(sthd),
    coho_p_days = sum(operational & coho) / sum(coho),
  ) %>%
  # re-format data frame to longer format
  pivot_longer(cols = c(chnk_p_days, sthd_p_days, coho_p_days),
               names_to = "species",
               values_to = "p_days_ptagis_run") %>%
  mutate(species = case_when(
    species == "chnk_p_days" ~ "chnk",
    species == "sthd_p_days" ~ "sthd",
    species == "coho_p_days" ~ "coho"
    )) %>%
  # finally, clean up the resulting data frame
  select(species,
         site_code,
         year,
         p_days_ptagis_run,
         p_days_ptagis_yr)

# save iptds operational dates data frame
save(site_yrs,
     ptagis_ops,
     file = here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))

# which iptds sites are in biologic?
source(here("keys/biologic_login.txt"))
#remotes::install_github("ryankinzer/fisheR")
library(fisheR)
biologic_login(email, password)
biologic_sites = get_biologic_sites() # all biologic sites that i have permission for

snake_biologic_sites = sr_iptds_meta %>%
  filter(site_code %in% biologic_sites) %>%
  pull(site_code)

### END SCRIPT