# -----------------------
# Author: Mike Ackerman
# Purpose: Compile and summarise IPTDS operational dates and virtual test
#   tag data for all Snake River sites from PTAGIS.
# 
# Created: May 6, 2024
#   Last Modified: May 7, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(PITcleanr)
library(janitor)
library(fisheR)

#------------------------------
# Part 1: IPTDS Operational Times

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
  mutate(last_date = max(last_date, last_file_opened_on, na.rm = T)) %>%
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
  # BED doesn't have a first_date, grabbed from PTAGIS
  mutate(first_date = if_else(site_code == "BED", as.Date("2024-02-15"), first_date)) 

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
         everything()) ; rm(site_yrs)

# which iptds sites are in biologic?
source(here("keys/biologic_login.txt"))
biologic_login(email, password)
biologic_sites = get_biologic_sites() # biologic sites that i have permission for

sr_sites_in_biologic = sr_iptds_meta %>%
  filter(site_code %in% biologic_sites) %>%
  pull(site_code)

#------------------------------
# Part 2: Virtual Test Tags

# read in and combine virtual test tag data
vtt_df = list.files(path = here("output/virtual_test_tags/"),
                    pattern = "\\.rds$",
                    recursive = T,
                    full.names = T) %>%
  map(readRDS) %>%
  map_dfr(bind_rows) %>%
  mutate(year = year(time_stamp)) 

# summarise the number of vtt reads per hour by site, year, and antenna
vtt_per_hr = vtt_df %>%
  # filter to WEB, 2011 for now
  # filter(site_code == "WEB",
  #        year == 2011) %>%
  # round each time stamp down to the nearest hour
  mutate(time_stamp = floor_date(time_stamp, "hour")) %>%
  # count the number of vtt reads within each hour, per antenna
  group_by(site_code,
           year,
           transceiver_id,
           antenna_id,
           time_stamp) %>%
  summarise(vtt_reads = n(),
            .groups = "drop")

# summarise the proportion of hours within each day each transceiver had vtt reads, averaged across antennas by site, year, and day
vtt_per_day = vtt_per_hr %>%
  # filter to ESS, 2023 for now
  # filter(site_code == "LAP",
  #        year == 2023) %>%
  mutate(date = as.Date(time_stamp)) %>%
  # summarise the p of hours each day that each antenna had vtt_reads
  group_by(site_code,
           year,
           date,
           transceiver_id,
           antenna_id) %>%
  summarise(p_vtt = sum(vtt_reads > 0) / 24,
            .groups = "drop") %>%
  # add in antennas that had no vtt_reads for a given site, year, and date
  group_by(site_code, year) %>%
  complete(date, transceiver_id,
           fill = list(p_vtt = 0)) %>%
  # average p_vtt within each transceiver_id
  group_by(site_code,
           year,
           date,
           transceiver_id) %>%
  summarise(p_vtt = mean(p_vtt),
            .groups = "drop")

# example plot for a single site and year
site = "BBA"
yr = 2023
vtt_per_day %>%
  filter(site_code == site,
         year == yr) %>%
  group_by(site_code, year, transceiver_id) %>%
  summarise(.groups = "keep") %>%
  expand(date = format(seq(ymd("1900-01-01"),
                           ymd("1900-05-31"),
                           by = "days"),
                       "%m-%d")) %>%
  mutate(date = as.Date(paste0(as.character(year), "-", date))) %>%
  left_join(vtt_per_day,
            by = c("site_code", "year", "transceiver_id", "date")) %>%
  # replace NAs with 0
  mutate(p_vtt = ifelse(is.na(p_vtt), 0, p_vtt),
         month = month(date),
         day = day(date)) %>%
  ggplot(aes(x = date,
             y = p_vtt)) +
  geom_line() +
  facet_wrap(~ transceiver_id, ncol = 1) +
  theme_bw() +
  labs(x = NULL,
       y = "Proportion VTT Tags Read (Averaged Across Antennae)",
       title = paste0(site, ", ", yr))

# summary of vtt reads by site and year for each spawning season
vtt_summ = vtt_per_day %>%
  # create new df with each unique combo of site_code, year, and transceiver_id present in data
  group_by(site_code, year, transceiver_id) %>%
  summarise(.groups = "keep") %>%
  # expand data frame to include all mdays for each year
  expand(date = format(seq(ymd("1900-01-01"),
                           ymd("1900-12-31"),
                           by = "days"),
                       "%m-%d")) %>%
  mutate(date = as.Date(paste0(as.character(year), "-", date))) %>%
  left_join(vtt_per_day,
            by = c("site_code", "year", "transceiver_id", "date")) %>%
  # replace NAs with 0
  mutate(p_vtt = ifelse(is.na(p_vtt), 0, p_vtt),
         month = month(date),
         day = day(date)) %>%
  # set Chinook "spawning season"
  mutate(chnk = case_when(month == 6 & day >= 1 |
                          month %in% 7:8 |
                          month == 9 & day <= 15 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  # set steelhead "spawning season"
  mutate(sthd = case_when(month %in% 3:5 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  # set coho "spawning season"
  mutate(coho = case_when(month %in% 10:12 ~ TRUE,
                          TRUE ~ FALSE)) %>%
  select(-month, -day) %>%
  # summarise the proportion of each spawning season that each transceiver was reading vtts, averaged across antenna
  group_by(site_code,
           year,
           transceiver_id) %>%
  summarise(
    chnk_p_vtt = mean(p_vtt[chnk == T], na.rm = T),
    sthd_p_vtt = mean(p_vtt[sthd == T], na.rm = T),
    coho_p_vtt = mean(p_vtt[coho == T], na.rm = T),
    .groups = "drop"
  ) %>%
  # add an id for arrays
  mutate(array = paste0(site_code, "_", transceiver_id)) %>%
  pivot_longer(cols = c(chnk_p_vtt, sthd_p_vtt, coho_p_vtt),
               names_to = "species",
               values_to = "p_vtt") %>%
  mutate(species = recode(species,
                          chnk_p_vtt = "Chinook",
                          sthd_p_vtt = "Steelhead",
                          coho_p_vtt = "Coho")) %>%
  # remove 2024 for now, incomplete data
  filter(year != 2024)

# plot vtt summaries by array and year
arrays = unique(vtt_summ$array)
plot_list = list()
for(a in arrays) {
  array_p = subset(vtt_summ, array == a) %>%
    ggplot(aes(x = year,
               y = p_vtt,
               color = species)) +
    geom_line() +
    labs(x = NULL,
         y = "Avg. VTT Reads",
         color = NULL,
         title = a) +
    scale_x_continuous(limits = c(min(vtt_summ$year), max(vtt_summ$year)),
                       breaks = seq(min(vtt_summ$year), max(vtt_summ$year), by = 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw()
  
  plot_list[[a]] = array_p
}
all_arrays_p = gridExtra::marrangeGrob(plot_list, nrow = 8, ncol = 1)
ggsave(paste0(here("figures/array_vtt_summary.pdf")),
       all_arrays_p,
       width = 8.5,
       height = 14,
       units = "in")

# save iptds operations objects
save(iptds_ops,
     vtt_summ,
     file = here("output/iptds_operations_summaries.rda"))

### END SCRIPT
