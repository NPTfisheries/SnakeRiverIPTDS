# -----------------------
# Author: Mike Ackerman
# Purpose: Compile and summarise IPTDS operational dates and virtual test
#   tag data for all Snake River sites from PTAGIS.
# 
# Created: May 6, 2024
#   Last Modified: August 2, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)

#------------------------------
# summarize queried VTT data

# read in and combine virtual test tag data
vtt_df = list.files(path = here("data/virtual_test_tags/"),
                    pattern = "\\.rds$",
                    recursive = T,
                    full.names = T) %>%
  map(readRDS) %>%
  map_dfr(bind_rows) %>%
  mutate(year = year(time_stamp)) 

# summarize the number of vtt reads per hour by site, year, and antenna
vtt_per_hr = vtt_df %>%
  # round each time stamp down to the nearest hour
  mutate(time_stamp = floor_date(time_stamp, "hour")) %>%
  # count the number of vtt reads within each hour, per antenna
  group_by(site_code,
           year,
           transceiver_id,
           antenna_id,
           time_stamp) %>%
  summarize(vtt_reads = n(),
            .groups = "drop")

# summarize the proportion of hours within each day each transceiver had vtt reads, averaged across antennas by site, year, and day
vtt_per_day = vtt_per_hr %>%
  mutate(date = as.Date(time_stamp)) %>%
  # summarize the proportion of hours each day that each antenna had vtt_reads
  group_by(site_code,
           year,
           date,
           transceiver_id,
           antenna_id) %>%
  summarize(p_vtt = sum(vtt_reads > 0) / 24,
            .groups = "drop") %>%
  # add in antennas that had no vtt_reads for a given site, year, and date
  group_by(site_code, year) %>%
  complete(date, transceiver_id,
           fill = list(p_vtt = 0)) %>%
  # average p_vtt across antennas for each transceiver
  group_by(site_code,
           year,
           date,
           transceiver_id) %>%
  summarize(p_vtt = mean(p_vtt),
            .groups = "drop")

# example plot for a single site and year, faceted by transceiver
site = "ZEN"
yr = 2023
vtt_per_day %>%
  filter(site_code == site,
         year == yr) %>%
  group_by(site_code, year, transceiver_id) %>%
  summarise(.groups = "keep") %>%
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
  ggplot(aes(x = date,
             y = p_vtt)) +
  geom_line() +
  facet_wrap(~ transceiver_id, ncol = 1) +
  theme_test() +
  labs(x = NULL,
       y = "Proportion VTT Tags Read (Averaged Across Antennas)",
       title = paste0(site, ", ", yr))

# summary of vtt reads by site and year
vtt_summ = vtt_per_day %>%
  # create new df with each unique combo of site_code, year, and transceiver_id present in data
  group_by(site_code, year, transceiver_id) %>%
  summarize(.groups = "keep") %>%
  # expand data frame to include all month-days for each year
  expand(date = format(seq(ymd("1900-01-01"),
                           ymd("1900-12-31"),
                           by = "days"),
                       "%m-%d")) %>%
  mutate(date = as.Date(paste0(as.character(year), "-", date))) %>%
  # join proportion of vtt reads
  left_join(vtt_per_day,
            by = c("site_code", "year", "transceiver_id", "date")) %>%
  # replace NAs with 0
  mutate(p_vtt = ifelse(is.na(p_vtt), 0, p_vtt),
         # add columns for month and day
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
  group_by(site_code, year, transceiver_id) %>%
  summarize(
    # calculate the mean of p_vtt where each species is TRUE, excluding groups where all values are 0
    chnk_p_vtt = ifelse(all(chnk == 0), NA, mean(p_vtt[chnk == T], na.rm = T)),
    sthd_p_vtt = ifelse(all(sthd == 0), NA, mean(p_vtt[sthd == T], na.rm = T)),
    coho_p_vtt = ifelse(all(coho == 0), NA, mean(p_vtt[coho == T], na.rm = T)),
    # check to see if there is any instance where chnk is TRUE and p_vtt is > 0; if so, return 1, otherwise return 0
    chnk_n_ts = ifelse(any(chnk & p_vtt > 0, na.rm = TRUE), 1, 0),
    sthd_n_ts = ifelse(any(sthd & p_vtt > 0, na.rm = TRUE), 1, 0),
    coho_n_ts = ifelse(any(coho & p_vtt > 0, na.rm = TRUE), 1, 0),
    .groups = "drop"
  ) %>%
  # now summarize by site and year (group transceivers); also count the number of transceivers in operation
  group_by(site_code, year) %>%
  summarize(
    chnk_p_vtt = mean(chnk_p_vtt),
    sthd_p_vtt = mean(sthd_p_vtt),
    coho_p_vtt = mean(coho_p_vtt),
    chnk_n_ts = sum(chnk_n_ts),
    sthd_n_ts = sum(sthd_n_ts),
    coho_n_ts = sum(coho_n_ts),
    .groups = "drop") %>%
  # re-format data frame to longer format
  pivot_longer(
    cols = c(chnk_p_vtt, sthd_p_vtt, coho_p_vtt),
    names_to = "species",
    names_prefix = "",
    values_to = "p_vtt") %>%
  mutate(
    species = case_when(
      species == "chnk_p_vtt" ~ "chnk",
      species == "sthd_p_vtt" ~ "sthd",
      species == "coho_p_vtt" ~ "coho")) %>%
  mutate(n_transceiver = case_when(
    species == "chnk" ~ chnk_n_ts,
    species == "sthd" ~ sthd_n_ts,
    species == "coho" ~ coho_n_ts
  )) %>%
  # finally, clean up the resulting data frame
  select(species, 
         site_code,
         year,
         p_vtt,
         n_transceiver) 

# plot vtt summaries by site and year
sites = unique(vtt_summ$site_code)
plot_list = list()
for(s in sites) {
  site_p = subset(vtt_summ, site_code == s) %>%
    ggplot(aes(x = year,
               y = p_vtt,
               color = species)) +
    geom_line() +
    labs(x = NULL,
         y = "Avg. VTT Reads",
         color = NULL,
         title = s) +
    scale_x_continuous(limits = c(min(vtt_summ$year), max(vtt_summ$year)),
                       breaks = seq(min(vtt_summ$year), max(vtt_summ$year), by = 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw()
  
  plot_list[[s]] = site_p
}

# save site vtt summary plot to .pdf
all_sites_p = gridExtra::marrangeGrob(plot_list, nrow = 8, ncol = 1)
ggsave(paste0(here("output/figures/iptds_operations/site_vtt_summary_"), Sys.Date(), ".pdf"),
       all_sites_p,
       width = 8.5,
       height = 14,
       units = "in")

#------------------------------
# summarize ptagis operational dates and VTT summaries together
# load iptds operational dates data frame
load(here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))

site_ops = vtt_summ %>%
  # IMPORTANT STEP: Determine if site is operational or not based on vtt data
  mutate(vtt_operational = ifelse(n_transceiver > 0 & # are there greater than 0 transceivers
                                  p_vtt >= 0.25,      # is p_vtt >= 0.25, averaged across transceivers and antennas
                                  TRUE,               # site is operational
                                  FALSE)) %>%         # site is not operational
  # join some information from ptagis site operations
  left_join(ptagis_ops %>%
              select(site_code,
                     ptagis_first_year = first_year,
                     ptagis_last_year = last_year,
                     `2010`:last_col()) %>%
              pivot_longer(
                cols = `2010`:last_col(),
                names_to = "year",
                values_to = "ptagis_n_days"
              ) %>%
              mutate(year = as.numeric(year)) %>%
              mutate(ptagis_p_days = ptagis_n_days / 365) %>%
              filter(year >= ptagis_first_year,
                     year <= ptagis_last_year) %>%
              select(-ptagis_first_year, -ptagis_last_year),
            by = c("site_code", "year"))

# JUST NEED TO BUTTON THIS UP TO MAKE FINAL DETERMINATIONS FOR SITE OPERATIONS BY SPECIES, SITE, AND YEAR!!!

# save site operations summary

### END SCRIPT
