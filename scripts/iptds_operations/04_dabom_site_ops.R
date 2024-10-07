# -----------------------
# Author: Mike Ackerman
# Purpose: Summarize operations of sites used in DABOM.
# 
# Created: August 7, 2024
#   Last Modified: October 7, 2024
# 
# Notes: I need to think about how I output the results of the following without overwriting previous stuff, especially when the user fills out
# which sites are operational each year and which sites to use to produce abundance estimates. In the future, I may do this for individual years
# after updating ptagis operations and vtt summaries and then append to previous results.

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(sf)
library(readxl)
library(writexl)

# load configuration files
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240927.rda") ; rm(flowlines, configuration, parent_child, crb_sites_sf)

# summarize populations for each site
dabom_site_pops = sr_site_pops %>%
  select(site_code,
         site_type,
         sthd = sthd_popid,
         chnk = chnk_popid,
         coho = coho_popid) %>%
  # re-format data frame
  pivot_longer(
    cols = c(sthd, chnk, coho),
    names_to = "species",
    values_to = "popid"
  ) %>%
  st_drop_geometry()

# load site operations datasets
load(here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))
load(here("output/iptds_operations/ptagis_virtual_test_tags_summary.rda"))

# summarize dabom site operations by species, site, and spawn year
dabom_int_ops = ptagis_ops %>%
  left_join(vtt_summ,
            by = c("species", "site_code", "year")) %>%
  # year is equal to spawn_year for all species
  rename(spawn_year = year) %>%
  # IMPORTANT STEP: Determine if site is operational or not based on ptagis ops and vtt datasets
  mutate(ptagis_operational = ifelse(p_days_ptagis_run >= 0.75, # was site operational for at least 75% of days of species run
                                     TRUE,
                                     FALSE)) %>%
  mutate(vtt_operational = ifelse(n_transceiver > 0 & # are there greater than 0 transceivers
                                    p_vtt >= 0.50,      # is p_vtt >= 0.50, averaged across transceivers and antennas
                                  TRUE,     
                                  FALSE)) %>%
  # replace NAs with FALSE for vtt_operational
  mutate(vtt_operational = replace_na(vtt_operational, FALSE)) %>%
  # remove years prior to 2010
  filter(spawn_year >= 2010) %>%
  # fill in missing years by species and site_code; for missing records set operational columns to FALSE
  ungroup() %>%
  mutate(min_spawn_year = min(spawn_year),
         max_spawn_year = max(spawn_year)) %>%
  group_by(species, site_code) %>%
  complete(spawn_year = full_seq(c(min_spawn_year, max_spawn_year), 1)) %>%
  mutate(ptagis_operational = ifelse(is.na(ptagis_operational), FALSE, ptagis_operational),
         vtt_operational = ifelse(is.na(vtt_operational), FALSE, vtt_operational)) %>%
  ungroup() %>%
  select(-min_spawn_year, -max_spawn_year) %>%
  # create "auto" and "user" designations of whether a site was operational or not
  mutate(auto_operational = ptagis_operational | vtt_operational) %>%
  mutate(user_operational = case_when(
    ptagis_operational == TRUE & vtt_operational == TRUE ~ TRUE,
    ptagis_operational == FALSE & vtt_operational == FALSE ~ FALSE,
    TRUE ~ NA)) %>%
  # add populations; use right_join to remove sites not in dabom_site_pops
  right_join(dabom_site_pops %>%
               # exclude MRR sites for now
               filter(site_type == "INT") %>%
               select(species, site_code, popid),
             by = c("species", "site_code")) %>%
  # for coho, remove any records prior to 2023
  filter(!(species == "coho" & spawn_year < 2023)) %>%
  arrange(site_code, spawn_year, species) %>%
  mutate(use_for_pop_abundance = NA)

# read in complete tag histories since SY2010
tags_by_site = list.files(path = "C:/Git/SnakeRiverFishStatus/data/complete_tag_histories/",
                          pattern = "\\.csv$",
                          full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(.x, show_col_types = F), .id = "file_name") %>%
  # add species and spawn year
  mutate(file_name = str_replace(file_name, ".*/", ""), 
         species = str_extract(file_name, "(?<=_)[^_]+"),       
         spawn_year = str_extract(file_name, "SY[0-9]{4}")) %>%
  select(-file_name) %>%
  # summarize number of unique tags observed by site
  select(species,
         spawn_year,
         site_code = `Event Site Code Value`,
         tag_code = `Tag Code`) %>%
  filter(site_code %in% (dabom_site_pops %>%
                           #filter(site_type == "MRR") %>%
                           pull(site_code) %>%
                           unique())) %>%
  distinct() %>%
  mutate(spawn_year = as.numeric(gsub("SY", "", spawn_year))) %>%
  mutate(species = case_when(
    species == "Chinook" ~ "chnk",
    species == "Steelhead" ~ "sthd",
    species == "Coho" ~ "coho"
  )) %>%
  group_by(species,
           spawn_year,
           site_code) %>%
  summarise(n_tags = n(), .groups = "drop")

# unique species and spawn_year combinations in dabom_int_ops
spc_sy = dabom_int_ops %>%
  select(species, spawn_year) %>%
  distinct()

# summarize operations for mrr sites used in dabom
dabom_mrr_ops = dabom_site_pops %>%
  select(site_code, site_type) %>%
  filter(site_type == "MRR") %>%
  select(-site_type) %>%
  distinct() %>%
  # expand based on spc_yr
  crossing(spc_sy) %>%
  # add the number of tags observed for a given species and spawn year (if any)
  left_join(tags_by_site, 
            by = c("species", "site_code", "spawn_year")) %>%
  mutate(n_tags = replace_na(n_tags, 0)) %>%
  # set auto_operational to TRUE if tags were observed
  mutate(auto_operational = n_tags > 0) %>%
  mutate(user_operational = ifelse(auto_operational, TRUE, NA)) %>%
  select(-n_tags)

# merge the int and mrr operations
dabom_ops = bind_rows(dabom_int_ops, dabom_mrr_ops) %>%
  left_join(tags_by_site, 
            by = c("species", "site_code", "spawn_year")) %>%
  mutate(n_tags = replace_na(n_tags, 0),
         notes = NA) %>%
  # consider adding a line where use_for_pop_abundance is auto set to FALSE if user_operational is FALSE
  select(species, 
         popid, 
         site_code, 
         spawn_year, 
         p_days_ptagis_run,
         p_days_ptagis_yr,
         p_vtt,
         n_transceiver,
         n_tags,
         everything(),
         notes) %>%
  arrange(site_code, species, popid, spawn_year)

# write dabom site operations summary to excel file
#write_xlsx(dabom_ops, path = paste0(here("output/iptds_operations/dabom_site_operations_"), Sys.Date(), ".xlsx"))

### END SCRIPT
