# -----------------------
# Author: Mike Ackerman
# Purpose: Summarize operations of sites used in DABOM.
# 
# Created: August 7, 2024
#   Last Modified:
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(sf)
library(readxl)

# load configuration files
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240304.rda") ; rm(flowlines, configuration, node_paths, parent_child, pc_nodes)
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)

# summarize populations for each site
dabom_site_pops = sites_sf %>%
  # filter for sites at or above LTR
  filter(str_extract(rkm, "^[0-9]{3}") == "522" & as.numeric(str_extract(rkm, "(?<=\\.)[0-9]{3}")) >= 100) %>%
  # remove GOA, LGR, and GRS
  filter(!site_code %in% c("GOA", "LGR", "GRS")) %>%
  select(site_code, geometry) %>%
  st_transform(crs = 4326) %>%
  # join steelhead, sp/sum chinook, and (made up) coho populations
  st_join(sth_pop %>% 
            st_transform(crs = 4326) %>%
            select(sthd = TRT_POPID)) %>%
  st_join(spsm_pop %>% 
            st_transform(crs = 4326) %>%
            select(chnk = TRT_POPID)) %>%
  left_join(read_excel("C:/Git/SnakeRiverFishStatus/data/coho_populations/coho_populations.xlsx") %>%
              rename(site_code = spawn_site) %>%
              select(site_code,
                     coho = coho_TRT_POPID)) %>%
  # re-format data frame
  pivot_longer(
    cols = c(sthd, chnk, coho),
    names_to = "species",
    values_to = "trt_pop"
  ) %>%
  # correct some population designations
  mutate(trt_pop = case_when(
    site_code %in% c("SC1", "SC2") & species == "chnk" ~ "SCUMA",
    site_code %in% c("IR1", "IR2") & species == "chnk" ~ NA,      # We don't necessarily know whether IR1 and IR2 fish end up in IRMAI or IRBSH
    site_code == "JOC"             & species == "chnk" ~ "Joseph",
    site_code == "SW1"             & species == "chnk" ~ "SEUMA/SEMEA/SEMOO",
    site_code == "WR1"             & species == "chnk" ~ "GRLOS/GRMIN",
    site_code %in% c("SC1", "SC2") & species == "sthd" ~ "CRSFC-s",
    site_code == "USI"             & species == "sthd" ~ "SREFS-s",
    TRUE ~ trt_pop
  )) %>%
  st_drop_geometry() ; rm(spsm_pop, sth_pop)

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
               filter(nchar(site_code) == 3),
             by = c("species", "site_code")) %>%
  # for coho, remove any records prior to 2023
  filter(!(species == "coho" & spawn_year < 2023)) %>%
  arrange(site_code, spawn_year, species) %>%
  mutate(use_for_pop_abundance = NA)

# save dabom site operations summary
# save(site_ops,
#      file = here("output/iptds_operations/site_operations_summary.rda"))

### END SCRIPT
