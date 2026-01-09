# -----------------------
# Author: Mike Ackerman
# Purpose: Compile some MRR and Snake River dam data for IPTDS leaflet
# 
# Created: April 22, 2024
#   Last Modified: January 9, 2026
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(sf)

# dabom mrr sites
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20260109.rda") ; rm(flowlines)
dabom_sites = parent_child %>%
  unlist() %>%
  unique() %>%
  tibble(site_code = .) %>%
  # remove dams and downriver sites
  filter(!site_code %in% c("LGR", "GRS", "GOA", "LMA", "IHR", "MCN", "JDA", "TDA", "BON", 
                           "PRA", "PRO", "WWB", "UMW", "JD1", "DRM", "KLR", "FID", "RCX", "LWL", "WRA")) %>%
  pull()

sr_dam_codes = c("LGR", "GOA", "LMA", "IHR")

mrr_sites = configuration %>%
  mutate(node = str_remove(node, "_D|_U")) %>%
  filter(node %in% dabom_sites) %>%
  filter(nchar(site_code) > 3) %>%
  select(site_code,
         node,
         site_type,
         site_name,
         site_description,
         latitude,
         longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

sr_dam_sites = configuration %>%
  filter(site_code %in% sr_dam_codes) %>%
  select(site_code,
         site_name,
         latitude,
         longitude) %>%
  distinct()

# save dabom mrr sites
save(mrr_sites, file = here("data/spatial/dabom_mrr_sites.rda"))
save(sr_dam_sites, file = here("data/spatial/sr_dam_sites.rda"))

### END SCRIPT
