# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to begin to summarize available habitat within ICTRT populations,
#   including the amount of available habitat above IPTDS.
# 
# Created: July 10, 2024
#   Last Modified: 
# 
# Notes:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(here)

#--------------------
# load and prep data

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)

sthd_pops = sth_pop %>%
  st_set_crs(st_crs(4326)) ; rm(sth_pop)

chnk_pops = spsm_pop %>%
  st_set_crs(st_crs(4326)) ; rm(spsm_pop)

# snake river iptds
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240304.rda")
rm(configuration, node_paths, parent_child, pc_nodes)

iptds_sf = sites_sf %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>%
  # grab only INT sites
  filter(site_type == "INT") %>%
  select(site_code, site_name) %>%
  # transform to WGS84
  st_transform(4326)

# iptds_sf = sites_sf %>%
#   # tranform to WGS84
#   
#   # trim down to sites within the Snake River steelhead DPS
#   st_join(sthd_pops) %>%
#   filter(!is.na(TRT_POPID)) %>%
#   # grab only INT sites
#   filter(site_type == "INT") %>%
#   # remove a few dam sites
#   filter(!str_detect(site_name, "Little Goose|Lower Granite|Lower Monumental"))

# intrinsic potential layer
ip_sf = readRDS(here("data/spatial/ip.rds"))

# plot the data
ggplot() +
  geom_sf(data = ip_sf,
          color = "dodgerblue",
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = iptds_sf, 
          color = "red",
          size = 2) +
  theme_minimal()

### END SCRIPT
