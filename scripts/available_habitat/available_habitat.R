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

# load and prep data
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240304.rda")
rm(configuration, node_paths, parent_child, pc_nodes)

iptds_sf = sites_sf %>%
  filter(str_starts(rkm, "522")) %>% # i need to modify this to remove dams
  filter(nchar(site_code) == 3)
