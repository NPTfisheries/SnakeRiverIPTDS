# -----------------------
# Author: Mike Ackerman
# Purpose: Compare available spawning ip vs. qrf habitat using qrf dataset w/ ip attributes
#            joined to it.
# 
# Created: December 5, 2024
#   Last Modified:
# 
# Notes:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(here)
library(janitor)

# set default crs
default_crs = st_crs(32611) # WGS 84, UTM zone 11N

#--------------------
# load and prep data

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# load redd qrf dataset w/ ip attributes joined to it
load(file = here("data/spatial/full_snake_redd_qrf_w_ip_attr.rda"))

# trim datasets using extents in qrf, then calculate available ip vs. qrf habitat
qrf_sf_w_ip_trim = qrf_sf_w_ip_attr %>%
  # keep only reaches used by either sp/sum chinook or steelhead (according to StreamNet)
  filter(chnk == TRUE | sthd == TRUE) %>%
  # the chnk_use and sthd_use designations are FAR from perfect, but this at least gets rid of some mainstem reaches
  filter(!(chnk_use == "Migration only" & sthd_use == "Migration only"))