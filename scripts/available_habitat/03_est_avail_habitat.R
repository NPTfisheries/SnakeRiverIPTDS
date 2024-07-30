# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to begin to summarize available habitat within ICTRT populations,
#   including the amount of available habitat above IPTDS.
# 
# Created: July 10, 2024
#   Last Modified: July 12, 2024
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

# snake river iptds
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240304.rda")
rm(configuration, node_paths, parent_child, pc_nodes)

iptds_sf = sites_sf %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  # note this excludes some sites below lgr, but those likely aren't appropriate for expansions anyways
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>% 
  # grab only INT sites
  filter(site_type == "INT") %>%
  select(site_code, site_name) %>%
  # transform to WGS84
  st_transform(default_crs)

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

# intrinsic potential layer; already been clipped using snake river steelhead DPS
ip_sf = readRDS(here("data/spatial/ip.rds")) %>%
  clean_names() %>%
  st_transform(default_crs) %>%
  # trim down to only useful columns
  select(name,
         llid,
         strmname,
         length_m = length,
         elev_m = elev,
         wide_ww,
         wide_bf,
         gradient,
         sthdrate,
         chinrate,
         # current spawning, from state agencies, streamnet, observation, and expert opinion
         currsthd = currsush,
         currspch,
         currsuch) %>%
  # merge current spring and summer chinook spawning into a single column
  mutate(currchnk = currspch + currsuch) %>%
  select(-currspch,-currsuch) %>%
  # potential habitat weights based on recommendations from Cooney and Holzer (2006) Appendix C
  mutate(sthd_wt = case_when(sthdrate == 3 ~ 1,
                             sthdrate == 2 ~ 0.5,
                             sthdrate == 1 ~ 0.25,
                             TRUE ~ 0)) %>%
  mutate(chnk_wt = case_when(chinrate == 3 ~ 1,
                             chinrate == 2 ~ 0.5,
                             chinrate == 1 ~ 0.25,
                             TRUE ~ 0)) %>%
  # calculate some potentially useful metrics; matches calculations done by ip group
  mutate(area_ww = length_m * wide_ww,
         area_bf = length_m * wide_bf,
         length_w_sthd = length_m * sthd_wt,
         length_w_chnk = length_m * chnk_wt,
         # note that sthd uses bankfull width; chnk uses wetted width to reflect time of occupancy
         area_w_sthd = area_bf * sthd_wt,
         area_w_chnk = area_ww * chnk_wt) %>%
  # move geometry to the end
  select(everything(), geometry) %>%
  # finally, remove stream reaches with "negligible" habitat
  filter(!(chnk_wt == 0 & sthd_wt == 0)) %>%
  # join mpg and trt population names 
  st_join(sthd_pops %>%
            select(sthd_mpg = MPG,
                   sthd_popid = TRT_POPID)) %>%
  st_join(chnk_pops %>%
            select(chnk_mpg = MPG,
                   chnk_popid = TRT_POPID))
  
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

#--------------------
# estimate available ip habitat within trt populations
sthd_pop_ip = ip_sf %>%
  st_drop_geometry() %>%
  group_by(sthd_popid) %>%
  summarize(ip_length_w = sum(length_w_sthd, na.rm = T),
            ip_area_w = sum(area_w_sthd, na.rm = T),
            ip_length_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = T),
            ip_area_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = T),
            .groups = "drop") %>%
  rename(popid = sthd_popid) %>%
  mutate(species = "sthd") %>%
  filter(!is.na(popid))

chnk_pop_ip = ip_sf %>%
  st_drop_geometry() %>%
  group_by(chnk_popid) %>%
  summarize(ip_length_w = sum(length_w_chnk, na.rm = T),
            ip_area_w = sum(area_w_chnk, na.rm = T),
            ip_length_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = T),
            ip_area_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = T),
            .groups = "drop") %>%
  rename(popid = chnk_popid) %>%
  mutate(species = "chnk") %>%
  filter(!is.na(popid))

pop_ip = bind_rows(sthd_pop_ip, chnk_pop_ip) %>%
  mutate(ip_length_p = ip_length_curr / ip_length_w,
         ip_area_p = ip_area_curr / ip_area_w) %>%
  select(species, popid, everything()) ; rm(sthd_pop_ip, chnk_pop_ip)


### END SCRIPT

#--------------------
# scrap code; looks cute, might delete later

#--------------------
# snap iptds to ip dataset
# iptds_sf_snap = st_snap(iptds_sf, ip_sf, tolerance = 50)

# dem_list = lapply(dem_files, rast)
# unlist(dem_list)
# 
# m = mosaic(unlist(dem_list), fun = mean)
# combined_extent = ext(do.call(c, lapply(dem_list, ext)))
# 
# extents = lapply(dem_list, ext)
# 
# # Reproject and resample to a common resolution
# target_dem <- dem_list[[1]]
# target_res <- res(target_dem)
# 
# dem_list <- lapply(dem_list, function(dem) {
#   dem <- project(dem, crs(target_dem))
#   resample(dem, target_dem)
# })
# 
# # Merge DEM tiles using do.call
# merged_dem <- do.call(mosaic, c(dem_list, fun = "mean"))
# 
# # Fill NoData values using focal
# filled_dem <- focal(merged_dem, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)

