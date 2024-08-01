# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to begin to summarize available habitat within ICTRT populations,
#   including the amount of available habitat above IPTDS.
# 
# Created: July 10, 2024
#   Last Modified: July 31, 2024
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
  dplyr::select(site_code) %>%
  # transform to WGS84
  st_transform(default_crs) %>%
  # join ictrt populations to iptds (just using sthd for now)
  st_join(sthd_pops %>%
            dplyr::select(pop = TRT_POPID)) %>%
  arrange(site_code)

# load the prepped intrinsic potential and redd qrf datasets
load(file = here("data/spatial/prepped_snake_ip.rda"))
load(file = here("data/spatial/snake_redd_qrf.rda"))
  
# plot the intrinsic potential data
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
  labs(title = "Intrinsic Potential with IPTDS and Steelhead Populations") +
  theme_minimal()

# plot the qrf data
ggplot() +
  geom_sf(data = qrf_sf,
          color = "springgreen",
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = iptds_sf, 
          color = "red",
          size = 2) +
  labs(title = "Redd QRF with IPTDS and Steelhead Populations") +
  theme_minimal()

#--------------------
# estimate available ip and qrf habitat within iptds polygons

# create empty data frame to store results for each site
site_avail_hab = NULL
for (s in 1:nrow(iptds_sf)) {
  
  # grab the site and watershed polygon
  site = iptds_sf[s,] %>% st_drop_geometry()
  site_poly = get(load(paste0(here("output/iptds_polygons"), "/", site$site_code, ".rda")))
  
  cat(paste0("Estimating available habitat for site ", site$site_code, ".\n"))
  
  # summarize intrinsic potential habitat for each site polygon
  site_ip = ip_sf %>%
    st_intersection(site_poly) %>%
    st_drop_geometry() %>%
    summarize(chnk_ip_length_w = sum(length_w_chnk, na.rm = T),
              chnk_ip_area_w = sum(area_w_chnk, na.rm = T),
              chnk_ip_length_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = T),
              chnk_ip_area_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = T),
              sthd_ip_length_w = sum(length_w_sthd, na.rm = T),
              sthd_ip_area_w = sum(area_w_sthd, na.rm = T),
              sthd_ip_length_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = T),
              sthd_ip_area_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = T),
              .groups = "drop") %>%
    mutate(site_code = site$site_code)
  
  # summarize redd qrf habitat for each site polygon
  site_qrf = qrf_sf %>%
    st_intersection(site_poly) %>%
    st_drop_geometry() %>%
    summarize(chnk_qrf_length_m = sum(ifelse(chnk, reach_leng_m, 0)),
              chnk_qrf_n = sum(ifelse(chnk, chnk_per_m * reach_leng_m, 0)),
              chnk_qrf_n_se = sum(ifelse(chnk, chnk_per_m_se * reach_leng_m, 0)),
              sthd_qrf_length_m = sum(ifelse(sthd, reach_leng_m, 0)),
              sthd_qrf_n = sum(ifelse(sthd, sthd_per_m * reach_leng_m, 0)),
              sthd_qrf_n_se = sum(ifelse(sthd, sthd_per_m_se * reach_leng_m, 0)),
              .groups = "drop") %>%
    mutate(site_code = site$site_code)
  
  # join the ip and qrf summaries for each site
  site_ip_qrf = left_join(site_ip, site_qrf, by = "site_code") %>%
    select(site_code, everything())
  
  # append ip and qrf results to site_avail_hab
  site_avail_hab = rbind(site_avail_hab, site_ip_qrf)

}

#--------------------
# estimate available ip habitat within trt populations
pop_ip = ip_sf %>%
  # estimate available ip habitat for chinook populations
  st_join(chnk_pops %>%
            select(chnk_pop = TRT_POPID)) %>%
  st_drop_geometry() %>%
  group_by(chnk_pop) %>%
  summarize(ip_length_w = sum(length_w_chnk, na.rm = T),
            ip_area_w = sum(area_w_chnk, na.rm = T),
            ip_length_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = T),
            ip_area_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = T),
            .groups = "drop") %>%
  rename(pop = chnk_pop) %>%
  mutate(species = "chnk") %>%
  filter(!is.na(pop)) %>%
  # estimate available ip habitat for steelhead populations
  rbind(
    ip_sf %>%
      st_join(sthd_pops %>%
                select(sthd_pop = TRT_POPID)) %>%
      st_drop_geometry() %>%
      group_by(sthd_pop) %>%
      summarize(ip_length_w = sum(length_w_sthd, na.rm = T),
                ip_area_w = sum(area_w_sthd, na.rm = T),
                ip_length_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = T),
                ip_area_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = T),
                .groups = "drop") %>%
      rename(pop = sthd_pop) %>%
      mutate(species = "sthd") %>%
      filter(!is.na(pop))
  ) %>%
  select(species, pop, everything())

#--------------------
# estimate available qrf habitat within trt populations
pop_qrf = qrf_sf %>%
  # estimate available qrf habitat for chinook populations
  st_join(chnk_pops %>%
            select(chnk_pop = TRT_POPID)) %>%
  st_drop_geometry() %>%
  group_by(chnk_pop) %>%
  summarize(qrf_length_m = sum(ifelse(chnk, reach_leng_m, 0)),
            qrf_n = sum(ifelse(chnk, chnk_per_m * reach_leng_m, 0)),
            qrf_n_se = sum(ifelse(chnk, chnk_per_m_se * reach_leng_m, 0)),
            .groups = "drop") %>%
  rename(pop = chnk_pop) %>%
  mutate(species = "chnk") %>%
  filter(!is.na(pop)) %>%
  # estimate available qrf habitat for sthd populations
  rbind(
    qrf_sf %>%
      st_join(sthd_pops %>%
                select(sthd_pop = TRT_POPID)) %>%
      st_drop_geometry() %>%
      group_by(sthd_pop) %>%
      summarize(qrf_length_m = sum(ifelse(sthd, reach_leng_m, 0)),
                qrf_n = sum(ifelse(sthd, sthd_per_m * reach_leng_m, 0)),
                qrf_n_se = sum(ifelse(sthd, sthd_per_m_se * reach_leng_m, 0)),
                .groups = "drop") %>%
      rename(pop = sthd_pop) %>%
      mutate(species = "sthd") %>%
      filter(!is.na(pop))
  ) %>%
  select(species, pop, everything())

# join population ip and qrf results


### END SCRIPT