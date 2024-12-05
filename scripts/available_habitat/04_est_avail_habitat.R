# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to begin to summarize available habitat within ICTRT populations,
#   including the amount of available habitat above IPTDS.
# 
# Created: July 10, 2024
#   Last Modified: November 13, 2024
# 
# Notes:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(sf)
library(here)
library(janitor)
library(ggrepel)

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
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20241105.rda")
rm(configuration, parent_child, flowlines)

# create sf object of dabom sites
sr_int_sites_sf = sr_site_pops %>%
  select(site_code,
         site_type,
         sthd_popid,
         chnk_popid) %>%
  filter(site_type == "INT") %>%
  st_join(crb_sites_sf %>%
            select(rkm),
          by = "site_code") %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>%
  select(-site_type, -rkm) %>%
  pivot_longer(cols = c("sthd_popid", "chnk_popid"),
               names_to = "spc_code",
               values_to = "popid") %>%
  mutate(spc_code = str_sub(spc_code, 1, 4)) %>%
  filter(!is.na(popid))

# load the prepped intrinsic potential and redd qrf datasets
load(file = here("data/spatial/prepped_snake_ip.rda"))

qrf_sf = get(load(file = here("data/spatial/snake_redd_qrf.rda")))

# plot the intrinsic potential data
ggplot() +
  geom_sf(data = ip_sf,
          aes(color = as.factor(sthdrate)),
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = sr_int_sites_sf, 
          color = "red",
          size = 2) +
  labs(title = "Intrinsic Potential with IPTDS and Steelhead Populations") +
  theme_minimal()

# plot the qrf data
ggplot() +
  geom_sf(data = qrf_sf,
          aes(color = sthd_use),
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = sr_int_sites_sf, 
          color = "red",
          size = 2) +
  labs(title = "Redd QRF with IPTDS and Steelhead Populations") +
  theme_minimal()

#--------------------
# estimate available ip and qrf habitat within iptds polygons

# create empty data frame to store results for each site
site_avail_hab = NULL
for (s in 1:nrow(sr_int_sites_sf)) {
  
  # grab the site_code, spc_code, and polygons for each combination
  site_code = sr_int_sites_sf[s,] %>% pull(site_code)
  spc_code  = sr_int_sites_sf[s,] %>% pull(spc_code)
  
  site_poly = get(load(paste0(here("output/iptds_polygons"), "/", spc_code, "/", site_code, ".rda")))
  
  cat(paste0("Estimating available habitat above site ", site_code, ", ", spc_code, ".\n"))
  
  # summarize intrinsic potential habitat for each site polygon
  site_ip = ip_sf %>%
    st_intersection(site_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc_code == "chnk") {
        summarise(., 
                  ip_length_w = sum(length_w_chnk, na.rm = TRUE),
                  ip_area_w = sum(area_w_chnk, na.rm = TRUE),
                  ip_length_w_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = TRUE),
                  ip_area_w_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = TRUE),
                  .groups = "drop")
      } else if (spc_code == "sthd") {
        summarise(., 
                  ip_length_w = sum(length_w_sthd, na.rm = TRUE),
                  ip_area_w = sum(area_w_sthd, na.rm = TRUE),
                  ip_length_w_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = TRUE),
                  ip_area_w_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = TRUE),
                  .groups = "drop")
      }
    } %>%
    mutate(site_code = site_code,
           spc_code = spc_code)
  
  # summarize redd qrf habitat for each site polygon
  site_qrf = qrf_sf %>%
    st_intersection(site_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc_code == "chnk") {
        filter(., chnk == TRUE & chnk_use == "Spawning and rearing") %>%
        summarise(.,
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(chnk_per_m * reach_leng_m),
                  qrf_n_se = sum(chnk_per_m_se * reach_leng_m),
                  .groups = "drop")
      } else if (spc_code == "sthd") {
        filter(., sthd == TRUE & sthd_use == "Spawning and rearing") %>%
        summarise(.,
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(sthd_per_m * reach_leng_m),
                  qrf_n_se = sum(sthd_per_m_se * reach_leng_m),
                  .groups = "drop")
      }
    } %>%
    mutate(site_code = site_code,
           spc_code = spc_code)
  
  # join the ip and qrf summaries for each site
  site_ip_qrf = left_join(site_ip, site_qrf, by = c("site_code", "spc_code")) %>%
    select(site_code, spc_code, everything())
  
  # append ip and qrf results to site_avail_hab
  site_avail_hab = bind_rows(site_avail_hab, site_ip_qrf)
  
} # end site loop

#--------------------
# estimate available habitat within trt populations
pop_df = sr_int_sites_sf %>%
  select(spc_code, popid) %>%
  st_drop_geometry() %>%
  distinct()

pop_avail_hab = NULL
for (p in 1:nrow(pop_df)) {
  
  # grab the spc_code and polygons for each population
  spc_code = pop_df[p,] %>% pull(spc_code)
  popid = pop_df[p,] %>% pull(popid)
  
  cat(paste0("Estimating available habitat in population ", popid, ".\n"))
  
  # if pop contains a "/", unlist pops
  if(str_detect(popid, "/")) { popid = list(str_split(popid, "/", simplify = TRUE) %>% str_replace_all("/", "")) %>% unlist() }
  
  # get the population polygon
  if(spc_code == "chnk") {
    pop_poly = chnk_pops %>%
      filter(TRT_POPID %in% popid) %>%
      mutate(TRT_POPID = factor(TRT_POPID, levels = popid)) %>%
      arrange(TRT_POPID) %>%
      select(popid = TRT_POPID) %>%
      # to accommodate cases with multiple populations
      summarise(
        popid = paste(popid, collapse = "/"),
        geometry = st_union(geometry)
      )
  }
  if(spc_code == "sthd") {
    pop_poly = sthd_pops %>%
      filter(TRT_POPID %in% popid) %>%
      mutate(TRT_POPID = factor(TRT_POPID, levels = popid)) %>%
      arrange(TRT_POPID) %>%
      select(popid = TRT_POPID) %>%
      # to accommodate cases with multiple populations
      summarise(
        popid = paste(popid, collapse = "/"),
        geometry = st_union(geometry)
      )
  }
  
  # summarize intrinsic potential habitat for each population polygon
  pop_ip = ip_sf %>%
    st_intersection(pop_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc_code == "chnk") {
        summarise(.,
                  ip_length_w = sum(length_w_chnk, na.rm = T),
                  ip_area_w = sum(area_w_chnk, na.rm = T),
                  ip_length_w_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = T),
                  ip_area_w_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = T),
                  .groups = "drop")
      } else if (spc_code == "sthd") {
        summarise(.,
                  ip_length_w = sum(length_w_sthd, na.rm = T),
                  ip_area_w = sum(area_w_sthd, na.rm = T),
                  ip_length_w_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = T),
                  ip_area_w_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = T),
                  .groups = "drop")
      }
    } %>%
    mutate(spc_code = spc_code,
           popid = pop_poly$popid)
  
  # estimate available qrf habitat within trt populations
  pop_qrf = qrf_sf %>%
    st_intersection(pop_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc_code == "chnk") {
        filter(., chnk == TRUE & chnk_use == "Spawning and rearing") %>%
        summarise(.,
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(chnk_per_m * reach_leng_m),
                  qrf_n_se = sum(chnk_per_m_se * reach_leng_m),
                  .groups = "drop")
      } else if (spc_code == "sthd") {
        filter(., sthd == TRUE & sthd_use == "Spawning and rearing") %>%
        summarise(.,
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(sthd_per_m * reach_leng_m),
                  qrf_n_se = sum(sthd_per_m_se * reach_leng_m),
                  .groups = "drop")
      }
    } %>%
    mutate(spc_code = spc_code,
           popid = pop_poly$popid)
  
  # join the ip and qrf summaries for each site
  pop_ip_qrf = left_join(pop_ip, pop_qrf, by = c("spc_code", "popid")) %>%
    select(spc_code, popid, everything())
  
  # append ip and qrf results to site_avail_hab
  pop_avail_hab = bind_rows(pop_avail_hab, pop_ip_qrf)
  
} # end population loop

#--------------------
# join and summarize site and population available habitat
avail_hab_df = site_avail_hab %>%
  left_join(sr_int_sites_sf,
            by = c("site_code", "spc_code")) %>%
  select(site_code, 
         spc_code, 
         popid,
         site_ip_length_w_curr = ip_length_w_curr,
         site_qrf_n = qrf_n,
         site_qrf_n_se = qrf_n_se,
         -geometry) %>%
  left_join(pop_avail_hab %>%
              select(spc_code,
                     popid,
                     pop_ip_length_w_curr = ip_length_w_curr,
                     pop_qrf_n = qrf_n,
                     pop_qrf_n_se = qrf_n_se),
            by = c("spc_code", "popid")) %>%
  mutate(
    p_ip_length_w_curr = site_ip_length_w_curr / pop_ip_length_w_curr,
    p_qrf_n = site_qrf_n / pop_qrf_n,
    # standard error of the proportion using the delta method
    p_qrf_n_se = p_qrf_n * sqrt((site_qrf_n_se / site_qrf_n)^2 + (pop_qrf_n_se / pop_qrf_n)^2)
  ) %>%
  select(site_code,
         spc_code,
         popid,
         site_ip_length_w_curr,
         pop_ip_length_w_curr,
         p_ip_length_w_curr,
         site_qrf_n,
         site_qrf_n_se,
         pop_qrf_n,
         pop_qrf_n_se,
         p_qrf_n,
         p_qrf_n_se)

# save the important objects
save(site_avail_hab,
     pop_avail_hab,
     avail_hab_df,
     file = here("output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda"))

# explore differences in proportions of IP and QRF Redd Habitat
ggplot(avail_hab_df, aes(x = p_qrf_n, 
                         y = p_ip_length_w_curr,
                         color = spc_code)) +
  geom_point() + 
  geom_abline(a = 0, b = 1) +
  #geom_text(aes(label = site_code)) +
  geom_text_repel(aes(label = site_code)) +
  labs(x = "p(QRF)",
       y = "p(IP)",
       title = "Scatterplot of proportion of habitat above IPTDS (IP vs. QRF).") +
  theme_minimal()

# explore differences in the total available IP vs. redd QRF habitat in trt populations
avail_hab_df %>%
  select(-site_code) %>%
  distinct(popid, .keep_all = TRUE) %>%
  ggplot(aes(x = pop_qrf_n,
             y = pop_ip_length_w_curr,
             color = spc_code)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = popid), size = 3) +
  labs(x = "QRF",
       y = "IP",
       title = "Scatterplot of available IP vs. QRF habitat.") +
  theme_minimal()

ggsave(here("output/figures/available_habitat/pop_ip_vs_qrf.png"),
       width = 11,
       height = 8,
       dpi = 300)

### END SCRIPT