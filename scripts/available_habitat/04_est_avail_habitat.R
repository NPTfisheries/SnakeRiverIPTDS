# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to begin to summarize available habitat within ICTRT populations,
#   including the amount of available habitat above IPTDS.
# 
# Created: July 10, 2024
#   Last Modified: August 8, 2024
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
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20241105.rda")
rm(configuration, parent_child, flowlines)

# create sf object of dabom sites
sr_int_site_sf = sr_site_pops %>%
  select(site_code, site_type, sthd_popid, chnk_popid) %>%
  filter(site_type == "INT") %>%
  st_join(crb_sites_sf %>%
            select(rkm),
          by = "site_code") %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>%
  select(-site_type, -rkm)

# load the prepped intrinsic potential and redd qrf datasets
load(file = here("data/spatial/prepped_snake_ip.rda"))

qrf_sf = get(load(file = here("data/spatial/snake_redd_qrf.rda"))) %>%
  # the chnk_use and sthd_use designations are FAR from perfect, but this at least gets rid of some mainstem reaches
  filter(!(chnk_use == "Migration only" & sthd_use == "Migration only"))

# plot the intrinsic potential data
ggplot() +
  geom_sf(data = ip_sf,
          color = "dodgerblue",
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = sr_int_site_sf, 
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
  geom_sf(data = sr_int_site_sf, 
          color = "red",
          size = 2) +
  labs(title = "Redd QRF with IPTDS and Steelhead Populations") +
  theme_minimal()

#--------------------
# estimate available ip and qrf habitat within iptds polygons

# create empty data frame to store results for each site
site_avail_hab = NULL
for (s in 1:nrow(sr_int_site_sf)) {
  
  # grab the site and watershed polygons for each species
  site = sr_int_site_sf[s,] %>% st_drop_geometry()
  
  # check if chnk_poly and sthd_poly files exist
  chnk_poly_path = paste0(here("output/iptds_polygons/chnk"), "/", site$site_code, ".rda")
  sthd_poly_path = paste0(here("output/iptds_polygons/sthd"), "/", site$site_code, ".rda")
  
  chnk_poly <- if (file.exists(chnk_poly_path)) get(load(chnk_poly_path)) else NULL
  sthd_poly <- if (file.exists(sthd_poly_path)) get(load(sthd_poly_path)) else NULL
  
  cat(paste0("Estimating available habitat for site ", site$site_code, ".\n"))
  
  # summarize intrinsic potential habitat for each site polygon
  site_ip = if (!is.null(chnk_poly)) {
    ip_sf %>%
      st_intersection(chnk_poly) %>%
      st_drop_geometry() %>%
      summarize(
        chnk_ip_length_w = sum(length_w_chnk, na.rm = TRUE),
        chnk_ip_area_w = sum(area_w_chnk, na.rm = TRUE),
        chnk_ip_length_curr = sum(if_else(currchnk > 0, length_w_chnk, 0), na.rm = TRUE),
        chnk_ip_area_curr = sum(if_else(currchnk > 0, area_w_chnk, 0), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble(chnk_ip_length_w = NA, chnk_ip_area_w = NA, chnk_ip_length_curr = NA, chnk_ip_area_curr = NA)
  }
  
  site_ip = bind_cols(site_ip,
                      if (!is.null(sthd_poly)) {
                        ip_sf %>%
                          st_intersection(sthd_poly) %>%
                          st_drop_geometry() %>%
                          summarize(
                            sthd_ip_length_w = sum(length_w_sthd, na.rm = TRUE),
                            sthd_ip_area_w = sum(area_w_sthd, na.rm = TRUE),
                            sthd_ip_length_curr = sum(if_else(currsthd > 0, length_w_sthd, 0), na.rm = TRUE),
                            sthd_ip_area_curr = sum(if_else(currsthd > 0, area_w_sthd, 0), na.rm = TRUE),
                            .groups = "drop"
                          )
                        } else {
                          tibble(sthd_ip_length_w = NA, sthd_ip_area_w = NA, sthd_ip_length_curr = NA, sthd_ip_area_curr = NA)
                        }
  ) %>% mutate(site_code = site$site_code)
  
  # summarize redd qrf habitat for each site polygon
  site_qrf = if (!is.null(chnk_poly)) {
    qrf_sf %>%
      st_intersection(chnk_poly) %>%
      st_drop_geometry() %>%
      summarize(
        chnk_qrf_length_m = sum(ifelse(chnk, reach_leng_m, 0)),
        chnk_qrf_n = sum(ifelse(chnk, chnk_per_m * reach_leng_m, 0)),
        chnk_qrf_n_se = sum(ifelse(chnk, chnk_per_m_se * reach_leng_m, 0)),
        .groups = "drop"
      )
  } else {
    tibble(chnk_qrf_length_m = NA, chnk_qrf_n = NA, chnk_qrf_n_se = NA)
  }
  
  site_qrf = bind_cols(site_qrf,
                       if (!is.null(sthd_poly)) {
                         qrf_sf %>%
                           st_intersection(sthd_poly) %>%
                           st_drop_geometry() %>%
                           summarize(
                             sthd_qrf_length_m = sum(ifelse(sthd, reach_leng_m, 0)),
                             sthd_qrf_n = sum(ifelse(sthd, sthd_per_m * reach_leng_m, 0)),
                             sthd_qrf_n_se = sum(ifelse(sthd, sthd_per_m_se * reach_leng_m, 0)),
                             .groups = "drop"
                           )
                        } else {
                          tibble(sthd_qrf_length_m = NA, sthd_qrf_n = NA, sthd_qrf_n_se = NA)
                        }
  ) %>% mutate(site_code = site$site_code)
  
  # join the ip and qrf summaries for each site
  site_ip_qrf = left_join(site_ip, site_qrf, by = "site_code") %>%
    select(site_code, everything())
  
  # append ip and qrf results to site_avail_hab
  site_avail_hab = bind_rows(site_avail_hab, site_ip_qrf)
  
} # end site loop

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
pop_avail_hab = left_join(pop_ip, pop_qrf, by = c("species", "pop"))

#--------------------
# join and summarize site and population available habitat
avail_hab_summ = sr_int_site_sf %>%
  st_drop_geometry() %>%
  left_join(site_avail_hab, by = "site_code") %>%
  select(site_code,
         chnk_popid,
         chnk_ip_length_w,
         chnk_ip_length_curr,
         chnk_qrf_n,
         chnk_qrf_n_se,
         sthd_popid,
         sthd_ip_length_w,
         sthd_ip_length_curr,
         sthd_qrf_n,
         sthd_qrf_n_se) %>%
  mutate(chnk_poplist = str_split(chnk_popid, "/")) %>%
  mutate(sthd_poplist = str_split(sthd_popid, "/"))

### CONTINUE HERE!!!

#--------------------
# join and summarize site and population available habitat
avail_hab_summ = dabom_site_sf %>%
  select(site_code) %>%
  left_join(site_avail_hab, by = "site_code") %>%
  select(site_code,
         chnk_ip_length_w,
         chnk_ip_length_curr,
         chnk_qrf_n,
         sthd_ip_length_w,
         sthd_ip_length_curr,
         sthd_qrf_n) %>%
  # join trt population names for each site
  st_join(chnk_pops %>%
            select(chnk_pop = TRT_POPID)) %>%
  st_join(sthd_pops %>%
            select(sthd_pop = TRT_POPID)) %>% ### I NEED TO COME BACK AND FIX THE POPULATIONS FOR SELECT SITES
  st_drop_geometry() %>%
  # fix chinook population assignments for some sites
  mutate(chnk_pop = case_when(
    site_code %in% c("SC1", "SC2") ~ "SCUMA",
    # note that UGR monitors both GRUMA and GRCAT
    site_code == "UGR" ~ "GRUMA",
    TRUE ~ chnk_pop
  )) %>%
  # fix steelhead population assignments for some sites
  mutate(sthd_pop = case_when(
    site_code %in% c("SC1", "SC2") ~ "CRSFC-s",
    TRUE ~ sthd_pop
  )) %>%
  # join available habitat for chinook populations
  left_join(pop_avail_hab %>%
              select(pop,
                     chnk_pop_ip_length_w = ip_length_w,
                     chnk_pop_ip_length_curr = ip_length_curr,
                     chnk_pop_qrf_n = qrf_n),
            by = c("chnk_pop" = "pop")) %>%
  # join available habitat for steelhead populations
  left_join(pop_avail_hab %>%
              select(pop,
                     sthd_pop_ip_length_w = ip_length_w,
                     sthd_pop_ip_length_curr = ip_length_curr,
                     sthd_pop_qrf_n = qrf_n),
            by = c("sthd_pop" = "pop")) %>%
  # calculate proportions of available habitat for each site (cap each proportion at 1)
  mutate(
    p_chnk_ip_length_w = pmin(chnk_ip_length_w / chnk_pop_ip_length_w, 1),
    p_chnk_ip_length_curr = pmin(chnk_ip_length_curr / chnk_pop_ip_length_curr, 1),
    p_chnk_qrf_n = pmin(chnk_qrf_n / chnk_pop_qrf_n, 1),
    p_chnk_avg = (p_chnk_ip_length_curr + p_chnk_qrf_n) / 2,
    p_sthd_ip_length_w = pmin(sthd_ip_length_w / sthd_pop_ip_length_w, 1),
    p_sthd_ip_length_curr = pmin(sthd_ip_length_curr / sthd_pop_ip_length_curr, 1),
    p_sthd_qrf_n = pmin(sthd_qrf_n / sthd_pop_qrf_n, 1),
    p_sthd_avg = (p_sthd_ip_length_curr + p_sthd_qrf_n) / 2
  ) %>%
  # replace any NA or NaN with a 1 i.e., all of the (lack of) habitat is monitored
  replace_na(list(
    p_chnk_ip_length_w = 1,
    p_chnk_ip_length_curr = 1,
    p_chnk_qrf_n = 1,
    p_chnk_avg = 1,
    p_sthd_ip_length_w = 1,
    p_sthd_ip_length_curr = 1,
    p_sthd_qrf_n = 1
  )) %>%
  select(site_code,
         chnk_pop,
         p_chnk_ip_length_curr,
         p_chnk_qrf_n,
         p_chnk_avg,
         sthd_pop,
         p_sthd_ip_length_curr,
         p_sthd_qrf_n,
         p_sthd_avg)

# save the important objects
save(site_avail_hab,
     pop_avail_hab,
     avail_hab_summ,
     file = here("output/available_habitat/snake_available_habitat.rda"))

# save the important objects to excel
# library(writexl)
# write_xlsx(x = list(site_avail_hab, pop_avail_hab, avail_hab_summ),
#            path = here("output/available_habitat/snake_available_habitat.xlsx"))
  
# explore differences in proportions of IP and QRF Redd Habitat
# ggplot(avail_hab_summ, aes(x = p_sthd_qrf_n, y = p_sthd_ip_length_curr)) +
#   geom_point(color = "blue") +
#   geom_abline(a = 0, b = 1) +
#   geom_text(aes(label = site_code)) +
#   labs(x = "p(QRF Redd Capacity)",
#        y = "p(IP Habitat)",
#        title = "Scatterplot of proportion of steelhead habitat above IPTDS (IP vs. QRF).") +
#   theme_minimal()
# 
# ggplot(avail_hab_summ, aes(x = p_chnk_qrf_n, y = p_chnk_ip_length_curr)) +
#   geom_point(color = "blue") +
#   geom_abline(a = 0, b =1) +
#   geom_text(aes(label = site_code)) +
#   labs(x = "p(QRF Redd Capacity)",
#        y = "p(IP Habitat)",
#        title = "Scatterplot of proportion of Chinook salmon habitat above IPTDS (IP vs. QRF).") +
#   theme_minimal()

### END SCRIPT