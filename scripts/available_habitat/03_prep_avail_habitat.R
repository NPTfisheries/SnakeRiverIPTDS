# -----------------------
# Author: Mike Ackerman
# Purpose: Prep and save the intrinsic potential and qrf datasets prior
#   to estimating the available habitat in ICTRT populations and above IPTDS.
# 
# Created: August 1, 2024
#   Last Modified: November 5, 2024
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

# prep intrinsic potential layer; already been clipped using snake river steelhead DPS
ip_sf = readRDS(here("data/spatial/ip.rds")) %>%
  clean_names() %>%
  st_transform(default_crs) %>%
  # trim down to only useful columns
  select(name,                # Unique segment identifier
         llid,                # water course longitude/latitude identifier number; derived unique water course route identifier from PNW Framework Hydrography (100k scale)
         strmname,            # common name of watercourse from the PNW Framework Hydrography (100k scale)
         length_m = length,   # segment length (m)
         elev_m = elev,       # mean elevation of stream segment (m), calculated from USGS 10m DEM
         wide_ww,             # modeled wetted width of stream (summer minimum) (m)
         wide_bf,             # modeled bankfull width of stream, (m)
         gradient,            # % gradient of stream segment, calculated from USGS DEM
         sthdrate,            # Steelhead IP habitat rating value; 0 = none/very low, 1 = low, 2 = moderate, 3 = high quality
         chinrate,            # Spring/Summer Chinook IP habitat rating value; 0 = none/very low, 1 = low, 2 = moderate, 3 = high quality
         currsthd = currsush, # if value > 0 = current summer steelhead spawning, from state agencies, streamnet, observation, and expert opinion
         currspch,            # if value > 0 = current Spring Chinook spawning, from state agencies, streamnet, observation, and expert opinion
         currsuch) %>%        # if value > 0 = current Summer Chinook spawning, from state agencies, streamnet, observation, and expert opinion
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
  filter(!(chnk_wt == 0 & sthd_wt == 0))

# save the prepped intrinsic potential layer
save(ip_sf, file = here("data/spatial/prepped_snake_ip.rda"))
#st_write(ip_sf, here("data/spatial/prepped_snake_ip.gpkg"), layer = "prepped_ip", delete_dsn = T)

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop, spsm_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)

# prep the qrf data
qrf_sf = st_read("D:/NAS/data/qrf/gitrepo_data/output/gpkg/Rch_Cap_RF_No_elev_redds.gpkg") %>%
  clean_names() %>%
  st_transform(default_crs) %>%
  select(unique_id,
         gnis_name,
         reach_leng_m = reach_leng,
         chnk,
         chnk_use,
         sthd,
         sthd_use,
         chnk_per_m,
         chnk_per_m_se,
         sthd_per_m,
         sthd_per_m_se) %>%
  # keep only reaches used by either sp/sum chinook or steelhead (according to StreamNet)
  filter(chnk == TRUE | sthd == TRUE) %>%
  # trim the qrf data to the extent of snake river steelhead populations
  st_intersection(sthd_pops %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) %>%
  # the chnk_use and sthd_use designations are FAR from perfect, but this at least gets rid of some mainstem reaches
  filter(!(chnk_use == "Migration only" & sthd_use == "Migration only"))

# save the prepped qrf dataset
save(qrf_sf, file = here("data/spatial/snake_redd_qrf.rda"))

### END SCRIPT
