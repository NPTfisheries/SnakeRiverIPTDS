# -----------------------
# Author: Mike Ackerman
# Purpose: Attempt to join useful fields from the IP dataset to the redd QRF dataset
# 
# Created: December 14, 2024
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

# prep intrinsic potential layer; already been clipped using snake river steelhead dps
ip_sf = readRDS(here("data/spatial/ip.rds")) %>%
  clean_names() %>%
  st_transform(default_crs) %>%
  # rename a few columns to match column names used in other analyses
  select(name,
         llid,
         strmname,
         length_m = length,   # segment length (m)
         elev_m = elev,
         flowstat,
         wide_ww,
         wide_bf,
         gradient,
         bf_fld_rat,
         bf_fld_ra2,
         bf_fld_ra3,
         per_for,
         blocktype,
         blockgrad,
         blocknatu,
         maxwaterb,
         inlake,
         sediment,
         velocode,
         ratecodec,
         ratecodes,
         sthdrate,
         chinrate,
         currsthd = currsush,
         currspch,
         currsuch,
         chinrear,
         sthdrear) %>%
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
  select(everything(), geometry)

# plot the intrinsic potential data
ggplot() +
  geom_sf(data = ip_sf,
          color = "dodgerblue",
          size = 1) +
  geom_sf(data = sthd_pops,
          fill = "gray90",
          color = "black",
          alpha = 0.5) +
  labs(title = "Intrinsic Potential with Steelhead Populations") +
  theme_minimal()

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
         model,
         chnk_per_m,
         chnk_per_m_se,
         chnk_per_m2,
         chnk_per_m2_se,
         sthd_per_m,
         sthd_per_m_se,
         sthd_per_m2,
         sthd_per_m2_se,
         geometry = geom) %>%
  # keep only reaches used by either sp/sum chinook or steelhead (according to StreamNet)
  #filter(chnk == TRUE | sthd == TRUE) %>%
  # trim the qrf data to the extent of snake river steelhead populations
  st_intersection(sthd_pops %>%
                    st_union() %>%
                    nngeo::st_remove_holes()) #%>%
  # the chnk_use and sthd_use designations are FAR from perfect, but this at least gets rid of some mainstem reaches
  #filter(!(chnk_use == "Migration only" & sthd_use == "Migration only"))

# convert qrf polylines to points
qrf_sf_midpts = qrf_sf %>%
  st_cast("LINESTRING") %>%
  select(unique_id) %>%
  # midpoint of each line segment
  #mutate(midpoint = st_line_sample(geometry, sample = 0.5)) %>%
  mutate(geometry = st_centroid(geometry))
  #st_cast("POINT")

# plot the results of the point casting
ggplot() +
  geom_sf(data = qrf_sf, aes(color = "Line Segments"), size = 1) +
  geom_sf(data = qrf_sf_midpts, aes(color = "Midpoints"), size = 0.1) +
  # Customize the plot
  scale_color_manual(values = c("Line Segments" = "blue", "Midpoints" = "red")) +
  labs(title = "Midpoints of Line Segments",
       color = "Legend") +
  theme_minimal()

# find the nearest ip polyline for each qrf midpoint
nearest_ip_id = st_nearest_feature(qrf_sf_midpts, ip_sf)

# add attributes from ip_sf to the midpoints
ip_4_qrf = qrf_sf_midpts %>%
  mutate(nearest_ip_id = nearest_ip_id) %>%
  left_join(ip_sf %>%
              st_drop_geometry() %>%
              mutate(nearest_ip_id = row_number()),
            by = "nearest_ip_id") %>%
  select(-nearest_ip_id) %>%
  st_drop_geometry() %>%
  rename_with(
    .fn = ~ paste0(., "_ip"),       # Add "_ip" suffix
    .cols = -c(unique_id)           # Exclude "unique_id" column
  )

# join resulting ip_4_qrf dataset to the qrf_sf object
qrf_sf_w_ip_attr = qrf_sf %>%
  left_join(ip_4_qrf,
            by = "unique_id")

# save the prepped qrf dataset
save(qrf_sf_w_ip_attr, file = here("data/spatial/full_snake_redd_qrf_w_ip_attr.rda"))

### END SCRIPT
