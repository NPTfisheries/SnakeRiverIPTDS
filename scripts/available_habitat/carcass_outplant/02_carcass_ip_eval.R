# -----------------------
# Author: Mike Ackerman
# Purpose: 
# 
# Created: May 28, 2025
#   Last Modified: 
# 
# Notes:

# clear environment
rm(list = ls())

# load necessary packages
library(here)

# load carcass study streams
load(file = here("output/carcass_outplant_study/carcass_study_streams.rda"))

# load the prepped intrinsic potential and redd qrf datasets
load(file = here("data/spatial/prepped_snake_ip.rda"))

# convert streams_sf to long form for below analysis
streams_sf_long = streams_sf %>%
  pivot_longer(cols = c("sthd_popid", "chnk_popid"),
               names_to = "spc_code",
               values_to = "popid") %>%
  mutate(spc_code = str_sub(spc_code, 1, 4))

# create empty data frame to store results for each site
streams_ip_df = NULL
for (s in 1:nrow(streams_sf_long)) {
  
  # grab the site_code, spc_code, and polygons for each combination
  site_code = streams_sf_long[s,] %>% pull(site_code)
  spc_code  = streams_sf_long[s,] %>% pull(spc_code)
  
  stream_poly = get(load(paste0(here("output/carcass_outplant_study/carcass_outplant_polygons"), "/", spc_code, "/", site_code, ".rda")))
  
  cat(paste0("Estimating available habitat within stream ", site_code, ", ", spc_code, ".\n"))
  
  # summarize intrinsic potential habitat for each site polygon
  stream_ip = ip_sf %>%
    st_intersection(stream_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc_code == "chnk") {
        summarise(.,
                  length_wip_m = sum(length_w_chnk, na.rm = TRUE),
                  avg_width_m  = mean(wide_ww[chnk_wt != 0], na.rm = TRUE),
                  tot_length_m = sum(length_m, na.rm = TRUE),
                  tot_area_m2  = sum(area_ww, na.rm = TRUE),
                  area_non0_m2 = sum(area_ww[chnk_wt != 0], na.rm = TRUE),
                  area_wip_m2  = sum(area_w_chnk, na.rm = TRUE),
                  .groups = "drop")
      } else if (spc_code == "sthd") {
        summarise(.,
                  length_wip_m = sum(length_w_sthd, na.rm = TRUE),
                  avg_width_m  = mean(wide_bf[sthd_wt != 0], na.rm = TRUE),
                  tot_length_m = sum(length_m, na.rm = TRUE),
                  tot_area_m2  = sum(area_bf, na.rm = TRUE),
                  area_non0_m2 = sum(area_bf[sthd_wt != 0], na.rm = TRUE),
                  area_wip_m2  = sum(area_w_sthd, na.rm = TRUE),
                  .groups = "drop")
      }
    } %>%
    mutate(site_code = site_code,
           spc_code = spc_code) %>%
    select(site_code, spc_code, everything())
  
  # append ip results to site_ip_df
  streams_ip_df = bind_rows(streams_ip_df, stream_ip)
  
} # end site loop

# get available habitat for populations
load(here("output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda")) ; rm(site_avail_hab, avail_hab_df)

#--------------------
# escapement goals
esc_goal_df = tribble(
  ~popid, ~low, ~med, ~high,
  "CRLAP", 750, 1875, 3000, 
  "SCLAW", 500, 1250, 2000,
  "SCUMA",1000, 2500, 4000,
  "CRLOL", 500, 1250, 2000,
  "CRLOC",1000, 2500, 4000,
  "SEMEA", 500, 1250, 2000,
  "CRLMA-s", 1500, 4500, 7500,
  "CRSFC-s", 1000, 3000, 5000,
  "CRLOL-s",  500, 1500, 2500,
  "CRLOC-s", 1000, 3000, 5000,
  "CRSEL-s", 1000, 3000, 5000
)

stream_order = c("Lolo_Cr",
                 "Red_R",
                 "American_R",
                 "Crooked_R",
                 "Newsome_Cr",
                 "Meadow_Cr_SFCW",
                 "OHara_Cr",
                 "Lapwai_All",
                 "Big_Canyon_Cr")

# estimate proportion ip within potential study stream
carcass_ip_df = streams_ip_df %>%
  filter(spc_code == "chnk") %>%
  left_join(streams_sf_long %>% st_drop_geometry(),
            by = join_by(site_code, spc_code)) %>%
  left_join(pop_avail_hab %>%
              select(popid,
                     spc_code,
                     pop_area_wip_m2 = ip_area_w),
            by = join_by(popid, spc_code)) %>%
  mutate(p_ip = area_wip_m2 / pop_area_wip_m2) %>%
  left_join(esc_goal_df %>%
              select(popid, high),
            by = join_by(popid)) %>%
  mutate(est_hist_n = high * p_ip,
         est_hist_kg = case_when(
           spc_code == "chnk" ~ est_hist_n * 5,
           spc_code == "sthd" ~ est_hist_n * 3.48,
           TRUE ~ est_hist_n * NA
         ),
         kg_per_total_m2 = est_hist_kg / tot_area_m2,
         kg_per_wip_m2   = est_hist_kg / area_wip_m2) %>%
  select(site_code, 
         spc_code,
         popid,
         avg_width_m,
         high_esc_goal = high,
         p_ip,
         est_hist_n,
         est_hist_kg,
         kg_per_total_m2,
         kg_per_wip_m2) %>%
  filter(site_code %in% stream_order) %>%
  mutate(site_code = factor(site_code, levels = stream_order)) %>%
  arrange(site_code)

reach_length = 1000 # meters
treatment_kg_df = carcass_ip_df %>%
  select(site_code,
         popid,
         avg_width_m,
         high_esc_goal,
         p_ip,
         est_hist_n,
         est_hist_kg) %>%
  mutate(treatment = case_when(
    site_code %in% c("Lolo_Cr", "Crooked_R") ~ "TH",
    site_code %in% c("Red_R", "Newsome_Cr")  ~ "TL",
    site_code %in% c("American_R", "Meadow_Cr_SFCW", "OHara_Cr", "Big_Canyon_Cr") ~ "C"
  )) %>%
  mutate(treatment_p = case_when(
    treatment == "TH" ~ 2,
    treatment == "TL" ~ 1,
    treatment == "C"  ~ 0
  ),
  treatment_kg = est_hist_kg * treatment_p,
  kg_m2 = treatment_kg / (reach_length * avg_width_m))

sum(treatment_kg_df$treatment_kg, na.rm = T)

# treatment_ip_df = carcass_ip_df %>%
#   select(site_code,
#          spc_code,
#          popid,
#          kg_per_wip_m2,
#          avg_width_m) %>%
#   mutate(reach_m2 = reach_length * avg_width_m,
#          kg = kg_per_wip_m2 * reach_m2,
#          n_carcasses = kg / 5.3) # approximate weighted average carcass weight
# 
# treatment_lit_df = carcass_ip_df %>%
#   select(site_code,
#          spc_code,
#          popid,
#          avg_width_m) %>%
#   mutate(est_reach_m2 = reach_length * avg_width_m,
#          treatment = case_when(
#            site_code %in% c("Lolo_Cr", "Crooked_R") ~ "TH",
#            site_code %in% c("Red_R", "Newsome_Cr")  ~ "TL",
#            site_code %in% c("American_R", "Meadow_Cr_SFCW", "OHara_Cr", "Big_Canyon_Cr") ~ "C"
#   )) %>%
#   mutate(treatment_kg_m2 = case_when(
#     treatment == "TH" ~ 0.5,
#     treatment == "TL" ~ 0.25,
#     treatment == "C"  ~ 0
#   )) %>%
#   mutate(kg = est_reach_m2 * treatment_kg_m2,
#          n_carcasses = kg / 5.3)
# 
# sum(treatment_lit_df$kg, na.rm = T)
# sum(treatment_lit_df$n_carcasses, na.rm = T)

# join and summarize site and population available habitat
# p_ip_df = site_ip_df %>%
#   left_join(sites_sf_long,
#             by = c("site_code", "spc_code")) %>%
#   dplyr::select(site_code,
#                 spc_code,
#                 popid,
#                 site_ip_length_w = ip_length_w) %>%
#   left_join(pop_avail_hab %>%
#               dplyr::select(spc_code,
#                             popid,
#                             pop_ip_length_w = ip_length_w),
#             by = c("spc_code", "popid")) %>%
#   mutate(p_ip = site_ip_length_w / pop_ip_length_w) %>%
#   left_join(esc_goal_df %>%
#               dplyr::select(popid,
#                             high)) %>%
#   mutate(est_hist_abund = p_ip * high) %>%
#   dplyr::select(site_code,
#                 spc_code,
#                 popid,
#                 high_esc_goal = high,
#                 p_ip,
#                 est_hist_abund)

# write results
# write_csv(p_ip_df,
#           file = here("output/available_habitat/carcass_outplant_streams.csv"))

### END SCRIPT