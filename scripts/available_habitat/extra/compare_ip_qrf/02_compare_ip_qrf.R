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
library(magrittr)

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
qrf_sf_w_ip_attr %<>%
  # trim to only reaches used by either sp/sum chinook or steelhead (according to StreamNet), just to quicken later work
  filter(chnk == TRUE | sthd == TRUE)

# create sf of trt pops
pop_df = chnk_pops %>%
  select(popid = TRT_POPID) %>%
  mutate(spc = "chnk") %>%
  bind_rows(sthd_pops %>%
              select(popid = TRT_POPID) %>%
              mutate(spc = "sthd"))

pop_ip_qrf_df = NULL
for (p in 1:nrow(pop_df)) {
  
  # grab the spc_code and polygons for each population
  spc   = pop_df[p,] %>% pull(spc)
  popid = pop_df[p,] %>% pull(popid)
  
  cat(paste0("Estimating available habitat in population ", popid, ".\n"))
  
  # get the population polygon
  if(spc == "chnk") {
    pop_poly = chnk_pops %>%
      filter(TRT_POPID %in% popid) %>%
      mutate(TRT_POPID = factor(TRT_POPID, levels = popid)) %>%
      arrange(TRT_POPID) %>%
      select(popid = TRT_POPID)
  }
  if(spc == "sthd") {
    pop_poly = sthd_pops %>%
      filter(TRT_POPID %in% popid) %>%
      mutate(TRT_POPID = factor(TRT_POPID, levels = popid)) %>%
      arrange(TRT_POPID) %>%
      select(popid = TRT_POPID)
  }
  
  # summarize intrinsic potential habitat for each population polygon
  pop_ip_qrf = qrf_sf_w_ip_attr %>%
    st_intersection(pop_poly) %>%
    st_drop_geometry() %>%
    {
      if (spc == "chnk") {
        filter(., chnk == TRUE & chnk_use == "Spawning and rearing") %>%
        summarise(.,
                  ip_length_m = sum(reach_leng_m),
                  ip_length_w = sum(length_w_chnk_ip, na.rm = T),
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(chnk_per_m * reach_leng_m),
                  .groups = "drop")
      } else if (spc == "sthd") {
        filter(., sthd == TRUE & sthd_use == "Spawning and rearing") %>%
        summarise(.,
                  ip_length_m = sum(reach_leng_m),
                  ip_length_w = sum(length_w_sthd_ip, na.rm = T),
                  qrf_length_m = sum(reach_leng_m),
                  qrf_n = sum(sthd_per_m * reach_leng_m),
                  .groups = "drop")
      }
    } %>%
    mutate(spc = spc,
           popid = popid) %>%
    select(spc, popid, everything())
  
  # append ip and qrf results to site_avail_hab
  pop_ip_qrf_df = bind_rows(pop_ip_qrf_df, pop_ip_qrf)

} # end population loop

# explore differences in the total available IP vs. redd QRF habitat in trt populations 
# when using StreamNet spatial extents for both
pop_ip_qrf_df %>%
  ggplot(aes(x = qrf_n,
             y = ip_length_w,
             color = spc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text_repel(aes(label = popid), size = 3) +
  labs(x = "QRF",
       y = "IP",
       title = "Scatterplot of available IP vs. QRF habitat using StreamNet spawning extents.") +
  theme_minimal()

ggsave(here("output/figures/available_habitat/pop_ip_vs_qrf_streamnet.png"),
       width = 11,
       height = 8,
       dpi = 300)

### END SCRIPT
  