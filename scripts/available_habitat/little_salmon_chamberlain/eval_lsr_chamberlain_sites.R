# -----------------------
# Author: Mike Ackerman
# Purpose: Delineate watershed polygons for theoretical or proposed sites
# 
# Created: May 8, 2025
#   Last Modified:
# 
# Notes:

# clear environment
rm(list = ls())

# load necessary packages
library(sf)
library(tidyverse)
library(here)
library(raster)
#install.packages("whitebox", repos = "http://R-Forge.R-project.org")
library(whitebox)
library(stars)

# set some defaults
ws_dir = "C:/Workspace/gis/10m_NED_DEMs/" # file path to spatial data
default_crs = st_crs(32611)               # set default crs: WGS 84, UTM zone 11N

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# prep theoretical sites to be evaluated
sites_sf = tribble(
  ~site_code, ~stream, ~sthd_popid, ~chnk_popid, ~latitude, ~longitude,
  "LSR_IDFG", "Little Salmon River", "SRLSR-s", "SRLSR", 45.3954849, -116.3383546,
  "Slate_Creek_Private", "Slate Creek", "SRLSR-s", "SRLSR", 45.6365, -116.24539,
  "Slate_Creek_Work_Center", "Slate Creek", "SRLSR-s", "SRLSR", 45.63387, -116.19882,
  "NF_Slate_CG", "Slate Creek", "SRLSR-s", "SRLSR", 45.638971, -116.119425,
  "Chamberlain_Airstrip_IDFG", "Chamberlain Creek", "SRCHA-s", "SRCHA", 45.37311, -115.187,
  "Poet_Creek_CG", "Bargamin Creek", "SRCHA-s", "SRCHA", 45.723186, -115.03290,
  "Shepp_Ranch", "Crooked Creek", "SRCHA-s", "SFSMA", 45.4385, -115.66422,
  "Warren_Wagon_Road", "Warren Creek", "SRCHA-s", "SFSMA", 45.27651, -115.69798,
  "French_Creek_Public", "French Creek", "SRCHA-s", "SFSMA", 45.39768, -116.03409,
  "Partridge_Creek_Public", "Partridge Creek", "SRCHA-s", "SFSMA", 45.40547, -116.13028,
  "Van_Creek_CG", "Kelly Creek", "SRCHA-s", "SFSMA", 45.42559, -116.1329,
  "Lake_Creek_Public", "Lake Creek", "SRCHA-s", "SFSMA", 45.38624, -116.21673
) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%          # got my waypoints using WGS 84
  st_transform(crs = default_crs)   # convert to UTM Zone 11N

# read in prepped DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

# begin species loop
for (spc in c("chnk", "sthd")) {
  
  spc_site_pops = sites_sf %>%
    dplyr::select(site_code,
                  popid = starts_with(spc)) %>%
    st_drop_geometry()
  
  # get the population polygons for the given species
  if(spc == "chnk") { spc_pops = chnk_pops }
  if(spc == "sthd") { spc_pops = sthd_pops }
  
  # begin site loop
  for (s in 1:nrow(spc_site_pops)) {
    
    # grab the site and population
    site = spc_site_pops[s,]
    pop = site$popid
    
    cat(paste0("Creating the watershed polygon for ", spc, ", site ", site$site_code, ".\n"))
    
    # get the population polygon
    poly = spc_pops %>%
      filter(TRT_POPID %in% pop) %>%
      dplyr::select(popid = TRT_POPID) %>%
      # to accommodate sites that cover multiple populations
      summarise(
        pop = paste(pop, collapse = "/"),
        geometry = st_union(geometry)
      )
    
    # check to see if the raster streams for a population polygon already exists; if not, run loop
    if(file.exists(paste0(ws_dir, "raster_streams/", spc, "/", paste(pop, collapse = "_"), ".tif"))) {
      
      # if the file exists, print a message and skip the loop
      cat(paste0("The raster streams file for population ", paste(pop, collapse = "_"), " already exists. Skipping the loop.\n"))
      
    } else {
      
      # if the file does not exist, perform the watershed delineation, etc.
      cat(paste0("The raster streams file for population ", paste(pop, collapse = "_"), " does not exist. Running the loop.\n"))
      
      # clip DEM using the population polygon
      pop_dem = crop(snake_dem, poly)
      
      # write population dem
      writeRaster(pop_dem, paste0(ws_dir, "pop_dems/", spc, "/", paste(pop, collapse = "_"), ".tif"), overwrite = TRUE)
      
      # breach depressions
      wbt_breach_depressions_least_cost(
        dem = paste0(ws_dir, "pop_dems/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        output = paste0(ws_dir, "pop_dems_breached/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        dist = 5,
        fill = TRUE
      )
      
      # fill depressions
      wbt_fill_depressions_wang_and_liu(
        dem = paste0(ws_dir, "pop_dems_breached/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        output = paste0(ws_dir, "pop_dems_breached_filled/", spc, "/", paste(pop, collapse = "_"), ".tif")
      )
      
      # create D8 flow accumulation
      wbt_d8_flow_accumulation(
        input = paste0(ws_dir, "pop_dems_breached_filled/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        output = paste0(ws_dir, "d8fa/", spc, "/", paste(pop, collapse = "_"), ".tif")
      )
      
      # create D8 pointer file
      wbt_d8_pointer(dem = paste0(ws_dir, "pop_dems_breached_filled/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                     output = paste0(ws_dir, "d8pointer/", spc, "/", paste(pop, collapse = "_"), ".tif"))
      
      # extract streams
      wbt_extract_streams(
        flow_accum = paste0(ws_dir, "d8fa/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        output = paste0(ws_dir, "raster_streams/", spc, "/", paste(pop, collapse = "_"), ".tif"),
        threshold = 6000
      )
    } # end breach and fill depressions, create D8 flow accumulation and pointer files, extract streams loop
    
    # set pour point
    pp = sites_sf %>%
      filter(site_code == site$site_code) %>%
      dplyr::select(geometry) %>%
      distinct() %>%
      # convert the sf point to a SpatialPoints object
      as("Spatial")
    
    # create shapefile of pour point
    raster::shapefile(pp, filename = paste0(ws_dir, "theoretical_sites/pour_points/", site$site_code, ".shp"), overwrite = TRUE)
    
    # snap pour points to raster stream
    wbt_jenson_snap_pour_points(pour_pts = paste0(ws_dir, "theoretical_sites/pour_points/", site$site_code, ".shp"),
                                streams = paste0(ws_dir, "raster_streams/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                                output = paste0(ws_dir, "theoretical_sites/snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                                snap_dist = 100)
    
    # delineate watershed
    wbt_watershed(d8_pntr = paste0(ws_dir, "d8pointer/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                  pour_pts = paste0(ws_dir, "theoretical_sites/snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                  output = paste0(ws_dir, "theoretical_sites/watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    
    # convert watershed from raster to vector
    ws_raster = raster(paste0(ws_dir, "theoretical_sites/watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    ws_vector = st_as_stars(ws_raster) %>%
      st_as_sf(merge = T)
    
    # finally, clip the extent of the watershed polygon using the population polygon in the rare case (e.g., USE) that it extends beyond  
    ws_vector_clip = st_intersection(ws_vector, poly)
    
    # write vector watershed
    save(ws_vector_clip, file = paste0(here("output/theoretical_site_polygons"), "/", spc, "/", site$site_code, ".rda"))
    st_write(ws_vector_clip, paste0(ws_dir, "theoretical_sites/watershed_polygons/", spc, "/", site$site_code, ".shp"), quiet = TRUE, append = FALSE)
    
  } # end loop over sites
} # end loop over species


# load the prepped intrinsic potential and redd qrf datasets
load(file = here("data/spatial/prepped_snake_ip.rda"))
qrf_sf = get(load(file = here("data/spatial/snake_redd_qrf.rda")))

# pivot_longer sf to prepare for below analysis
sites_sf_long = sites_sf %>%
  pivot_longer(cols = c("sthd_popid", "chnk_popid"),
               names_to = "spc_code",
               values_to = "popid") %>%
  mutate(spc_code = str_sub(spc_code, 1, 4))

# create empty data frame to store results for each site
site_hab_df = NULL
for (s in 1:nrow(sites_sf_long)) {
  
  # grab the site_code, spc_code, and polygons for each combination
  site_code = sites_sf_long[s,] %>% pull(site_code)
  spc_code  = sites_sf_long[s,] %>% pull(spc_code)
  
  site_poly = get(load(paste0(here("output/theoretical_site_polygons"), "/", spc_code, "/", site_code, ".rda")))

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
        #filter(., chnk == TRUE & !is.na(chnk_use)) %>%
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
    dplyr::select(site_code, spc_code, everything())
  
  # append ip and qrf results to site_avail_hab
  site_hab_df = bind_rows(site_hab_df, site_ip_qrf)
  
} # end site loop

# get available habitat for populations
load(here("output/available_habitat/snake_river_iptds_and_pop_available_habitat.rda")) ; rm(site_avail_hab, avail_hab_df)

#--------------------
# join and summarize site and population available habitat
theoretical_sites_df = site_hab_df %>%
  left_join(sites_sf_long,
            by = c("site_code", "spc_code")) %>%
  dplyr::select(site_code,
                stream,
                spc_code, 
                popid,
                site_ip_length_w_curr = ip_length_w_curr,
                site_qrf_n = qrf_n,
                site_qrf_n_se = qrf_n_se,
                -geometry) %>%
  left_join(pop_avail_hab %>%
              dplyr::select(spc_code,
                            popid,
                            pop_ip_length_w_curr = ip_length_w_curr,
                            pop_qrf_n = qrf_n,
                            pop_qrf_n_se = qrf_n_se),
            by = c("spc_code", "popid")) %>%
  mutate(
    p_ip_length_w_curr = site_ip_length_w_curr / pop_ip_length_w_curr,
    p_qrf_n = site_qrf_n / pop_qrf_n
    # standard error of the proportion using the delta method
    #p_qrf_n_se = p_qrf_n * sqrt((site_qrf_n_se / site_qrf_n)^2 + (pop_qrf_n_se / pop_qrf_n)^2)
  ) %>%
  dplyr::select(site_code,
                stream,
                spc_code,
                popid,
                p_ip_length_w_curr,
                p_qrf_n,
                site_qrf_n) %>%
  arrange(spc_code, popid, -p_qrf_n)

# plot some results
ts_df = theoretical_sites_df %>%
  mutate(p_hab = (p_ip_length_w_curr + p_qrf_n) / 2) %>%
  select(site_code, spc_code, popid, p_hab) %>%
  group_by(spc_code) %>%
  mutate(site_code = fct_reorder(site_code, p_hab, .desc = TRUE)) %>%
  ungroup()

# plot for steelhead
p1 = ts_df %>%
  filter(spc_code == "sthd") %>%
  ggplot(aes(x = site_code, y = p_hab)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~popid, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Steelhead", x = NULL, y = "p(Habitat)") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for chinook
p2 = ts_df %>%
  filter(spc_code == "chnk") %>%
  ggplot(aes(x = site_code, y = p_hab)) +
  geom_col(fill = "darkorange") +
  facet_wrap(~popid, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Chinook", x = NULL, y = "p(Habitat)") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# combine with patchwork
ts_p = p1 / p2 

# save plot
ggsave("output/figures/available_habitat/proposed_site_proportion_habitat.pdf",
       plot = ts_p,
       width = 8,
       height = 11.5,
       units = "in")

# save the results
write_csv(theoretical_sites_df,
          file = here("output/available_habitat/proposed_sites_available_habitat_summary.csv"))


