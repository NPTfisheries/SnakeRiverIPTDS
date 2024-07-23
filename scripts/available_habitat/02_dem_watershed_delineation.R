# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to delineate watersheds using a prepped DEM. 
#   More details to come later.
# 
# Created: July 22, 2024
#   Last Modified: 
# 
# Notes:

# clear environment
rm(list = ls())

# load necessary packages
library(tidyverse)
library(sf)
library(here)
library(raster)
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
library(whitebox)

# set some defaults
ws_dir = "C:/Workspace/gis/10m_NED_DEMs/" # file path to spatial data
default_crs = st_crs(32611)               # set default crs: WGS 84, UTM zone 11N

#--------------------
# load and prep data

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
  select(site_code) %>%
  # transform to WGS84
  st_transform(default_crs)

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# join ictrt populations to iptds
iptds_sf = iptds_sf %>%
  st_join(sthd_pops %>%
            select(sthd = TRT_POPID)) %>%
  st_join(chnk_pops %>%
            select(chnk = TRT_POPID)) %>%
  pivot_longer(cols = c("sthd", "chnk"),
               names_to = "species",
               values_to = "pop")

# read in DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

#--------------------
# begin loop
for (s in 1:nrow(iptds_sf)) {
  
  # grab the site and population
  site = iptds_sf[s,] %>% st_drop_geometry()
  pop = site$pop
  
  # get the population polygon
  if(site$species == "sthd") {
    poly = sthd_pops %>%
      filter(TRT_POPID == pop) %>%
      select(pop = TRT_POPID)
  }
  if(site$species == "chnk") {
    poly = chnk_pops %>%
      filter(TRT_POPID == pop) %>%
      select(pop = TRT_POPID)
  }
  
  # clip DEM using the population polygon
  pop_dem = crop(snake_dem, poly)
  
  # write population dem
  writeRaster(pop_dem, paste0("C:/Workspace/gis/10m_NED_DEMs/pop_dems/", pop, ".tif"), overwrite = TRUE)
  
  
  
}

library(stars)
ggplot() +
  geom_stars(tmp) +
  scale_fill_viridis_c() +
  geom_sf(poly, fill = "transparent")

### END SCRIPT

# load necessary packages
library(sf)
library(raster)
library(tmap)
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
library(whitebox)

# set some defaults
ws_dir = "C:/Workspace/gis/10m_NED_DEMs/" # file path to spatial data
#default_crs = st_crs(32611)               # set default crs to WGS 84, UTM zone 11N

# read in DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

# set tmap mode to interactive viewing
#tmap_mode("view")

# set values below 1500 to NA since they are artifacts around the edges
#snake_dem[snake_dem < 1500] = NA # it's unclear whether this is necessary

# plot dem to be sure everything is ok
# tm_shape(snake_dem) +
#   tm_raster(style = "cont", palette = "PuOr", legend.show = T) +
#   tm_scale_bar()

#------------------------------------
# prepare dem for hydrology analyses

# breach depressions
wbt_breach_depressions_least_cost(
  dem = paste0(ws_dir, "snake_river_10m_ned_dem.tif"),
  output = paste0(ws_dir, "snake_river_dem_breached.tif"),
  dist = 5,
  fill = TRUE
)