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
library(stars)

# set some defaults
ws_dir = "C:/Workspace/gis/10m_NED_DEMs/" # file path to spatial data
default_crs = st_crs(32611)               # set default crs: WGS 84, UTM zone 11N

#--------------------
# load and prep data

# snake river iptds
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240304.rda")
rm(configuration, node_paths, parent_child, pc_nodes)

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

iptds_sf = sites_sf %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  # note this excludes some sites below lgr, but those likely aren't appropriate for expansions anyways
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>% 
  # grab only INT sites
  filter(site_type == "INT") %>%
  select(site_code) %>%
  # transform to WGS84
  st_transform(default_crs) %>%
  # join ictrt populations to iptds (just using sthd for now)
  st_join(sthd_pops %>%
            select(pop = TRT_POPID)) %>%
  arrange(site_code)

# read in prepped DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

#--------------------
# begin loop
for (s in 1:nrow(iptds_sf)) {
  
  # grab the site and population
  site = iptds_sf[s,] %>% st_drop_geometry()
  pop = site$pop

  # more information here: https://vt-hydroinformatics.github.io/rgeoraster.html#prepare-dem-for-hydrology-analyses

  # check to see if the raster streams for a population polygon already exists; if not, run loop
  if(file.exists(paste0(ws_dir, "raster_streams/", pop, ".tif"))) {
    
    # if the file exists, print a message and skip the loop
    cat(paste0("The raster streams file for population ", pop, " already exists. Skipping the loop.\n"))
    
  } else {
    
    # if the file does not exist, perform the watershed delineation, etc.
    cat(paste0("The raster streams file for population ", pop, " does not exist. Running the loop.\n"))
    
    # get the population polygon
    poly = sthd_pops %>%
      filter(TRT_POPID == pop) %>%
      select(pop = TRT_POPID)
    
    # clip DEM using the population polygon
    pop_dem = crop(snake_dem, poly)
    
    # write population dem
    writeRaster(pop_dem, paste0(ws_dir, "pop_dems/", pop, ".tif"), overwrite = TRUE)
    
    # breach depressions
    wbt_breach_depressions_least_cost(
      dem = paste0(ws_dir, "pop_dems/", pop, ".tif"),
      output = paste0(ws_dir, "pop_dems_breached/", pop, ".tif"),
      dist = 5,
      fill = TRUE
    )
    
    # fill depressions
    wbt_fill_depressions_wang_and_liu(
      dem = paste0(ws_dir, "pop_dems_breached/", pop, ".tif"),
      output = paste0(ws_dir, "pop_dems_breached_filled/", pop, ".tif")
    )
    
    # create D8 flow accumulation
    wbt_d8_flow_accumulation(
      input = paste0(ws_dir, "pop_dems_breached_filled/", pop, ".tif"),
      output = paste0(ws_dir, "d8fa/", pop, ".tif")
    )
    
    # create D8 pointer file
    wbt_d8_pointer(dem = paste0(ws_dir, "pop_dems_breached_filled/", pop, ".tif"),
                   output = paste0(ws_dir, "d8pointer/", pop, ".tif"))
    
    # extract streams
    wbt_extract_streams(
      flow_accum = paste0(ws_dir, "d8fa/", pop, ".tif"),
      output = paste0(ws_dir, "raster_streams/", pop, ".tif"),
      threshold = 6000
    )
  } # end breach and fill depressions, create D8 flow accumulation and pointer files, extract streams loop

  # set pour point
  pp = iptds_sf %>%
    filter(site_code == site$site_code) %>%
    select(geometry) %>%
    distinct() %>%
    # convert the sf point to a SpatialPoints object
    as("Spatial")
  
  # create shapefile of pour point
  #pp_sp = SpatialPoints(pp, proj4string = CRS("EPSG:32611"))
  raster::shapefile(pp, filename = paste0(ws_dir, "pour_points/", site$site_code, ".shp"), overwrite = TRUE)

  # snap pour points to raster stream
  wbt_jenson_snap_pour_points(pour_pts = paste0(ws_dir, "pour_points/", site$site_code, ".shp"),
                              streams = paste0(ws_dir, "raster_streams/", pop, ".tif"),
                              output = paste0(ws_dir, "snapped_pour_points/", site$site_code, ".shp"),
                              snap_dist = 50)
  
  # delineate watershed
  wbt_watershed(d8_pntr = paste0(ws_dir, "d8pointer/", pop, ".tif"),
                pour_pts = paste0(ws_dir, "snapped_pour_points/", site$site_code, ".shp"),
                output = paste0(ws_dir, "watershed_rasters/", pop, ".tif"))
  
  # convert watershed from raster to vector
  ws_raster = raster(paste0(ws_dir, "watershed_rasters/", pop, ".tif"))
  ws_vector = st_as_stars(ws_raster) %>%
    st_as_sf(merge = T)
  
  # write vector watershed
  save(ws_vector, file = paste0(here("output/iptds_polygons"), "/", site$site_code, ".rda"))
  st_write(ws_vector, paste0(ws_dir, "watershed_polygons/", site$site_code, ".shp"), quiet = TRUE, append = FALSE)
  
} # end loop over sites

### END SCRIPT
