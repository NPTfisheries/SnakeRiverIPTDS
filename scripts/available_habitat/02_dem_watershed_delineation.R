# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to delineate watersheds using a prepped DEM. 
#   More details to come later.
# 
# Created: July 22, 2024
#   Last Modified: April 21, 2025
# 
# Notes:

# clear environment
rm(list = ls())

# load necessary packages
library(tidyverse)
library(sf)
library(here)
library(raster)
#install.packages("whitebox", repos = "http://R-Forge.R-project.org")
library(whitebox)
library(stars)

# set some defaults
ws_dir = "C:/Workspace/gis/10m_NED_DEMs/" # file path to spatial data
default_crs = st_crs(32611)               # set default crs: WGS 84, UTM zone 11N

#--------------------
# load and prep data

# snake river iptds
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20250416.rda")
rm(configuration, parent_child, flowlines)

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# create sf object of dabom sites
sr_int_site_sf = sr_site_pops %>%
  dplyr::select(site_code, site_type, sthd_popid, chnk_popid) %>%
  filter(site_type == "INT") %>%
  st_join(crb_sites_sf %>%
              dplyr::select(rkm),
          by = "site_code") %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173)

# read in prepped DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

# begin species loop
for (spc in c("chnk", "sthd")) {
  
  # get the sites and pops for the given species
  spc_site_pops = sr_int_site_sf %>%
    dplyr::select(site_code,
                  popid = starts_with(spc)) %>%
    filter(!is.na(popid)) %>%
    st_drop_geometry()
  
  # get the population polygons for the given species
  if(spc == "chnk") { spc_pops = chnk_pops }
  if(spc == "sthd") { spc_pops = sthd_pops }
  
  # begin site loop
  for (s in 1:nrow(spc_site_pops)) {
    
    # grab the site and population
    site = spc_site_pops[s,]
    pop = site$popid
    
    # if pop contains a "/", unlist pops
    if(str_detect(pop, "/")) { pop = list(str_split(pop, "/", simplify = TRUE) %>% str_replace_all("/", "")) %>% unlist() }
    
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
    
    # more information here: https://vt-hydroinformatics.github.io/rgeoraster.html#prepare-dem-for-hydrology-analyses
    
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
    pp = sr_int_site_sf %>%
      filter(site_code == site$site_code) %>%
      dplyr::select(geometry) %>%
      distinct() %>%
      # convert the sf point to a SpatialPoints object
      as("Spatial")
    
    # fix the pour point for some remaining sites that are too far from the raster streams
    loc = pp@coords # initially, set loc equal to coordinates of pp
    if(site$site_code == "AGC")    { loc = c(765062, 4983410) }
    if(site$site_code == "AFC")    { loc = c(477479, 5124384) }
    if(site$site_code == "BHC")    { loc = c(755989, 5000575) }
    if(site$site_code == "BTL")    { loc = c(787510, 4955448) }
    if(site$site_code == "CCW")    { loc = c(434932, 5004593) }
    if(site$site_code == "ESS")    { loc = c(615717, 4979148) }
    if(site$site_code == "HEC")    { loc = c(792429, 4952633) }
    if(site$site_code == "HYC")    { loc = c(765989, 4973079) }
    if(site$site_code == "RFL")    { loc = c(667607, 4892278) }
    if(site$site_code == "USI")    { loc = c(739787, 4975157) }
    if(site$site_code == "UGR" & spc == "chnk") { loc = c(428809, 5045476)}
    if(site$site_code == "VC1")    { loc = c(664480, 4898268) }
    if(site$site_code == "WB1")    { loc = c(554055, 5067449) }
    
    # if loc != coordinates of pp, update coordinates
    if(any(loc == coordinates(pp)) == FALSE) {
      centroid = colMeans(coordinates(pp))
      translation_vector = loc - centroid
      pp@coords = coordinates(pp) + matrix(rep(translation_vector, nrow(coordinates(pp))), nrow = nrow(coordinates(pp)), byrow = TRUE)
      #pp@bbox = bbox(pp)
    }
    
    # create shapefile of pour point
    raster::shapefile(pp, filename = paste0(ws_dir, "pour_points/", site$site_code, ".shp"), overwrite = TRUE)
    
    # snap pour points to raster stream
    wbt_jenson_snap_pour_points(pour_pts = paste0(ws_dir, "pour_points/", site$site_code, ".shp"),
                                streams = paste0(ws_dir, "raster_streams/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                                output = paste0(ws_dir, "snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                                snap_dist = 100)
    
    # delineate watershed
    wbt_watershed(d8_pntr = paste0(ws_dir, "d8pointer/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                  pour_pts = paste0(ws_dir, "snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                  output = paste0(ws_dir, "watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    
    # convert watershed from raster to vector
    ws_raster = raster(paste0(ws_dir, "watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    ws_vector = st_as_stars(ws_raster) %>%
      st_as_sf(merge = T)
    
    # finally, clip the extent of the watershed polygon using the population polygon in the rare case (e.g., USE) that it extends beyond  
    ws_vector_clip = st_intersection(ws_vector, poly)
    
    # write vector watershed
    save(ws_vector_clip, file = paste0(here("output/iptds_polygons"), "/", spc, "/", site$site_code, ".rda"))
    st_write(ws_vector_clip, paste0(ws_dir, "watershed_polygons/", spc, "/", site$site_code, ".shp"), quiet = TRUE, append = FALSE)

  } # end loop over sites
} # end loop over species

### END SCRIPT