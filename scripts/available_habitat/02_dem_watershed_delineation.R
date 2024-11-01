# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to delineate watersheds using a prepped DEM. 
#   More details to come later.
# 
# Created: July 22, 2024
#   Last Modified: August 8, 2024
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
load("C:/Git/SnakeRiverFishStatus/data/configuration_files/site_config_LGR_20240927.rda")
rm(configuration, node_paths, parent_child, pc_nodes, flowlines)

# ictrt population polygons
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  st_transform(default_crs) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  st_transform(default_crs) ; rm(spsm_pop)

# create sf object of dabom sites
dabom_site_sf = crb_sites_sf %>%
  # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
  filter(str_detect(rkm, "^522\\.")) %>%
  # note this excludes some sites below lgr, but those likely aren't appropriate for expansions anyways
  filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>%
  # grab only INT sites, for now
  filter(site_type == "INT") %>%
  dplyr::select(site_code) %>%
  # join sthd and chnk populations to iptds sites
  st_join(sthd_pops %>%
            dplyr::select(sthd_pop = TRT_POPID)) %>%
  st_join(chnk_pops %>%
            dplyr::select(chnk_pop = TRT_POPID)) %>%
  arrange(site_code)

# dabom_site_sf = sites_sf %>%
#   # filter to ensure the first 3-digit number is 522 and the second 3-digit number is > 173 (LGR)
#   filter(str_detect(rkm, "^522\\.")) %>%
#   # note this excludes some sites below lgr, but those likely aren't appropriate for expansions anyways
#   filter(as.numeric(str_extract(rkm, "(?<=^522\\.)(\\d{3})")) > 173) %>% 
#   # grab only INT sites or MRR sites previously used for population abundance estimates
#   filter(site_type == "INT" | 
#            site_code %in% c("SALEFT", "PAHH", "RAPH", "ALPOWC", "TENMC2")) %>%
#   dplyr::select(site_code) %>%
#   # transform to WGS84
#   st_transform(default_crs) %>%
#   # join ictrt populations to iptds (just using sthd for now)
#   st_join(sthd_pops %>%
#             dplyr::select(pop = TRT_POPID)) %>%
#   arrange(site_code)

# save dabom_site_sf
# save(dabom_site_sf,
#      file = here("output/available_habitat/dabom_sites_sf.rda"))

# read in prepped DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

### CONTINUE HERE!!!

#--------------------
# begin loop
for (s in 1:nrow(dabom_site_sf)) {

  # grab the site and population
  site = dabom_site_sf[s,] %>% st_drop_geometry()
  pop = site$pop
  
  cat(paste0("Creating the watershed polygon for site ", site$site_code, ".\n"))
  
  # get the population polygon
  poly = sthd_pops %>%
    filter(TRT_POPID == pop) %>%
    dplyr::select(pop = TRT_POPID)
  
  # accommodate sites that cover multiple populations
  if(site$site_code %in% c("SC1", "SC2")) {
    poly = sthd_pops %>%
      filter(TRT_POPID %in% c("CRLMA-s", "CRSFC-s")) %>%
      dplyr::select(pop = TRT_POPID) %>%
      summarise(
        pop = paste(pop, collapse = "_"),
        geometry = st_union(geometry)
      )
    pop = poly$pop
  }
  if(site$site_code == "SFG") {
    poly = sthd_pops %>%
      filter(TRT_POPID %in% c("SFMAI-s", "SFSEC-s")) %>%
      dplyr::select(pop = TRT_POPID) %>%
      summarise(
        pop = paste(pop, collapse = "_"),
        geometry = st_union(geometry)
      )
    pop = poly$pop
  }
  if(site$site_code %in% c("USE", "USI")) {
    poly = sthd_pops %>%
      filter(TRT_POPID %in% c("SRPAH-s", "SREFS-s", "SRUMA-s")) %>%
      dplyr::select(pop = TRT_POPID) %>%
      summarise(
        pop = paste(pop, collapse = "_"),
        geometry = st_union(geometry)
      )
    pop = poly$pop
  }
  
  # more information here: https://vt-hydroinformatics.github.io/rgeoraster.html#prepare-dem-for-hydrology-analyses

  # check to see if the raster streams for a population polygon already exists; if not, run loop
  if(file.exists(paste0(ws_dir, "raster_streams/", pop, ".tif"))) {
    
    # if the file exists, print a message and skip the loop
    cat(paste0("The raster streams file for population ", pop, " already exists. Skipping the loop.\n"))
    
  } else {
    
    # if the file does not exist, perform the watershed delineation, etc.
    cat(paste0("The raster streams file for population ", pop, " does not exist. Running the loop.\n"))
   
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
  pp = dabom_site_sf %>%
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
  if(site$site_code == "VC1")    { loc = c(664480, 4898268) }
  if(site$site_code == "WB1")    { loc = c(554055, 5067449) }
  if(site$site_code == "ALPOWC") { loc = c(483581, 5139973) }
  if(site$site_code == "PAHH")   { loc = c(734246, 4952326) }
  if(site$site_code == "TENMC2") { loc = c(500614, 5127071) }

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
                              streams = paste0(ws_dir, "raster_streams/", pop, ".tif"),
                              output = paste0(ws_dir, "snapped_pour_points/", site$site_code, ".shp"),
                              snap_dist = 100)
  
  # delineate watershed
  wbt_watershed(d8_pntr = paste0(ws_dir, "d8pointer/", pop, ".tif"),
                pour_pts = paste0(ws_dir, "snapped_pour_points/", site$site_code, ".shp"),
                output = paste0(ws_dir, "watershed_rasters/", pop, ".tif"))
  
  # convert watershed from raster to vector
  ws_raster = raster(paste0(ws_dir, "watershed_rasters/", pop, ".tif"))
  ws_vector = st_as_stars(ws_raster) %>%
    st_as_sf(merge = T)
  
  # finally, clip the extent of the watershed polygon using the population polygon in the rare case (e.g., USE) that it extends beyond  
  ws_vector_clip = st_intersection(ws_vector, poly)
  
  # write vector watershed
  save(ws_vector_clip, file = paste0(here("output/iptds_polygons"), "/", site$site_code, ".rda"))
  st_write(ws_vector_clip, paste0(ws_dir, "watershed_polygons/", site$site_code, ".shp"), quiet = TRUE, append = FALSE)
  
} # end loop over sites

### END SCRIPT