# -----------------------
# Author: Mike Ackerman
# Purpose: Prepare watershed polygons of carcass outplant streams for later intrinsic productivity
#   analyses.
# 
# Created: May 8, 2025
#   Last Modified: May 28, 2025
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

# prep sites to be evaluated
streams_sf = tribble(
  ~site_code, ~chnk_popid, ~sthd_popid, ~latitude, ~longitude,
  "Red_R", "SCUMA", "CRSFC-s", 45.80872, -115.47424,
  "American_R", "SCUMA", "CRSFC-s", 45.80938, -115.47569,
  "Crooked_R", "SCUMA", "CRSFC-s", 45.82283, -115.52799,
  "Newsome_Cr", "SCUMA", "CRSFC-s", 45.82981, -115.61407,
  "Meadow_Cr_SFCW", "SCUMA", "CRSFC-s", 45.82888, -115.92838,
  "Lolo_Cr", "CRLOL", "CRLOL-s", 46.37235, -116.16307,
  "OHara_Cr", "SEMEA", "CRSEL-s", 46.08432, -115.51683,
  "Meadow_Cr_Sel", "SEMEA", "CRSEL-s", 46.04437, -115.29690,
  "Pete_King_Cr", "CRLOC", "CRLOC-s", 46.16643, -115.59049,
  "Legendary_Bear_Cr", "CRLOC", "CRLOC-s", 46.51249, -114.76309,
  "Fishing_Cr", "CRLOC", "CRLOC-s", 46.49309, -114.85869,
  "Lapwai_All", "CRLAP", "CRLMA-s", 46.44851, -116.81785,
  "Sweetwater", "CRLAP", "CRLMA-s", 46.36907, -116.79592,
  "Lapwai_Mission", "CRLAP", "CRLMA-s", 46.36890, -116.79456,
  "Big_Canyon_Cr", "CRLAP", "CRLMA-s", 46.49634, -116.43420,
) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%          # got my waypoints using WGS 84
  st_transform(crs = default_crs)   # convert to UTM Zone 11N

# read in prepped DEM
snake_dem = raster(paste0(ws_dir, "snake_river_10m_ned_dem.tif"))

# begin species loop
for (spc in c("chnk", "sthd")) {
  
  spc_site_pops = streams_sf %>%
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
    pp = streams_sf %>%
      filter(site_code == site$site_code) %>%
      dplyr::select(geometry) %>%
      distinct() %>%
      # convert the sf point to a SpatialPoints object
      as("Spatial")
    
    # create shapefile of pour point
    raster::shapefile(pp, filename = paste0(ws_dir, "carcass_outplant_sites/pour_points/", site$site_code, ".shp"), overwrite = TRUE)
    
    # snap pour points to raster stream
    wbt_jenson_snap_pour_points(pour_pts = paste0(ws_dir, "carcass_outplant_sites/pour_points/", site$site_code, ".shp"),
                                streams = paste0(ws_dir, "raster_streams/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                                output = paste0(ws_dir, "carcass_outplant_sites/snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                                snap_dist = 100)
    
    # delineate watershed
    wbt_watershed(d8_pntr = paste0(ws_dir, "d8pointer/", spc, "/", paste(pop, collapse = "_"), ".tif"),
                  pour_pts = paste0(ws_dir, "carcass_outplant_sites/snapped_pour_points/", spc, "/", site$site_code, ".shp"),
                  output = paste0(ws_dir, "carcass_outplant_sites/watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    
    # convert watershed from raster to vector
    ws_raster = raster(paste0(ws_dir, "carcass_outplant_sites/watershed_rasters/", spc, "/", paste(pop, collapse = "_"), ".tif"))
    ws_vector = st_as_stars(ws_raster) %>%
      st_as_sf(merge = T)
    
    # finally, clip the extent of the watershed polygon using the population polygon in the rare case (e.g., USE) that it extends beyond  
    ws_vector_clip = st_intersection(ws_vector, poly)
    
    # write vector watershed
    save(ws_vector_clip, file = paste0(here("output/carcass_outplant_study/carcass_outplant_polygons"), "/", spc, "/", site$site_code, ".rda"))
    st_write(ws_vector_clip, paste0(ws_dir, "carcass_outplant_sites/watershed_polygons/", spc, "/", site$site_code, ".shp"), quiet = TRUE, append = FALSE)
    
  } # end loop over sites
} # end loop over species

# save streams_sf data frame
save(streams_sf,
     file = here("output/carcass_outplant_study/carcass_study_streams.rda"))

### END SCRIPT