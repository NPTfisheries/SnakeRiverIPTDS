# -----------------------
# Author: Mike Ackerman
# Purpose: Draft script to prep digital elevation model prior to watershed delineation
#   using the final DEM.
# 
# Created: July 15, 2024
#   Last Modified: November 5, 2024
# 
# Notes:

# clear environment
rm(list = ls())

# load necessary packages
library(tidyverse)
library(raster)
library(terra)
# library(sf)

#--------------------
# create a seamless dem

# read in dem tiles
dem_list = list.files("C:/Workspace/gis/10m_NED_DEMS/snake_river_tiles/", pattern = "*.tif", full.names = T) %>%
  lapply(., raster)

# merge rasters to create mosaic with a larger spatial extent
dem_mosaic = do.call(merge, dem_list)

# convert the raster to a terra object
mosaic_terra = terra::rast(dem_mosaic)

# fill in NA values using terra::focal
filled_mosaic = terra::focal(mosaic_terra, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE, NAonly = TRUE)

# transform to CRS 32611
filled_mosaic_crs32611 = terra::project(filled_mosaic, "EPSG:32611")

# convert back to raster, if needed
filled_mosaic_raster = raster(filled_mosaic_crs32611)

# save the final dem
writeRaster(filled_mosaic_raster, "C:/Workspace/gis/10m_NED_DEMs/snake_river_10m_ned_dem.tif", overwrite = TRUE)

### END SCRIPT
