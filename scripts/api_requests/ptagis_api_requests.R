#-----------------------
# Author: Mike Ackerman
# Purpose: Testing various PTAGIS API requests
# 
# Created: May 6, 2024
#   Last Modified:
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)

# set PTAGIS API key
api_key = read_table(here("ma_api_key.txt"), col_names = F) %>%
  as.character()

# site and year of interest
site_code = "ZEN"
yr = "2023"

# interrogation files for a given site and year
url = paste0("https://api.ptagis.org/files/interrogation/sites/", site_code, "/year/", yr)
int_file_meta = httr::GET(url) %>%
  httr::content(., "parsed") %>%
  pluck("model") %>%
  bind_rows()

# get a single interrogation file
int_file = int_file_meta$fileName[1]
url = paste0("https://api.ptagis.org/files/interrogation/", int_file)
int_file = httr::GET(url) %>%
  httr::content(., "parsed") %>%
  pluck("interrogationFieldData") %>%
  bind_rows()

# END SCRIPT