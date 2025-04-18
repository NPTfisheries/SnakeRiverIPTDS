# -----------------------
# Author: Mike Ackerman
# Purpose: Query virtual test tag data for all Snake River INT sites in a given year
#   via PTAGIS API.
# 
# Created: August 2, 2024
#   Last Modified: April 18, 2025
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(here)

# load sites in operation each year
load(here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))

# year of vtt tags to query
yr = 2024

# list of sites that were operational for the given year
int_sites_yr = site_yrs %>%
  filter(year == yr) %>%
  select(site_code) %>%
  pull()

# set api key
api_key = read_table(here("keys/ma_ptagis_api_key.txt"), col_names = F) %>%
  as.character()

#---------------------
# query virtual test tags via PTAGIS API requests

# a single site and year, for example
# vtt_df = queryTimerTagSite(site_code = "ZEN",
#                            year = 2024,
#                            api_key = api_key)

# plot virtual test data for a single site and year, for example
# vtt_df %>%
#   ggplot(aes(x = time_stamp, y = 1)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = -45, vjust = 0.5),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   #facet_grid(antenna_id~transceiver_id)
#   facet_wrap(~antenna_id, ncol = 1)

# query virtual test tag data for all sites operational in a given year
for (s in int_sites_yr) {
  tryCatch({
    # use queryTestTagSite() from PITcleanr to send virtual test tag API request to PTAGIS
    vtt_df = queryTimerTagSite(site_code = s,
                               year = yr,
                               api_key = api_key)
    
    # save the results, if they exist
    if (!is.null(vtt_df)) {
      saveRDS(vtt_df, file.path(here("data/virtual_test_tags"), paste0(yr, "/", s, "_", yr, ".rds")))
      print(paste0("Virtual test tag data saved for site ", s, ", year ", yr, "."))
    }
  }, error = function(e) {
    # handle the error i.e., print an error message
    print(paste0("Error occurred for site ", s, ": ", conditionMessage(e)))
  })
}

### END SCRIPT