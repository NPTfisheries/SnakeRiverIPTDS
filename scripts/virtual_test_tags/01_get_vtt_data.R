# -----------------------
# Author: Mike Ackerman
# Purpose: Query virtual test tag data for all Snake River sites in a given year
#   via PTAGIS API.
# 
# Created: April 29, 2024
#   Last Modified: July 10, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(PITcleanr)
library(janitor)
library(here)

# query interrogation site metadata
iptds_meta = queryInterrogationMeta() %>%
  clean_names()

# snake river interrogation site metadata
sr_iptds_meta = iptds_meta %>%
  filter(str_starts(rkm, "522"),
         site_type %in% c("Instream Remote Detection System",
                          "Adult Fishway"),
         # removes the four Snake River dams
         !operations_organization_code == "PSMFC") %>%
  select(site_code,
         name,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date,
         last_file_opened_on,
         operation_period,
         operations_organization_code,
         rkm,
         site_type,
         latitude,
         longitude) ; rm(iptds_meta)

# summarize iptds operational dates
iptds_ops = sr_iptds_meta %>%
  # get the latest date for each site
  group_by(site_code) %>%
  mutate(last_date = max(last_date, last_file_opened_on, na.rm = T)) %>%
  ungroup() %>%
  select(site_code,
         active,
         operational,
         first_year,
         last_year,
         first_date,
         last_date) %>%
  mutate(first_date = as.Date(first_date),
         last_date = as.Date(last_date)) %>%
  # These sites don't have first_date and/or last_date; grabbed from PTAGIS
  mutate(first_date = if_else(site_code == "BED", as.Date("2024-02-15"), first_date)) %>%
  mutate(first_date = if_else(site_code == "UG3", as.Date("2024-07-09"), first_date)) %>%
  mutate(last_date  = if_else(site_code == "UG3", as.Date(Sys.Date()),   last_date))  %>%
  mutate(first_date = if_else(site_code == "UG4", as.Date("2024-07-09"), first_date)) %>%
  mutate(last_date  = if_else(site_code == "UG4", as.Date(Sys.Date()),   last_date))

# sequence of years that each site was in operation
site_yrs = iptds_ops %>%
  group_by(site_code) %>%
  summarise(year = list(seq(min(year(first_date)), max(year(last_date)), by = 1))) %>%
  unnest(year)

#---------------------
# Query virtual test tags via PTAGIS API requests

# year of interest?
yr = 2024

# list of sites that were operational for the given year
sites = site_yrs %>%
  filter(year == yr) %>%
  select(site_code) %>%
  pull()

# set api key
api_key = read_table(here("keys/ma_ptagis_api_key.txt"), col_names = F) %>%
  as.character()

# a single site and year, for example
# vtt_df = queryTestTagSite(site_code = "ZEN",
#                           year = 2023,
#                           api_key = api_key)

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
for (s in sites) {
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
