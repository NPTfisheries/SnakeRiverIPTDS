# -----------------------
# Author: Mike Ackerman
# Purpose: Compile and summarise IPTDS operational dates and virtual test
#   tag data for all Snake River sites from PTAGIS.
# 
# Created: May 6, 2024
#   Last Modified: August 7, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
#library(tidyverse)
#library(here)


#------------------------------
# summarize ptagis operational dates and VTT summaries together
# load iptds operational dates data frame
load(here("output/iptds_operations/ptagis_iptds_operational_dates.rda"))

# summarize whether sites were operation by species, site, and year
site_ops = ptagis_ops %>%
  left_join(vtt_summ,
            by = c("species", "site_code", "year")) %>%
  # IMPORTANT STEP: Determine if site is operational or not based on ptagis ops info
  mutate(ptagis_operational = ifelse(p_days_ptagis_run >= 0.75, # was site operational for at least 75% of days of species run
                                     TRUE,
                                     FALSE)) %>%
  # IMPORTANT STEP: Determine if site is operational or not based on vtt data
  mutate(vtt_operational = ifelse(n_transceiver > 0 & # are there greater than 0 transceivers
                                    p_vtt >= 0.50,    # is p_vtt >= 0.50, averaged across transceivers and antennas
                                  TRUE,     
                                  FALSE)) %>%
  # if either of the above conditions is TRUE, consider the site operational
  mutate(operational = ifelse(is.na(ptagis_operational) & is.na(vtt_operational), NA,
                               coalesce(ptagis_operational, FALSE) | coalesce(vtt_operational, FALSE)))

# save site operations summary
save(site_ops,
     file = here("output/iptds_operations/site_operations_summary.rda"))

### END SCRIPT
