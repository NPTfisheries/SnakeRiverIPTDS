# -----------------------
# Author: Mike Ackerman
# Purpose: Create leaflet map to support Snake R. IPTDS prioritization and planning.
#   Contains information on recommendations for continued funding, decommission, candidates for project,
#   upgrades, etc. to aid in planning and discussions.
# 
# Created: April 11, 2024
#   Last Modified: July 24, 2025
# 
# Notes: Much of this is based on a previous script iptds_planning.R from RK. This script produces the default leaflet
#   with Snake River prioritization recommendations.

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(htmlwidgets)

# -----------------------
# compile data

# maintained iptds metadata
iptds_sf = read_excel("data/Maintained Snake River IPTDS Metadata 20260109.xlsx",
                      sheet = "SR_IPTDS_Sites") %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# current iptds
curr_iptds_sf = iptds_sf %>%
  filter(ptagis_active == TRUE)

# inactive iptds
past_iptds_sf = iptds_sf %>%
  filter(ptagis_active == FALSE)

# iptds recommendations
recommendations_sf = read_excel(here("data/prioritization/iptds_site_recommendations_20250409.xlsx"),
                                sheet = "Sheet1") %>%
  filter(!site_code %in% c("EFS (Proposed)",
                           "WB1 (Proposed)",
                           "SLT (Proposed)"#,
                           # based on e-mail from wdfw 11/27/2024; folding back in 04/07/2025
                           #"ACM"
                           )) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)
  
# populations
load("data/spatial/SR_pops.rda") ; rm(fall_pop)
sthd_pops = sth_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(sth_pop)
chnk_pops = spsm_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(spsm_pop)

# dabom mrr sites
load("data/spatial/dabom_mrr_sites.rda")

# snake river dam sites
load("data/spatial/sr_dam_sites.rda")

# streams and steelhead major/minor spawning areas
load("data/spatial/steelhead_gis_data.rda")
streams = sthd_critical %>%
  st_transform("EPSG:4326")

sthd_spawn = sthd_spawn %>%
  st_transform("EPSG:4326") ; rm(sthd_critical, sthd_extant, sthd_ip)

# chinook major/minor spawning areas
chnk_spawn = readRDS("data/spatial/spsm_spwn_areas.rds") %>%
  st_transform("EPSG:4326")

# -----------------------
# set some colors
sthd_mpg_col = colorFactor(palette = "Dark2", domain = sthd_pops$MPG)
sthd_spawn_col = colorFactor(palette = c("skyblue", "navy"), domain = sthd_spawn$TYPE, reverse = TRUE)
chnk_mpg_col = colorFactor(palette = "Set1", domain = chnk_pops$MPG)
chnk_spawn_col = colorFactor(palette = c("springgreen", "darkgreen"), domain = chnk_spawn$TYPE, reverse = TRUE)

recommendation_col = colorFactor(palette = c("black", "darkorchid2", "deepskyblue2", "springgreen3"), 
                                 domain = iptds_rec$recommendation,
                                 levels = c("Continue Funding", "Proposed New Site", "Candidate for O&M Project", "Decommission, Remove, or Transfer"))

om_col = colorFactor(palette = c("gray50", "black"), domain = iptds_sf$integrated_om_site)

priority_col = colorFactor(c("red", "orange", "yellow"), 
                           domain = recommendations_sf$action_priority, 
                           levels = c("HIGH", "MED", "LOW"),
                           na.color = "transparent")

juv_col = colorFactor(c("black", "gray50", "cyan"),
                      domain = juv_iptds_sf$juv_detect_site,
                      levels = c("Good To Go", "Good w/ Upgrades", "Proposed"))

# -----------------------
# build leaflet
base = leaflet() %>%
  # base maps
  setView(lng = -116, lat = 45.35, zoom = 7.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines(data = streams, color = "blue", weight = 1)

sr_iptds_leaflet = base %>%
  # steelhead populations
  addPolygons(data = sthd_pops,
              group = "Steelhead Populations",
              fillColor = ~sthd_mpg_col(MPG),
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>Steelhead</b></br>",
                            "<b>Pop ID:</b>", sthd_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", sthd_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", sthd_pops$MPG, "</br>")) %>%
  # steelhead major/minor spawning areas
  addPolygons(data = sthd_spawn,
              group = "Steelhead Spawning Areas",
              fillColor = ~sthd_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME, ", ", TYPE)) %>%
  # chinook salmon populations
  addPolygons(data = chnk_pops,
              group = "Sp/Sum Chinook Populations",
              fillColor = ~chnk_mpg_col(MPG),
              fillOpacity = 0.2,
              stroke = T,
              weight = 2,
              color = "black",
              opacity = 1,
              label = ~paste0(TRT_POPID, ": ", POP_NAME),
              popup = paste("<b>sp/sum Chinook salmon</b></br>",
                            "<b>Pop ID:</b>", chnk_pops$TRT_POPID, "</br>",
                            "<b>Pop Name:</b>", chnk_pops$POP_NAME, "</br>",
                            "<b>MPG:</b>", chnk_pops$MPG, "</br>")) %>%
  # chinook major/minor spawning areas
  addPolygons(data = chnk_spawn,
              group = "Sp/Sum Chinook Spawning Areas",
              fillColor = ~chnk_spawn_col(TYPE),
              fillOpacity = 0.2,
              stroke = T,
              weight = 1,
              color = "black",
              opacity = 1,
              label = ~paste0(POP_NAME, ": ", MSA_NAME, ", ", TYPE)) %>%
  # add current iptds
  addCircleMarkers(data = curr_iptds_sf,
                   group = "Current IPTDS Sites",
                   label = ~site_code,
                   fillColor = ~om_col(integrated_om_site), # fill color
                   fillOpacity = 0.9,                       # fill opacity
                   color = "chartreuse",                    # border color
                   opacity = 1,                             # border opacity
                   weight = 1,                              # border thickness
                   radius = 8,                              # marker size in pixels
                   popup = paste("<b>Site Code:</b>", curr_iptds_sf$site_code, "</br>",
                                 "<b>Site Name:</b>", curr_iptds_sf$site_name, "</br>",
                                 "<b>Stream:</b>", curr_iptds_sf$stream, "</br>",
                                 "<b>PTAGIS Active:</b>", curr_iptds_sf$ptagis_active, "</br>",
                                 "<b>Arrays:</b>", curr_iptds_sf$node_count, "</br>",
                                 "<b>Antennas:</b>", curr_iptds_sf$antenna_count, "</br>",
                                 "<b>Biomark Integrated O&M Site:</b>", curr_iptds_sf$integrated_om_site, "</br>",
                                 # "<b>Detection Probabilities:</b>", curr_iptds_sf$detection_prob, "</br>",
                                 # "<b>Current Funding:</b>", curr_iptds_sf$current_funding, "</br>",
                                 # "<b>BPA Funding Type:</b>", curr_iptds_sf$bpa_funding, "</br>",
                                 "<b>O&M Management:</b>", curr_iptds_sf$om_agency, "</br>",
                                 "<b>Site Description:</b>", curr_iptds_sf$site_description, "</br>")) %>%
  # add a legend for integrated o&m colors
  addLegend(data = curr_iptds_sf,
            position = "bottomleft",
            pal = om_col,
            values = ~factor(integrated_om_site, levels = c("TRUE", "FALSE")),
            title = "Integrated O&M Site",
            group = "Current IPTDS Sites",
            opacity = 0.8) %>%
  # add past iptds
  addCircleMarkers(data = past_iptds_sf,
                   group = "Past IPTDS Sites",
                   label = ~site_code,
                   fillColor = ~om_col(integrated_om_site), # fill color
                   fillOpacity = 0.9,                       # fill opacity
                   color = "red",                           # border color
                   opacity = 1,                             # border opacity
                   weight = 1,                              # border thickness
                   radius = 8,                              # marker size in pixels
                   popup = paste("<b>Site Code:</b>", past_iptds_sf$site_code, "</br>",
                                 "<b>Site Name:</b>", past_iptds_sf$site_name, "</br>",
                                 "<b>Stream:</b>", past_iptds_sf$stream, "</br>",
                                 "<b>PTAGIS Active:</b>", past_iptds_sf$ptagis_active, "</br>",
                                 "<b>Arrays:</b>", past_iptds_sf$node_count, "</br>",
                                 "<b>Antennas:</b>", past_iptds_sf$antenna_count, "</br>",
                                 "<b>Biomark Integrated O&M Site:</b>", past_iptds_sf$integrated_om_site, "</br>",
                                 # "<b>Detection Probabilities:</b>", past_iptds_sf$detection_prob, "</br>",
                                 # "<b>Current Funding:</b>", past_iptds_sf$current_funding, "</br>",
                                 # "<b>BPA Funding Type:</b>", past_iptds_sf$bpa_funding, "</br>",
                                 "<b>O&M Management:</b>", past_iptds_sf$om_agency, "</br>",
                                 "<b>Site Description:</b>", past_iptds_sf$site_description, "</br>")) %>%
  # add iptds recommendations
  addCircleMarkers(data = recommendations_sf,
                   group = "Integrated O&M Recommendations",
                   label = ~site_code,
                   fillColor = ~recommendation_col(recommendation),
                   fillOpacity = 0.8,
                   color = ~priority_col(action_priority),
                   opacity = 0.8,
                   weight = 2,
                   radius = 8,
                   popup = paste("<b>Site Code:</b>", recommendations_sf$site_code, "</br>",
                                 "<b>Priority:</b>", recommendations_sf$action_priority, "</br>",
                                 "<b>Notes:</b>", recommendations_sf$notes, "</br>")) %>%
  # add a legend for action priorities
  addLegend(data = recommendations_sf,
            position = "bottomleft",
            pal = priority_col,
            values = ~factor(action_priority, levels = c("HIGH", 
                                                         "MED", 
                                                         "LOW")),
            title = "Priority (Border)",
            group = "Integrated O&M Recommendations",
            opacity = 0.8) %>%
  # add a legend for recommendation colors
  addLegend(data = recommendations_sf,
            position = "bottomleft",
            pal = recommendation_col,
            values = ~factor(recommendation, levels = c("Continue Funding", 
                                                        "Proposed New Site", 
                                                        "Candidate for O&M Project", 
                                                        "Decommission, Remove, or Transfer")),
            title = "Integrated O&M Recommendations",
            group = "Integrated O&M Recommendations",
            opacity = 0.8) %>%
  # add info on juvenile detection sites
  # addCircleMarkers(data = juv_iptds_sf,
  #             group = "Juvenile Obs Sites",
  #             label = ~site_code,
  #             fillColor = ~juv_col(juv_detect_site),
  #             fillOpacity = 8,
  #             color = NA,
  #             opacity = 0,
  #             radius = 8,
  #             popup = paste("<b>Site Code:</b>", juv_iptds_sf$site_code, "</br>",
  #                           "<b>Site Name:</b>", juv_iptds_sf$site_name, "</br>",
  #                           "<b>Biomark Integrated O&M Site:</b>", juv_iptds_sf$integrated_om_site, "</br>")) %>%
  # add a legend for juvenile detection sites
  # addLegend(data = juv_iptds_sf,
  #           position = "bottomleft",
  #           pal = juv_col,
  #           values = ~factor(juv_detect_site, levels = c("Good To Go",
  #                                                        "Good w/ Upgrades",
  #                                                        "Proposed")),
  #           title = "Juvenile Obs Sites",
  #           group = "Juvenile Obs Sites",
  #           opacity = 0.8) %>%
  # dabom mrr sites
  addCircles(data = mrr_sites,
             group = "DABOM MRR Sites",
             label = ~site_code,
             color = "magenta",
             opacity = 1,
             weight = 5,
             popup = paste("<b>Site Code:</b>", mrr_sites$site_code, "</br>",
                           "<b>Site Name:</b>", mrr_sites$site_name, "</br>",
                           "<b>DABOM Node:</b>", mrr_sites$node, "</br>")) %>%
  # snake river dam sites
  addCircles(data = sr_dam_sites,
             group = "Snake River Dams",
             label = ~site_code,
             color = "dodgerblue",
             opacity = 1,
             weight = 6,
             popup = paste("<b>Site Name:</b>", sr_dam_sites$site_name, "</br>")) %>%
  # control layers
  addLayersControl(baseGroups = c("Steelhead Populations",
                                  "Steelhead Spawning Areas",
                                  "Sp/Sum Chinook Populations",
                                  "Sp/Sum Chinook Spawning Areas"),
                   overlayGroups = c("Current IPTDS Sites",
                                     "Past IPTDS Sites",
                                     "Integrated O&M Recommendations",
                                     #"Juvenile Obs Sites",
                                     "DABOM MRR Sites",
                                     "Snake River Dams"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Past IPTDS Sites") %>%
  hideGroup("Integrated O&M Recommendations") %>%
  #hideGroup("Juvenile Obs Sites") %>%
  hideGroup("DABOM MRR Sites") %>%
  hideGroup("Snake River Dams")

sr_iptds_leaflet

# save leaflet
saveWidget(sr_iptds_leaflet, file = here("shiny/leaflet/sr_iptds_leaflet_w_recommendations.html"))

### END SCRIPT