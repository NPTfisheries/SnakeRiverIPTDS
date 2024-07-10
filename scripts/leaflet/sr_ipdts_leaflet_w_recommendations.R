# -----------------------
# Author: Mike Ackerman
# Purpose: Create leaflet map to support Snake R. IPTDS prioritization and planning.
#   Contains information on recommendations for continued funding, decommission, candidates for project,
#   upgrades, etc. to aid in planning and discussions.
# 
# Created: April 11, 2024
#   Last Modified: July 10, 2024
# 
# Notes: Much of this is based on a previous script iptds_planning.R from RK. This script produces the default leaflet
#   with Snake River prioritization recommendations.

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(readxl)
library(sf)
library(leaflet)
library(htmlwidgets)

# -----------------------
# COMPILE DATA

# current iptds data
iptds_cur = read_excel(here("data/prioritization/Snake River IPTDS Prioritization 20240417.xlsx"),
                       sheet = "SR_IPTDS_Sites") %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# iptds recommendations
iptds_rec = read_excel(here("data/prioritization/iptds_site_recommendations_20240411.xlsx"),
                       sheet = "Sheet1") %>%
  filter(!site_code %in% c("EFS (Proposed)",
                           "WB1",
                           "SLT (Proposed)",
                           "ALP (Proposed)")) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)
  
# populations
load(here("data/spatial/SR_pops.rda")) ; rm(fall_pop)
sthd_pops = sth_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(sth_pop)

chnk_pops = spsm_pop %>%
  select(TRT_POPID, POP_NAME, MPG) ; rm(spsm_pop)

# dabom mrr sites
load(here("data/spatial/dabom_mrr_sites.rda"))

# streams and steelhead major/minor spawning areas
load(here("data/spatial/steelhead_gis_data.rda"))
streams = sthd_critical %>%
  st_transform("EPSG:4326")

sthd_spawn = sthd_spawn %>%
  st_transform("EPSG:4326") ; rm(sthd_critical, sthd_extant, sthd_ip)

# chinook major/minor spawning areas
chnk_spawn = readRDS(here("data/spatial/spsm_spwn_areas.rds")) %>%
  st_transform("EPSG:4326")

# -----------------------
# SET SOME COLORS
sthd_mpg_col = colorFactor(palette = "Dark2", domain = sthd_pops$MPG)
sthd_spawn_col = colorFactor(palette = c("skyblue", "navy"), domain = sthd_spawn$TYPE, reverse = TRUE)
chnk_mpg_col = colorFactor(palette = "Set1", domain = chnk_pops$MPG)
chnk_spawn_col = colorFactor(palette = c("springgreen", "darkgreen"), domain = chnk_spawn$TYPE, reverse = TRUE)
rec_col = colorFactor(palette = c("black", "red3", "royalblue4", "goldenrod3"), 
                      domain = iptds_rec$recommendation,
                      levels = c("Continue Funding", "Proposed New Site", "Candidate for O&M Project", "Decommission, Remove, or Transfer"))
cur_col = colorFactor(palette = c("gray50", "black"), domain = iptds_cur$integrated_om_site)
priority_col = colorFactor(c("red", "orange", "yellow"), 
                           domain = iptds_rec$action_priority, 
                           levels = c("HIGH", "MED", "LOW"),
                           na.color = "transparent")

# -----------------------
# BUILD LEAFLET
base = leaflet() %>%
  # base maps
  setView(lng = -115.5660, lat = 45.4000, zoom = 7.5) %>%
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
  addCircles(data = iptds_cur,
             group = "Current IPTDS Sites",
             label = ~site_code,
             color = ~cur_col(integrated_om_site),
             opacity = 1,
             weight = 10,
             popup = paste("<b>Site Code:</b>", iptds_cur$site_code, "</br>",
                           "<b>Site Name:</b>", iptds_cur$site_name, "</br>",
                           "<b>Stream:</b>", iptds_cur$stream, "</br>",
                           "<b>Node Count:</b>", iptds_cur$node_count, "</br>",
                           "<b>Antenna Count:</b>", iptds_cur$antenna_count, "</br>",
                           "<b>PTAGIS Active:</b>", iptds_cur$ptagis_active, "</br>",
                           "<b>Detection Probabilities:</b>", iptds_cur$detection_prob, "</br>",
                           "<b>Biomark Integrated O&M Site:</b>", iptds_cur$integrated_om_site, "</br>",
                           "<b>Status and Trends:</b>", iptds_cur$adult_status_trends, "</br>",
                           "<b>Current Funding:</b>", iptds_cur$current_funding, "</br>",
                           "<b>BPA Funding Type:</b>", iptds_cur$bpa_funding, "</br>",
                           "<b>O&M Agency:</b>", iptds_cur$om_agency, "</br>",
                           "<b>Site Description:</b>", iptds_cur$site_description, "</br>")) %>%
  # add iptds recommendations
  addCircles(data = iptds_rec,
             group = "IPTDS Recommendations",
             label = ~site_code,
             color = ~rec_col(recommendation),
             opacity = 1,
             weight = 10,
             popup = paste("<b>Site Code:</b>", iptds_rec$site_code, "</br>",
                           "<b>Priority:</b>", iptds_rec$action_priority, "</br>",
                           "<b>Notes:</b>", iptds_rec$notes, "</br>")) %>%
  # dabom mrr sites
  addCircles(data = mrr_sites,
             group = "DABOM MRR Sites",
             label = ~site_code,
             color = "hotpink",
             opacity = 1,
             weight = 5,
             popup = paste("<b>Site Code:</b>", mrr_sites$site_code, "</br>",
                           "<b>Site Name:</b>", mrr_sites$site_name, "</br>",
                           "<b>DABOM Node:</b>", mrr_sites$node, "</br>")) %>%
  # color by status and trends tier
  addCircleMarkers(data = iptds_rec,
                   group = "Action Priority",
                   color = ~priority_col(action_priority)) %>%
  # control layers
  addLayersControl(baseGroups = c("Steelhead Populations",
                                  "Steelhead Spawning Areas",
                                  "Sp/Sum Chinook Populations",
                                  "Sp/Sum Chinook Spawning Areas"),
                   overlayGroups = c("IPTDS Recommendations",
                                     "Current IPTDS Sites",
                                     "DABOM MRR Sites",
                                     "Action Priority"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  # add legends
  # addLegend(data = sthd_spawn,       # consider moving this to polygon labels, instead
  #           position = "bottomleft",
  #           pal = sthd_spawn_col,
  #           values = ~TYPE,
  #           title = "Spawning Area Type",
  #           group = "Steelhead Spawning Areas",
  #           opacity = 0.5) %>%
  # addLegend(data = chnk_spawn,       # consider moving this to polygon labels, instead
  #           position = "bottomleft",
  #           pal = chnk_spawn_col,
  #           values = ~TYPE,
  #           title = "Spawning Area Type",
  #           group = "Chinook Spawning Areas",
  #           opacity = 0.5) %>%
  addLegend(data = iptds_rec,        # consider moving to lower-left, if other legends are removed
            position = "bottomleft",
            pal = rec_col,
            values = ~factor(recommendation, levels = c("Continue Funding", 
                                                        "Proposed New Site", 
                                                        "Candidate for O&M Project", 
                                                        "Decommission, Remove, or Transfer")),
            title = "IPTDS Recommendations",
            group = "IPTDS Recommendations",
            opacity = 0.8) %>%
  addLegend(data = iptds_cur,
            position = "bottomleft",
            pal = cur_col,
            values = ~factor(integrated_om_site, levels = c("TRUE", "FALSE")),
            title = "Integrated O&M Site",
            group = "Current IPTDS Sites",
            opacity = 0.8) %>%
  addLegend(data = iptds_rec, 
            position = "bottomright",
            pal = priority_col,
            values = ~factor(action_priority, levels = c("HIGH", "MED", "LOW")),
            title = "Action Priority",
            group = "Action Priority",
            opacity = 0.5) %>%
  hideGroup("DABOM MRR Sites") %>%
  hideGroup("Action Priority")

sr_iptds_leaflet

# save leaflet
saveWidget(sr_iptds_leaflet, file = here("shiny/leaflet/sr_iptds_leaflet_w_recommendations.html"))

### END SCRIPT
