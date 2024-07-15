#' Title Summarize and map populations by summarized IP and capacity estimates.
#' Author Ryan Kinzer
#' Created March 8, 2024
# Last Modified: 
# Notes: 

# Load needed libraries -----
library(tidyverse)
library(sf)

source(here::here('scripts', 'summarize_spatial_metrics.R'))
source(here::here('scripts', 'save_spatial_layers.R'))

crs_default <- 4326 # WGS84 (4326) is necessary for leaflet to work. #26911 #4326 #26911

# filepath
spatial_files <- 'C://Spatial_Files/QGIS Files/NAD83_11N_26911/'

# ICC boundary
icc <- st_read(paste0(spatial_files,'TribalTerritory_NPT/TribalTerritory_NPT.shp')) %>%
  st_transform(crs = crs_default)

save_spatial_layers(icc, type = 'raw')

# dissolve huc12 into huc10----
huc12 <- st_read(paste0(spatial_files,'HUC12/HUC12_snake_river.shp')) %>%
  st_transform(crs = crs_default)

save_spatial_layers(huc12, type = 'raw')

huc10 <- huc12 %>%
  group_by(HUC_10, HU_10_NAME) %>%
  summarise() %>%
  st_cast("POLYGON")

save_spatial_layers(huc10, type = 'raw')

# load population polygons
spsm_pops <- st_read(paste0(spatial_files,'Snake River Extant SpSm Chinook POP/Snake River Extant SpSm Chinook POP.shp')) %>% 
  st_transform(crs = crs_default) %>%
  filter(!(TRT_POPID %in% c('NCUMA', 'NCLMA')))

save_spatial_layers(spsm_pops, type = 'raw')

sthd_pops <- st_read(paste0(spatial_files,'Snake River Extant Steelhead POP/Snake River Extant Steelhead POP.shp')) %>%
  st_transform(crs = crs_default) %>%
  filter(TRT_POPID != 'CRNFC-s')

save_spatial_layers(sthd_pops, type = 'raw')

fall_pops <- st_read(paste0(spatial_files,'Snake River Extant Fall Chinook POP/Snake River Extant Fall Chinook POP.shp')) %>%
  st_transform(crs = crs_default)

save_spatial_layers(fall_pops, type = 'raw')

# load spawning areas
spatial_files <- 'C://Spatial_Files/TRT_files/icbma/'

# chinook
spsm_spwn_areas <- st_read(paste0(spatial_files,'icbmac.shp')) %>% 
  st_transform(crs = crs_default) %>%
  filter(ESU_NAME == 'Snake Spring/Summer Chinook') %>%
  filter(!(POP_NAME %in% c('NCUMA', 'NCLMA')))

save_spatial_layers(spsm_spwn_areas, type = 'raw')

# steelhead
sthd_spwn_areas <- st_read(paste0(spatial_files,'icbmas.shp')) %>% 
  st_transform(crs = crs_default) %>%
  filter(ESU_NAME == 'Snake River Steelhead') %>%
  filter(POP_NAME != 'CRNFC-s')

save_spatial_layers(sthd_spwn_areas, type = 'raw')


# combine populations to each huc10
huc10_pnt <- st_point_on_surface(huc10)

huc10_pops <- huc10_pnt %>%
  st_join(spsm_pops %>%
            select(spsm_MPG = MPG, spsm_POP_NAME = POP_NAME, spsm_TRT_POPID = TRT_POPID)) %>%
  st_join(sthd_pops %>%
            select(sthd_MPG = MPG, sthd_POP_NAME = POP_NAME, sthd_TRT_POPID = TRT_POPID)) %>%
  st_join(fall_pops %>%
            select(fall_MPG = MPG, fall_POP_NAME = POP_NAME, fall_TRT_POPID = TRT_POPID)) %>%
  st_set_geometry(NULL) %>%
  select(-HU_10_NAME)

huc10 <- inner_join(huc10, huc10_pops, by = 'HUC_10')


# load stream layers----
# ip results
spatial_files <- 'C://Spatial_Files/QGIS Files/NAD83_11N_26911/'
ip <- st_read(paste0(spatial_files,'TRT_ICB_IP/snake_basin_intrinsic_potential.shp')) %>%
  st_transform(crs = crs_default)


ip <- ip %>%
  mutate(spsm_wt = case_when(CHINRATE == 3 ~ 1, # appendix C Cooney and Holzer 2006
                                 CHINRATE == 2 ~ .5,
                                 CHINRATE == 1 ~ .25,
                                 TRUE ~ 0)) %>%
  mutate(sthd_wt = case_when(STHDRATE == 3 ~ 1, # appendix C Cooney and Holzer 2006
                                 STHDRATE == 2 ~ .5,
                                 STHDRATE == 1 ~ .25,
                                 TRUE ~ 0))

ip_spwn <- st_join(ip,
                   spsm_spwn_areas %>%
                     select(spsm_MSA = MSA_NAME, spsm_spwn_type = TYPE),
                   join = st_covered_by,
                   left = TRUE)

ip_spwn <- st_join(ip_spwn,
                   sthd_spwn_areas %>%
                     select(sthd_MSA = MSA_NAME, sthd_spwn_type = TYPE),
                   join = st_covered_by,
                   left = TRUE)

ip <- ip_spwn

save_spatial_layers(ip, type = 'raw')

# qrf results
spatial_files <- 'C://Spatial_Files/QRF_Data_814/'

qrf_sum <- st_read(paste0(spatial_files,'Rch_Cap_RF_No_elev_juv_summer.gpkg')) %>%
  st_transform(crs = crs_default) %>%
  st_filter(huc10)

qrf_huc <- qrf_sum %>%
  st_join(huc10)

# now left join winter and redd
qrf_win <- st_read(paste0(spatial_files,'Rch_Cap_RF_No_elev_juv_winter.gpkg')) %>%
  st_transform(crs = crs_default) %>%
  st_filter(huc10)

qrf_huc <- qrf_huc %>%
  rename(spsm_sm = chnk_per_m, spsm_sm2 = chnk_per_m2, sthd_sm = sthd_per_m, sthd_sm2 = sthd_per_m2) %>%
  left_join(qrf_win %>%
              st_set_geometry(NULL) %>%
              select(UniqueID, spsm_w = chnk_per_m, spsm_w2 = chnk_per_m2, sthd_w = sthd_per_m, sthd_w2 = sthd_per_m2))


qrf_redds <- st_read(paste0(spatial_files,'Rch_Cap_RF_No_elev_redds.gpkg')) %>%
  st_transform(crs = crs_default) %>%
  st_filter(huc10)


qrf <- qrf_huc %>%
  left_join(qrf_redds %>%
              st_set_geometry(NULL) %>%
              select(UniqueID, spsm_r = chnk_per_m, spsm_r2 = chnk_per_m2, sthd_r = sthd_per_m, sthd_r2 = sthd_per_m2))


tmp <- qrf %>%
  select(UniqueID:reach_leng, HUC_10, HU_10_NAME, HUC6_code:chnk_use, sthd, sthd_use, Watershed, model,
         spsm_MPG:spsm_TRT_POPID,
         sthd_MPG = sthd_MPG.y, sthd_POP_NAME:fall_TRT_POPID,
         spsm_sm, spsm_sm2, spsm_w, spsm_w2, spsm_r, spsm_r2,
         sthd_sm, sthd_sm2, sthd_w, sthd_w2, sthd_r, sthd_r2)


qrf_spwn <- st_join(tmp,
                    spsm_spwn_areas %>%
                      select(spsm_MSA = MSA_NAME, spsm_spwn_type = TYPE),
                    join = st_covered_by,
                    left = TRUE)

qrf_spwn <- st_join(qrf_spwn,
                    sthd_spwn_areas %>%
                      select(sthd_MSA = MSA_NAME, sthd_spwn_type = TYPE),
                    join = st_covered_by,
                    left = TRUE)

qrf <- qrf_spwn

save_spatial_layers(qrf, type = 'raw') ### NEED TO READ IN AND CLEAN UP COLUMN NAMES.


# Calculate polygon metrics----

# by HUC10
huc10_metrics <- summarize_spatial_metrics(qrf, ip, poly = huc10, grp_var = HUC_10)
save_spatial_layers(huc10_metrics, type = 'summarized')


# summarize by population boundaries
spsm_pop_metrics <- summarize_spatial_metrics(qrf,
                                              ip = ip %>%
                                                rename(spsm_TRT_POPID = spsm_TRT_P),
                                              poly = spsm_pops %>%
                                                rename(spsm_TRT_POPID = TRT_POPID),
                                              grp_var = spsm_TRT_POPID)

save_spatial_layers(spsm_pop_metrics, type = 'summarized')


sthd_pop_metrics <- summarize_spatial_metrics(qrf,
                                              ip = ip %>%
                                                rename(sthd_TRT_POPID = sthd_TRT_P),
                                              poly = sthd_pops %>%
                                                rename(sthd_TRT_POPID = TRT_POPID),
                                              grp_var = sthd_TRT_POPID)

save_spatial_layers(sthd_pop_metrics, type = 'summarized')  

# summarize by spawn area boundaries
spsm_spwn_area_metrics <- summarize_spatial_metrics(qrf,
                                                    ip,
                                                    poly = spsm_spwn_areas %>%
                                                      rename(spsm_MSA = MSA_NAME),
                                                    grp_var = spsm_MSA)

save_spatial_layers(spsm_spwn_area_metrics, type = 'summarized')


sthd_spwn_area_metrics <- summarize_spatial_metrics(qrf,
                                                    ip,
                                                    poly = sthd_spwn_areas %>%
                                                      rename(sthd_MSA = MSA_NAME),
                                                    grp_var = sthd_MSA)

save_spatial_layers(sthd_spwn_area_metrics, type = 'summarized')


# save to csv for managers.
tmp <- huc10_metrics %>%
  st_set_geometry(NULL) %>%
  mutate(pop_huc = case_when(
    !is.na(spsm_MPG) ~ TRUE,
    !is.na(sthd_MPG) ~ TRUE,
    !is.na(fall_MPG) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(pop_huc) %>%
  select(-pop_huc)

write_csv(tmp,
          file = './data/huc10_metrics.csv')







