#Purpose: Create figures of IP and QRF summaries.
# Author: Ryan N. Kinzer
# Created: March 20th, 2024


# Load needed libraries -----
library(tidyverse)
library(sf)
library(scales)
library(ggmap)
library(maps)
#library(albersusa)
library(patchwork)

source(here::here("scripts", "plot_spatial_polygons.R"))

# intrinsic potential plots----

ip <- readRDS(here::here("data", "map_layers", "raw", "ip.rds"))
qrf <- readRDS(here::here("data", "map_layers", "raw", "qrf.rds"))


huc10_metrics <- readRDS(here::here("data", "map_layers", "summarized", "huc10_metrics.rds")) %>%
  filter(sthd_TRT_POPID != 'CRNFC-s')

spsm_pop_metrics <- readRDS(here::here("data", "map_layers", "summarized", "spsm_pop_metrics.rds"))
         
spsm_spwn_area_metrics <- readRDS(here::here("data", "map_layers", "summarized", "spsm_spwn_area_metrics.rds"))

sthd_pop_metrics <- readRDS(here::here("data", "map_layers", "summarized", "sthd_pop_metrics.rds"))

sthd_spwn_area_metrics <- readRDS(here::here("data", "map_layers", "summarized", "sthd_spwn_area_metrics.rds"))



# chinook ip
chnk_ip_stream <- ip %>%
  filter(CHINRATE != 0) #%>%
# mutate(CHINRATE = factor(CHINRATE, levels = c(1, 2, 3), labels = c('Low', 'Medium', 'High')))

chnk_ip_stream_fig <- ggplot() +
  #geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = chnk_ip_stream, aes(colour = CHINRATE)) +
  #scale_colour_viridis_d(option = 'turbo', direction = -1) +
  #scale_color_distiller(palette = 'Spectral') +
  # scale_color_manual(values = c('High' = 'red','Medium' =  'green','Low' =  'blue')) +
  scale_colour_distiller(palette = "RdYlBu") +
  labs(title = "NOAA's Intrinsic Potential",
       subtitle = "Sp/sm Chinook Salmon",
       colour = NULL) +
  theme_void() +
  theme(legend.position = 'bottom')


# steelhead ip
sthd_ip_stream <- ip %>%
  filter(STHDRATE != 0) #%>%
#mutate(STHDRATE = factor(STHDRATE, levels = c(1, 2, 3), labels = c('Low', 'Medium', 'High')))

sthd_ip_stream_fig <- ggplot() +
  #geom_sf(data = huc10, fill = 'black') +
  geom_sf(data = sthd_ip_stream, aes(colour = STHDRATE)) +
  # scale_color_manual(values = c('High' = 'red','Medium' =  'green','Low' =  'blue')) +
  scale_colour_distiller(palette = "RdYlBu") +
  labs(subtitle = "Summer Steelhead",
       caption = 'Warmer colors have a higher historical potential based on geomorphic characteristics.',
       colour = NULL) +
  theme_void() +
  theme(legend.position = 'bottom')


ip_stream_fig <- (chnk_ip_stream_fig + sthd_ip_stream_fig) +
  plot_layout(guides = 'collect') &
  theme(plot.background = element_rect(fill = 'black'),
        text = element_text(colour = 'white'),
        legend.position='none')

#ip_stream_fig
ggsave('./figures/noaa_intrinsic_potential.png', ip_stream_fig, width = 11, height = 8.5)


# IP by polygons.

chnk_pop_fig <- ggplot() +
  geom_sf(data = spsm_pop_metrics, aes(fill = spsm_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  labs(title = "NOAA's Intrinsic Potential",
       subtitle = "Sp/sm Chinook Salmon",
       colour = NULL) +
  theme_void()

chnk_huc_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = spsm_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()

chnk_spwn_fig <- ggplot() +
  geom_sf(data = spsm_spwn_area_metrics, aes(fill = spsm_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()


sthd_pop_fig <- ggplot() +
  geom_sf(data = sthd_pop_metrics, aes(fill = sthd_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  labs(subtitle = "Summer Steelhead",
       colour = NULL) +
  theme_void()


sthd_huc_fig <- ggplot() +
  geom_sf(data = huc10_metrics, aes(fill = sthd_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  theme_void()


sthd_spwn_fig <- ggplot() +
  geom_sf(data = sthd_spwn_area_metrics, aes(fill = sthd_ip)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  labs(caption = 'Warmer colors have a higher historical potential based on geomorphic characteristics.') +
  theme_void()


ip_poly_fig <- (chnk_pop_fig + sthd_pop_fig) / (chnk_huc_fig + sthd_huc_fig) / (chnk_spwn_fig + sthd_spwn_fig) +
  plot_layout(guides = 'collect') &
  theme(plot.background = element_rect(fill = 'black'),
        text = element_text(colour = 'white'),
        legend.position='none')

ip_poly_fig

ggsave('./figures/noaa_ip_poly.png', ip_poly_fig, width = 11, height = 8.5)

# qrf

num_groups <- 4

chnk_qrf_stream <- qrf %>%
  filter(chnk == 1) %>%
  mutate(sum_parr = cut(spsm_sm,
                        breaks = quantile(
                          .$spsm_sm, probs = seq(0, 1, length.out = num_groups + 1)
                        ))) %>%
  mutate(win_parr = cut(spsm_w,
                        breaks = quantile(
                          .$spsm_w, probs = seq(0, 1, length.out = num_groups + 1)
                        )))

chnk_qrf_sum_fig <- ggplot() +
  #geom_sf(data = huc10_metrics, fill = 'black') +
  geom_sf(data = chnk_qrf_stream, aes(colour = sum_parr)) +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(title = "Sp/sm Chinook Salmon",
       subtitle = "Summer Parr Capacity") +
  theme_void() +
  theme(legend.position = 'none')


chnk_qrf_winter_fig <- ggplot() +
  #geom_sf(data = huc10_metrics, fill = 'black') +
  geom_sf(data = chnk_qrf_stream, aes(colour = win_parr)) +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(subtitle = "Overwintering Capacity") +
  theme_void() +
  theme(legend.position = 'none')


sthd_qrf_stream <- qrf %>%
  filter(sthd == 1) %>%
  mutate(sum_parr = cut(sthd_sm,
                        breaks = quantile(
                          .$sthd_sm, probs = seq(0, 1, length.out = num_groups + 1)
                        ))) %>%
  mutate(win_parr = cut(sthd_w,
                        breaks = quantile(
                          .$sthd_w, probs = seq(0, 1, length.out = num_groups + 1)
                        )))


sthd_qrf_sum_fig <- ggplot() +
  geom_sf(data = sthd_qrf_stream, aes(colour = sum_parr)) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Summer Steelhead",
       subtitle = "Summer Parr Capacity"
  ) +
  theme_void() +
  theme(legend.position = 'none')


sthd_qrf_winter_fig <- ggplot() +
  geom_sf(data = sthd_qrf_stream, aes(colour = win_parr)) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  labs(subtitle = "Overwintering Capacity",
       caption = 'Warmer colors have a higher predicted fish density based on geomorphic characteristics.') +
  theme_void() +
  theme(legend.position = 'none')

qrf_stream_fig <- (chnk_qrf_sum_fig + chnk_qrf_winter_fig) / (sthd_qrf_sum_fig + sthd_qrf_winter_fig) +
  plot_layout(guides = 'collect') &
  theme(plot.background = element_rect(fill = 'black'),
        text = element_text(colour = 'white'),
        legend.position='none')

ggsave('./figures/qrf_stream_capacity.png', qrf_stream_fig, width = 11, height = 8.5)


# summarized polygon plots

# spring/summer
spsm_huc_fig <- plot_spatial_polygons(poly = huc10_metrics, species = 'spsm', plot_title = 'Sp/sm Chinook Salmon - HUC10')
ggsave('./figures/spsm_huc_capacity.png', spsm_huc_fig, width = 11, height = 8.5)


spsm_pop_fig <- plot_spatial_polygons(poly = spsm_pop_metrics, species = 'spsm', plot_title = 'Sp/sm Chinook Salmon - Populations')
ggsave('./figures/spsm_pop_capacity.png', spsm_pop_fig, width = 11, height = 8.5)

spsm_spwn_fig <- plot_spatial_polygons(poly = spsm_spwn_area_metrics, species = 'spsm', plot_title = 'Sp/sm Chinook Salmon - Spawn Areas')
ggsave('./figures/spsm_spwn_capacity.png', spsm_spwn_fig, width = 11, height = 8.5)

# Steelhead
sthd_huc_fig <- plot_spatial_polygons(poly = huc10_metrics, species = 'sthd', plot_title = 'Summer Steelhead - HUC10')
ggsave('./figures/sthd_huc_capacity.png', sthd_huc_fig, width = 11, height = 8.5)


sthd_pop_fig <- plot_spatial_polygons(poly = sthd_pop_metrics, species = 'sthd', plot_title = 'Summer Steelhead - Populations')
ggsave('./figures/sthd_pop_capacity.png', sthd_pop_fig, width = 11, height = 8.5)

sthd_spwn_fig <- plot_spatial_polygons(poly = sthd_spwn_area_metrics, species = 'sthd', plot_title = 'Summer Steelhead - Spawn Areas')
ggsave('./figures/sthd_spwn_capacity.png', sthd_spwn_fig, width = 11, height = 8.5)



fig <- ggplot() +
  geom_sf(data = spsm_pop_metrics, aes(fill = spsm_sm)) +
  #scale_fill_viridis_c(option = 'mako', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu') +
  labs(subtitle = 'Redd Capacity') +
  theme_void()

fig
