#-----------------------
# Author: Mike Ackerman
# Purpose: Testing various BioLogic API requests
# 
# Created: May 6, 2024
#   Last Modified: July 10, 2024
# 
# Notes: 

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
# remotes::install_github("mackerman44/fisheR")
library(fisheR)
library(janitor)

# log into Biomark's BioLogic database to retrive an API token
source(here("keys/biologic_login.txt"))
biologic_login(email, password)

# retrieve vector of sites that user has permissions
biologic_sites = get_biologic_sites()

# some parameters for BioLogic functions
site = c("KRS", "SFG", "ZEN", "ESS")
begin_dt = "2024-01-01"
end_dt = Sys.Date()

# pass API token to BioLogic; retrieve site environmental data
site_env_df = get_biologic_data(site = site[1],
                                endpoint = "enviro",
                                begin_dt = begin_dt,
                                end_dt = end_dt) %>%
  select(reader.site.slug,
         parameter.slug,
         parameter.units,
         read_at,
         value)

# which metrics are available?
site_env_df %>%
  tabyl(parameter.slug)

# pass API token to BioLogic; retrieve tag data for a site (possibility to return test tags?)
# tag_df = get_biologic_data(site = site[1],
#                            endpoint = "tags",
#                            begin_dt = begin_dt,
#                            end_dt = end_dt) %>%
#   select(-antenna.reader.site.name)

# pass API token to BioLogic; retrieve reader data for a site (includes battery and "in" voltage)
reader_df = get_biologic_data(site = site[1],
                              endpoint = "reader",
                              begin_dt = begin_dt,
                              end_dt = end_dt) %>%
  select(reader.site.slug,
         parameter.slug,
         parameter.units,
         reader.code,
         read_at,
         value)

# which metrics are available?
reader_df %>%
  tabyl(parameter.slug)

# pass API token to BioLogic; retrieve antenna data (includes capacitor, current, and noise)
antenna_df = get_biologic_data(site = site[1],
                               endpoint = "antenna",
                               begin_dt = "2024-04-01",
                               end_dt = end_dt) %>%
  filter(parameter.slug %in% c("current", "noise")) %>%
  select(antenna.reader.site.slug,
         parameter.slug,
         parameter.units,
         antenna.reader.code,
         antenna.code,
         read_at,
         value)

# which metrics are available?
antenna_df %>%
  tabyl(parameter.slug)

### END SCRIPT
