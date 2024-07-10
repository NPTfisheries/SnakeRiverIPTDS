# -----------------------
# Author: Mike Ackerman
# Purpose: Compile valid abundance estimates and coefficients of variations
#            for Snake River populations.
# 
# Created: April 4, 2024
#   Last Modified:
# 
# Notes:

# clear environment
rm(list = ls())

# load packages
library(readxl)
library(tidyverse)
library(here)

# compile data
sthd_df = read_excel("C:/Git/SnakeRiverFishStatus/output/syntheses/LGR_Steelhead_all_summaries_2024-06-20.xlsx",
                     sheet = "Pop_Tot_Esc",
                     progress = F)

chnk_df = read_excel("C:/Git/SnakeRiverFishStatus/output/syntheses/LGR_Chinook_all_summaries_2024-06-20.xlsx",
                     sheet = "Pop_Tot_Esc",
                     progress = F)
  
# steelhead
sthd_pops = data.frame(TRT_POPID = 
                         c("SRUMA-s", "SREFS-s", "SRPAH-s", "SRLEM-s", "SRNFS-s", "SRPAN-s", "SRCHA-s", # Salmon River
                           "MFUMA-s", "MFBIG-s", "SFMAI-s", "SFSEC-s", "SRLSR-s", 
                           "CRSEL-s", "CRLOC-s", "CRSFC-s", "CRLOL-s", "CRNFC-s", "CRLMA-s",            # Clearwater River
                           "IRMAI-s",                                                                   # Imnaha River
                           "GRUMA-s", "GRWAL-s", "GRJOS-s", "GRLMT-s",                                  # Grande Ronde River
                           "SNHCT-s",                                                                   # Hells Canyon
                           "SNASO-s", "SNTUC-s"))                                                       # Lower Snake

sthd_cvs = sthd_df %>%
  select(spawn_yr, TRT_POPID, valid_est, cv) %>%
  filter(valid_est == 1) %>%
  select(-valid_est) %>%
  mutate(cv = cv * 100) %>%
  pivot_wider(names_from = spawn_yr,
              values_from = cv) %>%
  # calculate the mean CV across years with valid estimates
  mutate(Mean = rowMeans(select(., `2010`:`2023`), na.rm = T)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 1))) %>%
  # fill in missing populations
  right_join(sthd_pops) %>%
  # arrange to match sthd_pops
  arrange(match(TRT_POPID, sthd_pops$TRT_POPID))

# chinook
chnk_pops = data.frame(TRT_POPID = 
                         c("SRUMA", "SRVAL", "SRLMA", "SRYFS", "SREFS", "SRPAH", "SRLEM", "SRNFS", "SRPAN", # Upper Salmon River
                           "MFBEA", "MFMAR", "MFSUL", "MFUMA", "MFLMA", "MFLOO", "MFCAM", "MFBIG", "SRCHA", # Middle Fork Salmon
                           "SFSMA", "SFEFS", "SFSEC", "SRLSR",                                              # South Fork Salmon
                           "IRBSH", "IRMAI", "GRUMA", "GRCAT", "GRLOO", "GRMIN", "GRLOS", "GRWEN",          # Grande Ronde / Imnaha
                           "SNASO", "SNTUC"))                                                               # Lower Snake

chnk_cvs = chnk_df %>%
  select(spawn_yr, TRT_POPID, valid_est, cv) %>%
  filter(valid_est == 1) %>%
  select(-valid_est) %>%
  mutate(cv = cv * 100) %>%
  pivot_wider(names_from = spawn_yr,
              values_from = cv) %>%
  # calculate the mean CV across years with valid estimates
  mutate(Mean = rowMeans(select(., `2010`:`2023`), na.rm = T)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 1))) %>%
  # fill in missing populations
  right_join(chnk_pops) %>%
  # arrange to match sthd_pops
  arrange(match(TRT_POPID, chnk_pops$TRT_POPID))                      

# write tables to .csvs
write_csv(sthd_cvs,
          file = here("output/prioritization/sthd_population_cvs.csv"))
write_csv(chnk_cvs,
          file = here("output/prioritization/chnk_population_cvs.csv"))

# END SCRIPT