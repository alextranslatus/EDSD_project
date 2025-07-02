# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

rm(list = ls())
gc()

  
### HHGRID MCS1 ####
hhgrid1 <- read_dta("data/datamcs/mcs1_hhgrid.dta")

hhgrid1_tw <- hhgrid1 %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
hhgrid1 <- bind_cols(hhgrid1, hhgrid1_tw)
rm(hhgrid1_tw)

hhgrid1 <- hhgrid1 %>%
  filter(ACNUM00 %in% c(1, 2)) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(ACNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(hhgrid1, "data/datamcs/clean/mcs1_hhgrid_clean.dta")

### HHGRID MCS2 ####
hhgrid2 <- read_dta("data/datamcs/mcs2_hhgrid.dta")

hhgrid2_tw <- hhgrid2 %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
hhgrid2 <- bind_cols(hhgrid2, hhgrid2_tw)
rm(hhgrid2_tw)

hhgrid2 <- hhgrid2 %>%
  filter(BCNUM00 %in% c(1, 2)) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(BCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(hhgrid2, "data/datamcs/clean/mcs2_hhgrid_clean.dta")

### HHGRID MCS3 ####
hhgrid3 <- read_dta("data/datamcs/mcs3_hhgrid.dta")

hhgrid3_tw <- hhgrid3 %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
hhgrid3 <- bind_cols(hhgrid3, hhgrid3_tw)
rm(hhgrid3_tw)

hhgrid3 <- hhgrid3 %>%
  filter(CCNUM00 %in% c(1, 2)) %>%
  mutate(across(ends_with("_tw"), ~ ifelse(CCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(hhgrid3, "data/datamcs/clean/mcs3_hhgrid_clean.dta")

