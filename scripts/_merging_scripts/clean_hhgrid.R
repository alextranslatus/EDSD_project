# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

setwd("~/Desktop/EDSD project/EDSDalex/datamcs")

rm(list = ls())
gc()

  
### HHGRID MCS1 ####
hhgrid1 <- read_dta("mcs1_hhgrid.dta")

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

write_dta(hhgrid1, "clean/mcs1_hhgrid_clean.dta")

