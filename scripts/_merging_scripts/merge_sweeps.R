# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

rm(list = ls())
gc()

mcs1 <- read_dta("data/datamcs/clean/mcs1_clean.dta")
mcs2 <- read_dta("data/datamcs/clean/mcs2_clean.dta")
mcs3 <- read_dta("data/datamcs/clean/mcs3_clean.dta") %>% 
  select(-c("CHCPRS00", "CHCPRS00_tw"))
hhgrid1 <- read_dta("data/datamcs/clean/mcs1_hhgrid_clean.dta")
hhgrid2 <- read_dta("data/datamcs/clean/mcs2_hhgrid_clean.dta")
hhgrid3 <- read_dta("data/datamcs/clean/mcs3_hhgrid_clean.dta")
longitudinal_family_file <- read_dta("mcs_longitudinal_family_file.dta")

mcs123 <- full_join(mcs1, mcs2, by = "MCSID")
mcs123 <- full_join(mcs123, mcs3, by = "MCSID")
mcs123 <- full_join(mcs123, hhgrid1, by = "MCSID")
mcs123 <- full_join(mcs123, hhgrid2, by = "MCSID")
mcs123 <- full_join(mcs123, hhgrid3, by = "MCSID")
mcs123 <- full_join(mcs123, longitudinal_family_file, by = "MCSID")

mcs123 <- mcs123 %>%
  select(-ends_with(".x"), -ends_with(".y"))

write_dta(mcs123, "data/mcs123_clean.dta")





