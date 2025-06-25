# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

setwd("~/Desktop/EDSD project/EDSDalex/datamcs")

rm(list = ls())
gc()

mcs1 <- read_dta("clean/mcs1_clean.dta")
mcs2 <- read_dta("clean/mcs2_clean.dta")
mcs3 <- read_dta("clean/mcs3_clean.dta")
hhgrid1 <- read_dta("clean/mcs1_hhgrid_clean.dta")
longitudinal_family_file <- read_dta("mcs_longitudinal_family_file.dta")

mcs123 <- full_join(mcs1, mcs2, by = "MCSID")
mcs123 <- full_join(mcs123, mcs3, by = "MCSID")
mcs123 <- full_join(mcs123, hhgrid1, by = "MCSID")
mcs123 <- full_join(mcs123, longitudinal_family_file, by = "MCSID")

write_dta(mcs123, "~/Desktop/EDSD project/EDSDalex/mcs123_clean.dta")





