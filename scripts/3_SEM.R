
for(
  pkg in c(
    "lavaan", # SEM
    "dplyr", # basics
    "data.table" # data.table 
  )
){
  if(!require(pkg, quietly = TRUE, character.only = TRUE)){
    install.packages(pkg)
  }
}

# Functions ####
source("scripts/functions.R")

# Data ####
load(file = "/Users/alexsheridan/Documents/Work/EDSD project/R/EDSD_project/data/analysisdata/elfemini.Rdata")
load(file = "/Users/alexsheridan/Documents/Work/EDSD project/R/EDSD_project/data/analysisdata/mcsmini.Rdata")

