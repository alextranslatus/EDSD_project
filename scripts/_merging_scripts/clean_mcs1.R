# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

rm(list = ls())
gc()

tryCatch({
  
### PARENT DERIVED ####
parent_derived <- read_dta("data/datamcs/mcs1_parent_derived.dta")

# Confirm that main interviewee is the mother
table(parent_derived$AELIG00[parent_derived$ADDRES00 == 1]) # 7 main respondents are fathers
table(parent_derived$AELIG00[parent_derived$ADDRES00 == 2]) # 28 partner respondents are mothers

# ADDRES00 respondent identity and interview status
# (13.0) Step mother: by proxy   (14.0) Step father: by proxy   (1.0) Natural mother: interviewed  
# (2.0) Natural father: interviewed     (3.0) Adoptive mother: interviewed    (4.0) Adoptive father: interviewed 
# (5.0) Foster mother: interviewed      (6.0) Foster father: interviewed      (7.0) Step mother/partner of father: interviewed 
# (8.0) Step father/partner of mother: interviewed      (9.0) Grandmother: interviewed       
# (10.0) Grandfather: interviewed        (11.0) Natural mother: by proxy        (12.0) Natural father: by proxy 
# (15.0) Natural mother: not interviewed (16.0) Natural father: not interviewed (17.0) Adoptive mother: not interviewed   
# (18.0) Adoptive father: not interviewed        (19.0) Foster mother: not interviewed  (20.0) Foster father: not interviewed 
# (21.0) Step mother: not interviewed    (22.0) Step father: not interviewed    (23.0) Natural mother: by proxy, not interviewed 
# (24.0) Natural father: by proxy, not interviewed       (25.0) Other female non-relative: interviewed  
# (26.0) Other male non-relative: interviewed    (27.0) Other female non-relative: not interviewed    
# (28.0) Other male non-relative: not interviewed        (29.0) Step mother: by proxy, not interviewed 
# (30.0) Step father: by proxy, not interviewed  (31.0) Other female relative: interviewed     
# (32.0) Other male relative: interviewed        (33.0) Female, unknown relationship: interviewed    
# (34.0) Male, unknown relationship: interviewed

# AELIG00 Eligibility for survey: Whether resp eligible for role of Main /(Proxy)Partner
# (1.0) Main Interview (2.0) Partner Interview (3.0) Proxy Interview (4.0) Not eligible

# Duplicate all variables for second parent
parent_derived_p <- parent_derived %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_derived_m <- parent_derived %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_derived <- bind_cols(parent_derived_m, parent_derived_p)
rm(parent_derived_p, parent_derived_m)

# Make first set valid for first parent, second for partner
parent_derived <- parent_derived %>%
  mutate(across(ends_with("_m"), ~ ifelse(AELIG00_m != 1, NA, .))) %>% 
  # need to fix this, add condition that _m needs to be ADDRES00 = 1, and _p ADDRES00 = 2
  # maybe find how to redistribute the mother partner interview as _m
  # maybe find how to redistribute the father main interview as _p
  mutate(across(ends_with("_p"), ~ ifelse(AELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% # Pull partner values into first record
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>% # Keep only first record per mcsid
  ungroup()

# Save cleaned data
write_dta(parent_derived, "data/datamcs/clean/mcs1_parent_derived_clean.dta")

### PARENT INTERVIEW ####
parent_interview <- read_dta("data/datamcs/mcs1_parent_interview.dta")

parent_interview_p <- parent_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_interview_m <- parent_interview %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_interview <- bind_cols(parent_interview_m, parent_interview_p)
rm(parent_interview_p, parent_interview_m)

parent_interview <- parent_interview %>%
  mutate(across(ends_with("_m"), ~ ifelse(AELIG00_m != 1, NA, .))) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(AELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(parent_interview, "data/datamcs/clean/mcs1_parent_interview_clean.dta")

### PARENT CM INTERVIEW ####
parent_cm_interview <- read_dta("data/datamcs/mcs1_parent_cm_interview.dta")
table(parent_cm_interview$AELIG00[parent_cm_interview$ADDRES00 == 1])

parent_cm_interview_p <- parent_cm_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_cm_interview_m <- parent_cm_interview %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_cm_interview <- bind_cols(parent_cm_interview_m, parent_cm_interview_p)
rm(parent_cm_interview_p, parent_cm_interview_m)

parent_cm_interview <- parent_cm_interview %>%
  mutate(across(ends_with("_m"), ~ ifelse(AELIG00_m != 1, NA, .))) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(AELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(parent_cm_interview, "data/datamcs/clean/mcs1_parent_cm_interview_clean.dta")

### FAMILY DERIVED ####
family_derived <- read_dta("data/datamcs/mcs1_family_derived.dta") # one line for twins
write_dta(family_derived, "data/datamcs/clean/mcs1_family_derived_clean.dta")

### GEO LINKED DATA ####
geographically_linked_data <- read_dta("data/datamcs/mcs1_geographically_linked_data.dta")
names(geographically_linked_data) <- toupper(names(geographically_linked_data))
write_dta(geographically_linked_data, "data/datamcs/clean/mcs1_geographically_linked_data_clean.dta")

### CM INTERVIEW ####
cm_interview <- read_dta("data/datamcs/mcs1_cm_interview.dta")

cm_interview_tw <- cm_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_interview <- bind_cols(cm_interview, cm_interview_tw)
rm(cm_interview_tw)

cm_interview <- cm_interview %>%
  mutate(across(ends_with("_tw"), ~ ifelse(ACNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_interview, "data/datamcs/clean/mcs1_cm_interview_clean.dta")

### CM DERIVED ####
cm_derived <- read_dta("data/datamcs/mcs1_cm_derived.dta")

cm_derived_tw <- cm_derived %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_derived <- bind_cols(cm_derived, cm_derived_tw)
rm(cm_derived_tw)

cm_derived <- cm_derived %>%
  mutate(across(ends_with("_tw"), ~ ifelse(ACNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_derived, "data/datamcs/clean/mcs1_cm_derived_clean.dta")

### MERGE FILES ####
parent_derived <- read_dta("data/datamcs/clean/mcs1_parent_derived_clean.dta")
parent_interview <- read_dta("data/datamcs/clean/mcs1_parent_interview_clean.dta")
parent_cm_interview <- read_dta("data/datamcs/clean/mcs1_parent_cm_interview_clean.dta")
family_derived <- read_dta("data/datamcs/clean/mcs1_family_derived_clean.dta")
geographically_linked_data <- read_dta("data/datamcs/clean/mcs1_geographically_linked_data_clean.dta")
cm_interview <- read_dta("data/datamcs/clean/mcs1_cm_interview_clean.dta")
cm_derived <- read_dta("data/datamcs/clean/mcs1_cm_derived_clean.dta")

mcs1 <- full_join(parent_derived, parent_interview, by = "MCSID")
mcs1 <- full_join(mcs1, parent_cm_interview, by = "MCSID")
mcs1 <- full_join(mcs1, family_derived, by = "MCSID")
mcs1 <- full_join(mcs1, geographically_linked_data, by = "MCSID")
mcs1 <- full_join(mcs1, cm_interview, by = "MCSID")
mcs1 <- full_join(mcs1, cm_derived, by = "MCSID")

mcs1 <- mcs1 %>%
  select(-ends_with(".x"), -ends_with(".y"))

write_dta(mcs1, "data/datamcs/clean/mcs1_clean.dta")

}, checkerror = function(e) {
  beep(8)  # Play an alert sound on error
  message("Error: ", e$message)  # Print the error message
})

beep(6)


