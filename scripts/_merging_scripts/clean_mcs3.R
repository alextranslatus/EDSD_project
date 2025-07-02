# Load necessary libraries
library(dplyr)
library(readr)
library(haven)
library(beepr)

rm(list = ls())
gc()

tryCatch({
  
### PARENT DERIVED ####
parent_derived <- read_dta("data/datamcs/mcs3_parent_derived.dta")

# Confirm that main interviewee is the mother
table(parent_derived$CELIG00[parent_derived$CDDRES00 == 1])

# Duplicate all variables for second parent
parent_derived_p <- parent_derived %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_derived_m <- parent_derived %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_derived <- bind_cols(parent_derived_m, parent_derived_p)
rm(parent_derived_p, parent_derived_m)

# Make first set valid for first parent, second for partner
parent_derived <- parent_derived %>%
  mutate(across(ends_with("_m"), ~ ifelse(CELIG00_m != 1, NA, .))) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(CELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% # Pull partner values into first record
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>% # Keep only first record per mcsid
  ungroup()

# Save cleaned data
write_dta(parent_derived, "data/datamcs/clean/mcs3_parent_derived_clean.dta")

### PARENT INTERVIEW ####
parent_interview <- read_dta("data/datamcs/mcs3_parent_interview.dta")

parent_interview_p <- parent_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_interview_m <- parent_interview %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_interview <- bind_cols(parent_interview_m, parent_interview_p)
rm(parent_interview_p, parent_interview_m)

parent_interview <- parent_interview %>%
  mutate(across(ends_with("_m"), ~ ifelse(CELIG00_m != 1, NA, .))) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(CELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(parent_interview, "data/datamcs/clean/mcs3_parent_interview_clean.dta")

### PARENT CM INTERVIEW ####
parent_cm_interview <- read_dta("data/datamcs/mcs3_parent_cm_interview.dta")

parent_cm_interview_p <- parent_cm_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_p"))
parent_cm_interview_m <- parent_cm_interview %>% rename_with(~ paste0(., "_m"), .cols = -MCSID)
parent_cm_interview <- bind_cols(parent_cm_interview_m, parent_cm_interview_p)
rm(parent_cm_interview_p, parent_cm_interview_m)

parent_cm_interview <- parent_cm_interview %>%
  mutate(across(ends_with("_m"), ~ ifelse(CELIG00_m != 1, NA, .))) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(CELIG00_p == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_p"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(parent_cm_interview, "data/datamcs/clean/mcs3_parent_cm_interview_clean.dta")

### FAMILY DERIVED ####
family_derived <- read_dta("data/datamcs/mcs3_family_derived.dta") # one line for twins
write_dta(family_derived, "data/datamcs/clean/mcs3_family_derived_clean.dta")

### OLDER SIBLINGS ####
older_siblings_questionnaire <- read_dta("data/datamcs/mcs3_older_siblings_questionnaire.dta")
write_dta(older_siblings_questionnaire, "data/datamcs/clean/mcs3_older_siblings_questionnaire_clean.dta")

### PROXY PARTNER ####
proxy_partner_interview <- read_dta("data/datamcs/mcs3_proxy_partner_interview.dta")
write_dta(proxy_partner_interview, "data/datamcs/clean/mcs3_proxy_partner_interview_clean.dta")

### GEO LINKED DATA ####
geographically_linked_data <- read_dta("data/datamcs/mcs3_geographically_linked_data.dta")
names(geographically_linked_data) <- toupper(names(geographically_linked_data))
write_dta(geographically_linked_data, "data/datamcs/clean/mcs3_geographically_linked_data_clean.dta")

### CM TEACHER SURVEY ####
cm_teacher_survey <- read_dta("data/datamcs/mcs3_cm_teacher_survey.dta")

cm_teacher_survey_tw <- cm_teacher_survey %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_teacher_survey <- bind_cols(cm_teacher_survey, cm_teacher_survey_tw)
rm(cm_teacher_survey_tw)

cm_teacher_survey <- cm_teacher_survey %>%
  mutate(across(ends_with("_tw"), ~ ifelse(CCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_teacher_survey, "data/datamcs/clean/mcs3_cm_teacher_survey_clean.dta")
  
### CM COGNITIVE ####
cm_cognitive_assessment <- read_dta("data/datamcs/mcs3_cm_cognitive_assessment.dta")

cm_cognitive_assessment_tw <- cm_cognitive_assessment %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_cognitive_assessment <- bind_cols(cm_cognitive_assessment, cm_cognitive_assessment_tw)
rm(cm_cognitive_assessment_tw)

cm_cognitive_assessment <- cm_cognitive_assessment %>%
  mutate(across(ends_with("_tw"), ~ ifelse(CCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_cognitive_assessment, "data/datamcs/clean/mcs3_cm_cognitive_assessment_clean.dta")

### CM INTERVIEW ####
cm_interview <- read_dta("data/datamcs/mcs3_cm_interview.dta")

cm_interview_tw <- cm_interview %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_interview <- bind_cols(cm_interview, cm_interview_tw)
rm(cm_interview_tw)

cm_interview <- cm_interview %>%
  mutate(across(ends_with("_tw"), ~ ifelse(CCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_interview, "data/datamcs/clean/mcs3_cm_interview_clean.dta")

### CM DERIVED ####
cm_derived <- read_dta("data/datamcs/mcs3_cm_derived.dta")

cm_derived_tw <- cm_derived %>% select(-MCSID) %>% rename_all(~ paste0(., "_tw"))
cm_derived <- bind_cols(cm_derived, cm_derived_tw)
rm(cm_derived_tw)

cm_derived <- cm_derived %>%
  mutate(across(ends_with("_tw"), ~ ifelse(CCNUM00 == 1, NA, .))) %>%
  group_by(MCSID) %>% 
  mutate(across(ends_with("_tw"), ~ ifelse(is.na(.), lead(.), .))) %>%
  slice(1) %>%
  ungroup()

write_dta(cm_derived, "data/datamcs/clean/mcs3_cm_derived_clean.dta")

### MERGE FILES ####
parent_derived <- read_dta("data/datamcs/clean/mcs3_parent_derived_clean.dta")
parent_interview <- read_dta("data/datamcs/clean/mcs3_parent_interview_clean.dta")
parent_cm_interview <- read_dta("data/datamcs/clean/mcs3_parent_cm_interview_clean.dta")
family_derived <- read_dta("data/datamcs/clean/mcs3_family_derived_clean.dta")
older_siblings_questionnaire <- read_dta("data/datamcs/clean/mcs3_older_siblings_questionnaire_clean.dta")
proxy_partner_interview <- read_dta("data/datamcs/clean/mcs3_proxy_partner_interview_clean.dta")
geographically_linked_data <- read_dta("data/datamcs/clean/mcs3_geographically_linked_data_clean.dta")
cm_teacher_survey <- read_dta("data/datamcs/clean/mcs3_cm_teacher_survey_clean.dta")
cm_cognitive_assessment <- read_dta("data/datamcs/clean/mcs3_cm_cognitive_assessment_clean.dta")
cm_interview <- read_dta("data/datamcs/clean/mcs3_cm_interview_clean.dta")
cm_derived <- read_dta("data/datamcs/clean/mcs3_cm_derived_clean.dta")

mcs3 <- full_join(parent_derived, parent_interview, by = "MCSID")
mcs3 <- full_join(mcs3, parent_cm_interview, by = "MCSID")
mcs3 <- full_join(mcs3, family_derived, by = "MCSID")
mcs3 <- full_join(mcs3, older_siblings_questionnaire, by = "MCSID")
mcs3 <- full_join(mcs3, proxy_partner_interview, by = "MCSID")
mcs3 <- full_join(mcs3, geographically_linked_data, by = "MCSID")
mcs3 <- full_join(mcs3, cm_teacher_survey, by = "MCSID")
mcs3 <- full_join(mcs3, cm_cognitive_assessment, by = "MCSID")
mcs3 <- full_join(mcs3, cm_interview, by = "MCSID")
mcs3 <- full_join(mcs3, cm_derived, by = "MCSID")

mcs3 <- mcs3 %>%
  select(-ends_with(".x"), -ends_with(".y"))

write_dta(mcs3, "data/datamcs/clean/mcs3_clean.dta")

}, checkerror = function(e) {
  beep(8)  # Play an alert sound on error
  message("Error: ", e$message)  # Print the error message
})

beep(6)



