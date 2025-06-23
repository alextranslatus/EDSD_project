
for(
  pkg in c(
    "readr", # read csv
    "haven", # read dta
    "readxl", # read xlsx
    "dplyr", # basics
    "data.table" # data.table
  )
){
  if(!require(pkg, quietly = TRUE, character.only = TRUE)){
    install.packages(pkg)
  }
}

# Loading Elfe
elfe <- read_csv("data/20250610DEM_1066_LP/DATA_DEM_1066_LP.csv")
dict_elfe <- read_delim("data/20250610DEM_1066_LP/CCT_DEM_1066_LP.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

eqr0 <- read_csv("data/20250610DEM_1066_LP/EQR0_SOCIODEMO.csv")
eqr5 <- read_csv("data/20250610DEM_1066_LP/EQR5_CODEAGEPROFESSIONSP.csv")
eqr12 <- read_csv("data/20250610DEM_1066_LP/EQR12_VARIABLESOCIODEMO.csv")
eqr28 <- read_csv("data/20250610DEM_1066_LP/EQR28_MCARTHUR.csv")
eqr39 <- read_csv("data/20250610DEM_1066_LP/EQR39_SCOREIDE.csv")

elfe <- full_join(elfe, eqr0, by = "id_DEM_1066_LP")
elfe <- full_join(elfe, eqr5, by = "id_DEM_1066_LP")
elfe <- full_join(elfe, eqr12, by = "id_DEM_1066_LP")
elfe <- full_join(elfe, eqr28, by = "id_DEM_1066_LP")
elfe <- full_join(elfe, eqr39, by = "id_DEM_1066_LP")

rm(list = ls(pattern = "^eqr"))

# Loading MCS
mcs <- read_dta("data/mcs123_clean.dta")
dict_mcs <- read_excel("data/mcs_catalog.xlsx")

# To data.table
elfe <- as.data.table(elfe)
mcs <- as.data.table(mcs)

# Functions ####
source("scripts/functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weights ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe <- elfe %>% rename(wgt1 = A01E_PONDREF,
                      wgt2 = A02E_PONDREF,
                      wgt3 = A03E_PONDREF,
                      wgt5 = A05E_PONDREF)
# MCS

mcs <- mcs %>% rename(wgt9m = AOVWT2,
                      wgt3 = BOVWT2,
                      wgt5 = COVWT2)

weights_elfe <- c("wgt1", "wgt2", "wgt3", "wgt5")
weights_mcs <- c("wgt9m", "wgt3", "wgt5")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe <- elfe %>% rename(sex = SEXE_ENF)

# MCS

mcs <- mcs %>% rename(sex = AHCSEX00)

sex_both <- c("sex") # 1 = "Male", 2 = "Female"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Education ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# Use info from the earlier wave possible: if info missing at 2m, take available info at 1y, etc.
elfe[, meducaf_imp := fcoalesce(meducaf_2m, meducaf_1y, meducaf_2y, meducaf_3y)]

# Three groups
elfe[meduc %in% c(1, 2, 3), meduc3 := 1]  # Up to and including bac
elfe[meduc == 4,            meduc3 := 2]  # Bac +2 (consider including bac if desired)
elfe[meduc %in% c(5, 6),    meduc3 := 3]  # Higher than bac +2
 
# MCS

mcs[APACQU00_m == 96 | APVCQU00_m == 96, meduc:= 1] # None of these qualifications | None of these qualifications 
mcs[APACQU00_m == 95 | APVCQU00_m == 95, meduc:= 2] # Other academic qualifications | Other vocational qualifications
mcs[APACQU00_m == 6  | APVCQU00_m == 6,  meduc:= 3] # GCSE grades D-G | NVQ / SVQ / GSVQ level 1
mcs[APACQU00_m == 5  | APVCQU00_m == 5,  meduc:= 4] # O level / GCSE grades A-C | NVQ / SVQ / GSVQ level 2
mcs[                   APVCQU00_m == 4,  meduc:= 5] # Trade apprenticeships
mcs[APACQU00_m == 4  | APVCQU00_m == 3,  meduc:= 6] # A / AS / S levels | NVQ / SVQ / GSVQ level 3
mcs[APACQU00_m == 3  | APVCQU00_m == 2,  meduc:= 7] # Dipl in higher ed | Nursing / other medical qualifications
mcs[APACQU00_m == 2  | APVCQU00_m == 1,  meduc:= 8] # First degree |  Professional quals at degree level
mcs[APACQU00_m == 1                   ,  meduc:= 9] # Higher degree

# Three groups
mcs[meduc %in% c(1, 2, 3, 4), meduc3:= 1] # Up to and including GCSEs
mcs[meduc %in% c(5, 6, 7), meduc3:= 2] # Trade, A-level and HE below degree
mcs[meduc %in% c(8, 9), meduc3:= 3] # Bachelor's degree or Higher degree
mcs[meduc == 1 & APLFTE00_m > 16 & !is.na(APLFTE00_m), meduc3:= 2] 

# Cross-checked with DICE documentation: the code is correct. Differences with DICE tabulations
# are because they look at parental edu and not just maternal edu.

edu_both <- c("meduc3") # 1 = "Low", 2 = "Medium", 3 = "High"
edu_elfe <- c("meduc") # 1 = "<=bepc",  2 = "cap-bep", 3 = "bac", 4 = "bac+2", 5 = "bac+3/4", 6 = ">bac+4"
edu_mcs <- c("meduc") # 1 = "None", 2 = "Other", 3 = "GCSE D-G", 4 = "GCSE A-C", 5 = "Trade", 6 = "A-level", 7 = "HE below deg", 8 = "Bach", 9 = "Higher deg"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Role modelling ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

tasks <- list(
  nappies = c("M02M_CHANGB", "M02P_CHANGB"),
  tuckin = c("M02M_COUCHB", "M02P_COUCHB"),
  bath = c("M02M_LAVB", "M02P_LAVB"),
  walk = c("M02M_PROMB", "M02P_PROMB"),
  night = c("M02M_NUITPLEU", "M02P_NUITPLEU"),
  doctor = c("M02M_MEDB", "M02P_MEDB"),
  dishes = c("M02M_VAISS", "M02P_VAISS"),
  groceries = c("M02M_COURSES", "M02P_COURSES"),
  cook = c("M02M_REPAS", "M02P_REPAS"),
  laundry = c("M02M_LINGE", "M02P_LINGE"),
  clean = c("M02M_MENAGE", "M02P_MENAGE"),
  diy = c("M02M_REPAR", "M02P_REPAR")
)

for (task in names(tasks)) {
  m_col <- tasks[[task]][1]
  p_col <- tasks[[task]][2]
  
  elfe[, (task) := get(m_col)]
  elfe[is.na(get(task)) & !(get(p_col) %in% c(6, 7)), (task) := 6 - get(p_col)]
  elfe[is.na(get(task)) | get(task) %in% c(6, 7), (task) := NA]
  
  elfe[get(task) %in% c(1, 2), paste0((task), 2)  := 1]
  elfe[get(task) == 3, paste0((task), 2)  := 2]
  elfe[get(task) %in% c(4, 5), paste0((task), 2)  := 3]
}

chores_elfe <- c("nappies", "tuckin", "bath", "walk", "night", "doctor", "dishes", "groceries", "cook", "laundry", "clean", "diy", 
                  # 1 = "Always the mother", 2 = "Most often the mother", 3 = "Both", 4 = "Most often the father", 5 = "Always the father"
                 "nappies3", "tuckin3", "bath3", "walk3", "night3", "doctor3", "dishes3", "groceries3", "cook3", "laundry3", "clean3", "diy3")
                  # 1 = "Mother mostly", 2 = "Balanced, 3 = "Father mostly"

# MCS 

tasks <- c(
  nappies3 = "APCHNA00_m",
  night3 = "APGEUP00_m",
  cook3 = "APCOOK00_m",
  clean3 = "APCLEA00_m",
  laundry3 = "APLAUN00_m",
  diy3 = "APHDIY00_m",
  budgetting3 = "APCASH00_m",
  doctor3 = "APLKIL00_m",
  lookafter3 = "APGECA00_m"
)

for (new_var in names(tasks)) {
  source_var <- tasks[[new_var]]
  mcs[get(source_var) %in% c(1, 2, 3), (new_var) := get(source_var)]
}

chores_mcs <- c("nappies3", "night3", "cook3", "clean3", "laundry3", "diy3", "budgetting3", "doctor3", "lookafter3")
                # 1 = "Mother mostly", 2 = "Balanced, 3 = "Father mostly"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attitudes expectations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

values <- 1:10
names <- c("socialsuccess", "lovelife", "interestingjob", "passion", "calmlife", "bigfamily", "lotsoffriends", "fairerworld", "goodhealth", "otherwish")

for (i in seq_along(values)) {
  val <- values[i]
  nm <- names[i]
  elfe[, (nm) := ifelse(M02M_SHBB1 == val | M02M_SHBB2 == val | M02M_SHBB3 == val, 1, 0)]
}

for (v in names) print(table(elfe[[v]], useNA = "ifany"))

# social <- c(1, 2, 6, 7)
# accomplish <- c(3, 4)
# other <- c(5, 8, 9, 10)

expectations_elfe <- c("socialsuccess", "lovelife", "interestingjob", "passion", "calmlife", "bigfamily", "lotsoffriends", "fairerworld", "goodhealth", "otherwish")
                        # Wish for the child, cited? 1 = "Yes", 0 = "No

# MCS

# Values to instill

mcs[, independence := ifelse(BPINDE00_m == 1, 1, 0)]
mcs[, obedience := ifelse(BPOBRE00_m == 1, 1, 0)]
mcs[, negotiation := ifelse(BPNEGO00_m == 1, 1, 0)]
mcs[, respectelders := ifelse(BPRSPE00_m == 1, 1, 0)]
mcs[, dowellatschool := ifelse(BPWESC00_m == 1, 1, 0)]
mcs[, instillreligiousvalues := ifelse(BPREVA00_m == 1, 1, 0)]

# Most important qualities

values <- 1:7
names <- c("bewellliked", "thinkforself", "workhard", "helpothers", "obeyparents", "qualityreligiousvalues")

for (i in seq_along(values)) {
  val <- values[i]
  nm <- names[i]
  mcs[, (nm) := ifelse(BPQUAL00_m == val | BPSEQU00_m == val | BPTHQU00_m == val, 1, 0)]
}

for (v in names) print(table(mcs[[v]], useNA = "ifany"))

expectations_mcs <- c("independence", "obedience", "negotiation", "respectelders", "dowellatschool", "instillreligiousvalues", 
                      # Values to instill, cited? 1 = "Yes", 0 = "No
                      "bewellliked", "thinkforself", "workhard", "helpothers", "obeyparents", "qualityreligiousvalues")
                      # Important qualities, cited? 1 = "Yes", 0 = "No

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Children's access to resources ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe[, paint3y:= A03R_ACT1]
elfe[, read3y:= A03R_ACT2]
elfe[, music3y:= A03R_ACT3]
elfe[, readplus3y:= A03R_ACT4]
elfe[, counting3y:= A03R_ACT5]
elfe[, writing3y:= A03R_ACT6]
elfe[, puzzle3y:= A03R_ACT7]
















# MCS

# age 3
vars <- c("physical3y", "library3y", "counting3y", "songs3y", "read3y", "familymeal3y", "alphabet3y", "paint3y")
m_vars <- c("BPSDPA00_m", "BPTOLI00_m", "BPNUMB00_m", "BPSONG00_m", "BPREEL00_m", "BPYOCH00_m", "BPEATW00_m", "BPALPH00_m", "BPDRAW00_m")
p_vars <- c("BPSDPA00_p", "BPTOLI00_p", "BPNUMB00_p", "BPSONG00_p", "BPREEL00_p", "BPYOCH00_p", "BPEATW00_p", "BPALPH00_p", "BPDRAW00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var) := ifelse(get(m_var) == 1, 1, 0)]
  mcs[is.na(get(new_var)), (new_var) := ifelse(get(p_var) == 1, 1, 0)]
}

vars <- c("fqcounting3y", "fqlibrary3y", "fqplay3y", "fqread3y", "fqpaint3y", "fqalphabet3y", "fqsongs3y")
m_vars <- c("BPOFCO00_m", "BPOFLI00_m", "BPPLAY00_m", "BPOFRE00_m", "BPPAMA00_m", "BPOFAB00_m", "BPOFSO00_m")
p_vars <- c("BPOFCO00_p", "BPOFLI00_p", "BPPLAY00_p", "BPOFRE00_p", "BPPAMA00_p", "BPOFAB00_p", "BPOFSO00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var) := ifelse(get(m_var) > 0, get(m_var), NA)]
  mcs[is.na(get(new_var)), (new_var) := ifelse(get(p_var) > 0, get(p_var), NA)]
}


# age 5

vars <- c("fqread5y", "fqstories5y", "fqmusic5y", "fqdraw5y", "fqphysical5y", "fqindoor5y", "fqpark5y")
m_vars <- c("CPREOF00_m", "CPSITS00_m", "CPPLMU00_m", "CPPAMA00_m", "CPACTI00_m", "CPGAME00_m", "CPWALK00_m")
p_vars <- c("CPREOF00_p", "CPSITS00_p", "CPPLMU00_p", "CPPAMA00_p", "CPACTI00_p", "CPGAME00_p", "CPWALK00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var) := ifelse(get(m_var) > 0, get(m_var), NA)]
  mcs[is.na(get(new_var)), (new_var) := ifelse(get(p_var) > 0, get(p_var), NA)]
}

act_mcs <- c("physical3y", "library3y", "counting3y", "songs3y", "read3y", "familymeal3y", "alphabet3y", "paint3y",
             # anyone does this with child? 1 = "Yes", 0 = "No",
             
             "fqcounting3y", "fqlibrary3y", "fqplay3y", "fqread3y", "fqpaint3y", "fqalphabet3y", "fqsongs3y",
             # how often? 1 = "Occasionally or less than once a week", 2 = "1 - 2 days per week", 3 = "3 times a week", 4 = "4 times a week", 5 = "5 times a week", 6 = "6 times a week", 7 = "7 times a week constantly"

             "fqread5y", "fqstories5y", "fqmusic5y", "fqdraw5y", "fqphysical5y", "fqindoor5y", "fqpark5y")
              # how often do you these with child? 1 = "Every day", 2 = "Several times a week", 3 = "Once or twice a week", 4 = "Once or twice a month", 5 = "Less often", 6 = "Not at all"








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe <- as.data.frame(elfe)
elfepot <- elfe
save(elfepot, file = "C:/Users/sheridan_ale/Desktop/Sogenre/R/Data/Elfe/elfepot.Rdata")
rm("elfe")

mcs <- as.data.frame(mcs)
mcspot <- mcs
save(mcspot, file = "C:/Users/sheridan_ale/Desktop/Sogenre/R/Data/MCS/mcspot.Rdata")
rm("mcs", "mcs_orig")

gc()


