
for(
  pkg in c(
    "readr", # read csv
    "haven", # read dta
    "readxl", # read xlsx
    "dplyr", # basics
    "beepr", # helps save time
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

beep()

# Functions ####
source("scripts/0_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weights ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# elfe[, wgt2m:= M02E_PONDREF]
elfe[, wgt1y:= A01E_PONDREF]
elfe[, wgt2y:= A02E_PONDREF]
elfe[, wgt3y:= A03E_PONDREF]
elfe[, wgt5y:= A05E_PONDREF]

# MCS

mcs[, wgt9m:= AOVWT2]
mcs[, wgt3y:= BOVWT2]
mcs[, wgt5y:= COVWT2]

mcs[, inwave9m:= ifelse(AHCPRS00 == 1, 1, 0)]
mcs[, inwave3y:= ifelse(BHCPRS00 == 1, 1, 0)]
mcs[, inwave5y:= ifelse(CHCPRS00 == 1, 1, 0)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c("wgt1y", "wgt2y", "wgt3y", "wgt5y"
                    # , "wgt2m"
                    )

mcs_variables <- c("wgt9m", "wgt3y", "wgt5y",
                   "inwave9m", "inwave3y", "inwave5y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe[, sex:= SEXE_ENF]

# MCS

mcs[!is.na(AHCSEX00), sex:= ifelse(AHCSEX00 == 1, 1, 2)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "sex")  # 1 = "Male", 2 = "Female"

mcs_variables <- c(mcs_variables, 
                   
                   "sex")  # 1 = "Male", 2 = "Female"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Family structure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe[!is.na(Child_hhld_2m), famstr2m:=	ifelse(Child_hhld_2m %in% 1:3, Child_hhld_2m, 4)]
elfe[!is.na(child_hhld_1y), famstr1y:=	ifelse(child_hhld_1y %in% 1:3, child_hhld_1y, 4)]
elfe[!is.na(child_hhld_2y), famstr2y:=	ifelse(child_hhld_2y %in% 1:3, child_hhld_2y, 4)]
elfe[!is.na(child_hhld_3y), famstr3y:=	ifelse(child_hhld_3y %in% 1:3, child_hhld_3y, 4)]
elfe[!is.na(child_hhld_5y), famstr5y:=	ifelse(child_hhld_5y %in% 1:3, child_hhld_5y, 4)]

elfe[, twopar2m:=	ifelse(Child_hhld_2m == 1, 1, 0)]
elfe[, twopar1y:=	ifelse(child_hhld_1y == 1, 1, 0)]
elfe[, twopar2y:=	ifelse(child_hhld_2y == 1, 1, 0)]
elfe[, twopar3y:=	ifelse(child_hhld_3y == 1, 1, 0)]
elfe[, twopar5y:=	ifelse(child_hhld_5y == 1, 1, 0)]

# MCS

mcs[ADHTYP00 == 1, famstr9m:= 1]
mcs[ADHTYP00 %in% c(2, 3, 4, 10), famstr9m:= 2]
mcs[ADHTYP00 %in% c(5, 11), famstr9m:= 3]
mcs[!is.na(ADHTYP00) & !(famstr9m %in% c(1, 2, 3)), famstr9m:= 4]

mcs[BDHTYP00 == 1, famstr3y:= 1]
mcs[BDHTYP00 %in% c(2, 3, 4, 15), famstr3y:= 2]
mcs[BDHTYP00 %in% c(5, 6, 7, 16), famstr3y:= 3]
mcs[!is.na(BDHTYP00) & !(famstr3y %in% c(1, 2, 3)), famstr3y:= 4]

mcs[CDHTYP00 == 1, famstr5y:= 1]
mcs[CDHTYP00 %in% c(2, 3, 4, 15), famstr5y:= 2]
mcs[CDHTYP00 %in% c(5, 6, 7, 16), famstr5y:= 3]
mcs[!is.na(CDHTYP00) & !(famstr5y %in% c(1, 2, 3)), famstr5y:= 4]

mcs[, twopar9m:= ifelse(famstr9m == 1, 1, 0)]
mcs[, twopar3y:= ifelse(famstr3y == 1, 1, 0)]
mcs[, twopar5y:= ifelse(famstr5y == 1, 1, 0)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "famstr2m", "famstr1y", "famstr2y", "famstr3y", "famstr5y",
                    # 1 = "Two natural parents", 2 = "Mother only", 3 = "Father only", 4 = "Other"
                    
                    "twopar2m", "twopar1y", "twopar2y", "twopar3y", "twopar5y"
                    # 1 = "Two natural parents", 0 = "No"
                    )

mcs_variables <- c(mcs_variables, 
                   
                   "famstr9m", "famstr3y", "famstr5y",
                   # 1 = "Two natural parents", 2 = "Mother only", 3 = "Father only", 4 = "Other"
                   
                   "twopar9m", "twopar3y", "twopar5y"
                   # 1 = "Two natural parents", 0 = "No"
                   )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Education ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# Use info from the earlier wave possible: if info missing at 2m, take available info at 1y, etc.
elfe[, meduc:= fcoalesce(meducaf_2m, meducaf_1y, meducaf_2y, meducaf_3y)]

# Three groups
elfe[meduc %in% c(1, 2, 3), meduc3:= 1]  # Up to and including bac
elfe[meduc == 4,            meduc3:= 2]  # Bac +2 (consider including bac if desired)
elfe[meduc %in% c(5, 6),    meduc3:= 3]  # Higher than bac +2
 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "meduc3", # 1 = "Low", 2 = "Medium", 3 = "High"
                    
                    "meduc")  # 1 = "<=bepc",  2 = "cap-bep", 3 = "bac", 4 = "bac+2", 5 = "bac+3/4", 6 = ">bac+4"

mcs_variables <- c(mcs_variables,
                   
                   "meduc3", # 1 = "Low", 2 = "Medium", 3 = "High"
                   
                   "meduc")  # 1 = "None", 2 = "Other", 3 = "GCSE D-G", 4 = "GCSE A-C", 5 = "Trade", 6 = "A-level", 7 = "HE below deg", 8 = "Bach", 9 = "Higher deg"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Role modelling ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# Who does what?

tasks <- list(
  nappies2m = c("M02M_CHANGB", "M02P_CHANGB"),
  tuckin2m = c("M02M_COUCHB", "M02P_COUCHB"),
  bath2m = c("M02M_LAVB", "M02P_LAVB"),
  walk2m = c("M02M_PROMB", "M02P_PROMB"),
  night2m = c("M02M_NUITPLEU", "M02P_NUITPLEU"),
  doctor2m = c("M02M_MEDB", "M02P_MEDB"),
  dishes2m = c("M02M_VAISS", "M02P_VAISS"),
  groceries2m = c("M02M_COURSES", "M02P_COURSES"),
  cook2m = c("M02M_REPAS", "M02P_REPAS"),
  laundry2m = c("M02M_LINGE", "M02P_LINGE"),
  clean2m = c("M02M_MENAGE", "M02P_MENAGE"),
  diy2m = c("M02M_REPAR", "M02P_REPAR")
)

for (task in names(tasks)) {
  m_col <- tasks[[task]][1]
  p_col <- tasks[[task]][2]
  
  elfe[, (task):= get(m_col)]
  elfe[is.na(get(task)) & !(get(p_col) %in% c(6, 7)), (task):= 6 - get(p_col)]
  elfe[is.na(get(task)) | get(task) %in% c(6, 7), (task):= NA]
  
  elfe[get(task) %in% c(1, 2), paste0((task), 3) := 1]
  elfe[get(task) == 3, paste0((task), 3) := 2]
  elfe[get(task) %in% c(4, 5), paste0((task), 3) := 3]
}

# Maternity leave

# elfe[, matleave2m:= ifelse(M02M_CONGMAT == 1 | M02M_CONGMATPAR %in% c(1, 2), 1, 0)]
# elfe[, patleave2m:= ifelse(M02M_CONGPAT %in% c(1, 2), 1, 0)]

# Who's working? Mother, father, both, neither?
time_points <- c("2m", "1y", "2y", "3y", "5y")

for (tp in time_points) {
  mother_var <- paste0("mother_occup_status_", tp)
  father_var <- paste0("father_occup_status_", tp)
  emp_var    <- paste0("emp", tp)
  
  elfe[get(mother_var) %in% c(1, 4) & get(father_var) %in% c(1, 4), (emp_var):= 1]
  elfe[get(mother_var) %in% c(2, 3) & get(father_var) %in% c(1, 4), (emp_var):= 2]
  elfe[get(mother_var) %in% c(1, 4) & get(father_var) %in% c(2, 3), (emp_var):= 3]
  elfe[get(mother_var) %in% c(2, 3) & get(father_var) %in% c(2, 3), (emp_var):= 4]
}

# Part-time?

# 2m mother

elfe[!is.na(M02M_LIENTYP_3), mpartemp2m:= ifelse(M02M_LIENTYP_3 == 2 & M02M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:12) {
  lientyp <- paste0("M02M_LIENTYP_", i)
  empltx  <- paste0("M02M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp2m == 0,
       mpartemp2m:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:12) {
  lientyp <- paste0("M02P_LIENTYP_", i)
  empltx  <- paste0("M02P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp2m == 0,
       mpartemp2m:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

# 2m father

elfe[!is.na(M02M_LIENTYP_3), fpartemp2m:= ifelse(M02M_LIENTYP_3 == 1 & M02M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:12) {
  lientyp <- paste0("M02M_LIENTYP_", i)
  empltx  <- paste0("M02M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp2m == 0,
       fpartemp2m:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:12) {
  lientyp <- paste0("M02P_LIENTYP_", i)
  empltx  <- paste0("M02P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp2m == 0,
       fpartemp2m:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}


# 1y mother

elfe[!is.na(A01M_LIENTYP_3), mpartemp1y:= ifelse(A01M_LIENTYP_3 == 2 & A01M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:15) {
  lientyp <- paste0("A01M_LIENTYP_", i)
  empltx  <- paste0("A01M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp1y == 0,
       mpartemp1y:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:15) {
  lientyp <- paste0("A01P_LIENTYP_", i)
  empltx  <- paste0("A01P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp1y == 0,
       mpartemp1y:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

# 1y father

elfe[!is.na(A01M_LIENTYP_3), fpartemp1y:= ifelse(A01M_LIENTYP_3 == 1 & A01M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:15) {
  lientyp <- paste0("A01M_LIENTYP_", i)
  empltx  <- paste0("A01M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp1y == 0,
       fpartemp1y:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:15) {
  lientyp <- paste0("A01P_LIENTYP_", i)
  empltx  <- paste0("A01P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp1y == 0,
       fpartemp1y:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

# 2y mother

elfe[!is.na(A02M_LIENTYP_3), mpartemp2y:= ifelse(A02M_LIENTYP_3 == 2 & A02M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A02M_LIENTYP_", i)
  empltx  <- paste0("A02M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp2y == 0,
       mpartemp2y:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:20) {
  lientyp <- paste0("A02P_LIENTYP_", i)
  empltx  <- paste0("A02P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp2y == 0,
       mpartemp2y:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

# 2y father

elfe[!is.na(A02M_LIENTYP_3), fpartemp2y:= ifelse(A02M_LIENTYP_3 == 1 & A02M_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:15) {
  lientyp <- paste0("A02M_LIENTYP_", i)
  empltx  <- paste0("A02M_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp2y == 0,
       fpartemp2y:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

for (i in 3:15) {
  lientyp <- paste0("A02P_LIENTYP_", i)
  empltx  <- paste0("A02P_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp2y == 0,
       fpartemp2y:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

# 3y mother

elfe[!is.na(A03R_LIENTYP_3), mpartemp3y:= ifelse(A03R_LIENTYP_3 == 2 & A03R_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A03R_LIENTYP_", i)
  empltx  <- paste0("A03R_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & mpartemp3y == 0,
       mpartemp3y:= ifelse(get(lientyp) == 2 & get(empltx) %in% 1:100, 1, 0)]
}

# 3y father

elfe[!is.na(A03R_LIENTYP_3), fpartemp3y:= ifelse(A03R_LIENTYP_3 == 1 & A03R_EMPLTX_3 %in% 1:100, 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A03R_LIENTYP_", i)
  empltx  <- paste0("A03R_EMPLTX_", i)
  
  elfe[!is.na(get(lientyp)) & fpartemp3y == 0,
       fpartemp3y:= ifelse(get(lientyp) == 1 & get(empltx) %in% 1:100, 1, 0)]
}

elfe[, mpartemp2m:= ifelse(emp2m %in% c(1,3), mpartemp2m, NA)]
elfe[, fpartemp2m:= ifelse(emp2m %in% c(1,2), fpartemp2m, NA)]

elfe[, mpartemp1y:= ifelse(emp1y %in% c(1,3), mpartemp1y, NA)]
elfe[, fpartemp1y:= ifelse(emp1y %in% c(1,2), fpartemp1y, NA)]

elfe[, mpartemp2y:= ifelse(emp2y %in% c(1,3), mpartemp2y, NA)]
elfe[, fpartemp2y:= ifelse(emp2y %in% c(1,2), fpartemp2y, NA)]

elfe[, mpartemp3y:= ifelse(emp3y %in% c(1,3), mpartemp3y, NA)]
elfe[, fpartemp3y:= ifelse(emp3y %in% c(1,2), fpartemp3y, NA)]

# MCS 

# Who does what?

tasks <- c(
  nappies9m3 = "APCHNA00_m",
  night9m3 = "APGEUP00_m",
  cook9m3 = "APCOOK00_m",
  clean9m3 = "APCLEA00_m",
  laundry9m3 = "APLAUN00_m",
  diy9m3 = "APHDIY00_m",
  budgetting9m3 = "APCASH00_m",
  doctor9m3 = "APLKIL00_m",
  lookafter9m3 = "APGECA00_m"
)

for (new_var in names(tasks)) {
  source_var <- tasks[[new_var]]
  mcs[get(source_var) %in% c(1, 2, 3), (new_var):= get(source_var)]
}

# # Maternity/paternity leave
# 
# mcs[, matleave9m:= ifelse(APMACT00_m == 2, 1, 0)]
# 
# mcs[, patleave9m:= ifelse(APPLEA00_p == 1, 1, 0)] # many fathers took leave?? 2 thirds
# 
# mcs[, patleave3y:= ifelse(BHFEAC00_p == 2, 1, 0)]
# BPMATB00
# 
# APPLEA00_p
# 
# BHFEAC00 = 2 or 5
# BPAWWY00 = 1 2 or 3
# BPPARL00 = 1
# BPMATB00 = 1
# 
# CPAWWY00 = 1 2 or 3
# CPPARL00 = 1

# Who's working? Mother, father, both, neither?
# Mix with family structure here?

mcs[ADCWRK00 == 1, emp9m:= 1] # (1.0) Both in work 
mcs[ADCWRK00 == 2, emp9m:= 3] # (3.0) Partner in work, main not
mcs[ADCWRK00 %in% c(2, 5, 9), emp9m:= 2] # (2.0) Main in work, partner not (5.0) Main in work or on leave, no partner (9.0) Main in work, partner status unknown    
mcs[ADCWRK00 %in% c(4, 6, 10), emp9m:= 4] # (4.0) Both not in work (6.0) Main not on work nor on leave, no partner (10.0) Main not in work, partner status unknown

mcs[BDCWRK00 == 1, emp3y:= 1]
mcs[BDCWRK00 == 3, emp3y:= 2]
mcs[BDCWRK00 %in% c(2, 5, 9), emp3y:= 3]
mcs[BDCWRK00 %in% c(4, 6, 10), emp3y:= 4]

# Part-time: 1 = "Yes", 0 = "No"

mcs[ADWKST00_m %in% c(1, 2), mpartemp9m:= ifelse(APFLXW0A_m == 1 | APFLXW0B_m == 1 | APFLXW0C_m == 1 | 
                            APFLXW0D_m == 1 | APFLXW0E_m == 1 | APFLXW0F_m == 1, 1, 0)]

mcs[ADWKST00_p %in% c(1, 2), fpartemp9m:= ifelse(APFLXW0A_p == 1 | APFLXW0B_p == 1 | APFLXW0C_p == 1 | 
                            APFLXW0D_p == 1 | APFLXW0E_p == 1 | APFLXW0F_p == 1, 1, 0)]

mcs[BDWKST00_m %in% c(1, 2), mpartemp3y:= ifelse(BPFLXW0A_m == 1, 1, 0)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    # "nappies2m", "tuckin2m", "bath2m", "walk2m", "night2m", "doctor2m", "dishes2m", "groceries2m", "cook2m", "laundry2m", "clean2m", "diy2m", 
                    # Who does what? 1 = "Always the mother", 2 = "Most often the mother", 3 = "Both", 4 = "Most often the father", 5 = "Always the father"
                    
                    "nappies2m3", "tuckin2m3", "bath2m3", "walk2m3", "night2m3", "doctor2m3", "dishes2m3", "groceries2m3", "cook2m3", "laundry2m3", "clean2m3", "diy2m3",
                    # Who does what (3 categories)? 1 = "Mother mostly", 2 = "Balanced, 3 = "Father mostly"
                    
                    "emp2m", "emp1y", "emp2y", "emp3y", "emp5y",
                    # Who's working? 1 = "Both parents", 2 = "Father only", 3 = "Mother only", 4 = "Neither"
                    
                    "mpartemp2m", "fpartemp2m", "mpartemp1y", "fpartemp1y", "mpartemp2y", "fpartemp2y", "mpartemp3y", "fpartemp3y"
                    # Part-time: 1 = "Yes", 0 = "No"
                    
                    # , "mpartkids2m", "fpartkids2m", "mpartkids1y", "fpartkids1y", "mpartkids2y", "fpartkids2y", "mpartkids3y", "fpartkids3y"
                    ) # Part-time to be with kids or at home: 1 = "Yes", 0 = "No"

mcs_variables <- c(mcs_variables,
                   
                   "nappies9m3", "night9m3", "cook9m3", "clean9m3", "laundry9m3", "diy9m3", "budgetting9m3", "doctor9m3", "lookafter9m3",
                   # Who does what (3 categories)? 1 = "Mother mostly", 2 = "Balanced, 3 = "Father mostly"
                   
                   "emp9m", "emp3y",
                   # Who's working? 1 = "Both parents", 2 = "Father only", 3 = "Mother only", 4 = "Neither"
                   
                   "mpartemp9m", "fpartemp9m", "mpartemp3y"
                   # Part-time: 1 = "Yes", 0 = "No"
                   ) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attitudes expectations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

values <- 1:10
names <- c("socialsuccess2m", "lovelife2m", "interestingjob2m", "passion2m", "calmlife2m", "bigfamily2m", "lotsoffriends2m", "fairerworld2m", "goodhealth2m", "otherwish2m")

for (i in seq_along(values)) {
  val <- values[i]
  nm <- names[i]
  elfe[, (nm):= ifelse(M02M_SHBB1 == val | M02M_SHBB2 == val | M02M_SHBB3 == val, 1, 0)]
}

# social <- c(1, 2, 6, 7)
# accomplish <- c(3, 4)
# other <- c(5, 8, 9, 10)

# MCS

# Values to instill

original_vars <- c("BPINDE00_m", "BPOBRE00_m", "BPNEGO00_m", "BPRSPE00_m", "BPWESC00_m", "BPREVA00_m")
new_vars <- c("independence3y", "obedience3y", "negotiation3y", "respectelders3y", "dowellatschool3y", "instillreligiousvalues3y")

for (i in seq_along(original_vars)) {
  mcs[, (new_vars[i]):= ifelse(get(original_vars[i]) == 1, 1, 0)]
}

# Most important qualities

values <- 1:7
names <- c("bewellliked3y", "thinkforself3y", "workhard3y", "helpothers3y", "obeyparents3y", "qualityreligiousvalues3y")

for (i in seq_along(values)) {
  val <- values[i]
  nm <- names[i]
  mcs[, (nm):= ifelse(BPQUAL00_m == val | BPSEQU00_m == val | BPTHQU00_m == val, 1, 0)]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "socialsuccess2m", "lovelife2m", "interestingjob2m", "passion2m", "calmlife2m", "bigfamily2m", "lotsoffriends2m", "fairerworld2m", "goodhealth2m", "otherwish2m"
                    ) # Wish for the child, cited? 1 = "Yes", 0 = "No"

mcs_variables <- c(mcs_variables,
                   
                   "independence3y", "obedience3y", "negotiation3y", "respectelders3y", "dowellatschool3y", "instillreligiousvalues3y", 
                   # Values to instill, cited? 1 = "Yes", 0 = "No"
                   
                   "bewellliked3y", "thinkforself3y", "workhard3y", "helpothers3y", "obeyparents3y", "qualityreligiousvalues3y"
                   ) # Most important qualities, cited? 1 = "Yes", 0 = "No"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Children's access to resources ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# Parent-child activity variables

activity_vars <- paste0("A03R_ACT", 1:7)
activity_names <- c("paint3y", "read3y", "music3y", "readplus3y", "counting3y", "writing3y", "puzzle3y")

for (i in seq_along(activity_vars)) {
  elfe[, (activity_names[i]):= ifelse(get(activity_vars[i]) == 1, 1, 0)]
}

# Extra activities

extra_vars <- paste0("A03R_ACEXTRASCP", 1:8)
extra_names <- c("swimming3y", "gymnastics3y", "circus3y", "sportsinit3y", "musicclass3y", "danceclass3y", "visualarts3y", "horseriding3y")

for (i in seq_along(extra_vars)) {
  elfe[, (extra_names[i]):= ifelse(get(extra_vars[i]) == 1, 1, 0)]
}

# MCS

# age 3

vars <- c("physical3y", "library3y", "counting3y", "songs3y", "read3y", "familymeal3y", "alphabet3y", "paint3y")
m_vars <- c("BPSDPA00_m", "BPTOLI00_m", "BPNUMB00_m", "BPSONG00_m", "BPREEL00_m", "BPYOCH00_m", "BPEATW00_m", "BPALPH00_m", "BPDRAW00_m")
p_vars <- c("BPSDPA00_p", "BPTOLI00_p", "BPNUMB00_p", "BPSONG00_p", "BPREEL00_p", "BPYOCH00_p", "BPEATW00_p", "BPALPH00_p", "BPDRAW00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var):= ifelse(get(m_var) == 1, 1, 0)]
  mcs[is.na(get(new_var)), (new_var):= ifelse(get(p_var) == 1, 1, 0)]
}

vars <- c("fqcounting3y", "fqlibrary3y", "fqplay3y", "fqread3y", "fqpaint3y", "fqalphabet3y", "fqsongs3y")
m_vars <- c("BPOFCO00_m", "BPOFLI00_m", "BPPLAY00_m", "BPOFRE00_m", "BPPAMA00_m", "BPOFAB00_m", "BPOFSO00_m")
p_vars <- c("BPOFCO00_p", "BPOFLI00_p", "BPPLAY00_p", "BPOFRE00_p", "BPPAMA00_p", "BPOFAB00_p", "BPOFSO00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var):= ifelse(get(m_var) > 0, get(m_var), NA)]
  mcs[is.na(get(new_var)), (new_var):= ifelse(get(p_var) > 0, get(p_var), NA)]
}

# age 5

vars <- c("fqread5y", "fqstories5y", "fqmusic5y", "fqdraw5y", "fqphysical5y", "fqindoor5y", "fqpark5y")
m_vars <- c("CPREOF00_m", "CPSITS00_m", "CPPLMU00_m", "CPPAMA00_m", "CPACTI00_m", "CPGAME00_m", "CPWALK00_m")
p_vars <- c("CPREOF00_p", "CPSITS00_p", "CPPLMU00_p", "CPPAMA00_p", "CPACTI00_p", "CPGAME00_p", "CPWALK00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[, (new_var):= ifelse(get(m_var) > 0, get(m_var), NA)]
  mcs[is.na(get(new_var)), (new_var):= ifelse(get(p_var) > 0, get(p_var), NA)]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "paint3y", "read3y", "music3y", "readplus3y", "counting3y", "writing3y", "puzzle3y",
                    # Parent-child activities: 1 = "Yes", 0 = "No"
                    
                    "swimming3y", "gymnastics3y", "circus3y", "sportsinit3y", "musicclass3y", "danceclass3y", "visualarts3y", "horseriding3y"
                    ) # child's activities in clubs: 1 = "Yes", 0 = "No"

mcs_variables <- c(mcs_variables,
                   
                   "physical3y", "library3y", "counting3y", "songs3y", "read3y", "familymeal3y", "alphabet3y", "paint3y",
                   # anyone does this with child? 1 = "Yes", 0 = "No"
                   
                   "fqcounting3y", "fqlibrary3y", "fqplay3y", "fqread3y", "fqpaint3y", "fqalphabet3y", "fqsongs3y",
                   # how often? 1 = "Occasionally or less than once a week", 2 = "1 - 2 days per week", 3 = "3 times a week", 4 = "4 times a week", 5 = "5 times a week", 6 = "6 times a week", 7 = "7 times a week constantly"
                   
                   "fqread5y", "fqstories5y", "fqmusic5y", "fqdraw5y", "fqphysical5y", "fqindoor5y", "fqpark5y"
                   ) # how often do you these with child? 1 = "Every day", 2 = "Several times a week", 3 = "Once or twice a week", 4 = "Once or twice a month", 5 = "Less often", 6 = "Not at all"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

beep()

names(mcs)[names(mcs) == "" | is.na(names(mcs))] <- paste0("unnamed_", seq_len(sum(names(mcs) == "" | is.na(names(mcs)))))

mcs <- mcs %>%
  filter(!is.na(sex))
  
elfemini <- elfe[, ..elfe_variables]
mcsmini <- mcs[, ..mcs_variables]

elfe <- as.data.frame(elfe)
save(elfe, file = "data/analysisdata/elfe.Rdata")

mcs <- as.data.frame(mcs)
save(mcs, file = "data/analysisdata/mcs.Rdata")

elfemini <- as.data.frame(elfemini)
save(elfemini, file = "data/analysisdata/elfemini.Rdata")

mcsmini <- as.data.frame(mcsmini)
save(mcsmini, file = "data/analysisdata/mcsmini.Rdata")

gc()

beep()
