
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

elfe2 <- read_delim("data/20250708DEM_1066_LP/DATA_DEM_1066_LP.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
elfe2 <- as.data.table(elfe2)

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
                    
                    "sex")  # "Male", "Female"

mcs_variables <- c(mcs_variables, 
                   
                   "sex")  # "Male", "Female"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Family structure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

elfe[!is.na(Child_hhld_2m), famstr2m:=	ifelse(Child_hhld_2m %in% 1:3, Child_hhld_2m, 4)]
elfe[!is.na(child_hhld_1y), famstr1y:=	ifelse(child_hhld_1y %in% 1:3, child_hhld_1y, 4)]
elfe[!is.na(child_hhld_2y), famstr2y:=	ifelse(child_hhld_2y %in% 1:3, child_hhld_2y, 4)]
elfe[!is.na(child_hhld_3y), famstr3y:=	ifelse(child_hhld_3y %in% 1:3, child_hhld_3y, 4)]
elfe[!is.na(child_hhld_5y), famstr5y:=	ifelse(child_hhld_5y %in% 1:3, child_hhld_5y, 4)]

elfe[!is.na(Child_hhld_2m), twopar2m:=	ifelse(Child_hhld_2m == 1, 1, 0)]
elfe[!is.na(child_hhld_1y), twopar1y:=	ifelse(child_hhld_1y == 1, 1, 0)]
elfe[!is.na(child_hhld_2y), twopar2y:=	ifelse(child_hhld_2y == 1, 1, 0)]
elfe[!is.na(child_hhld_3y), twopar3y:=	ifelse(child_hhld_3y == 1, 1, 0)]
elfe[!is.na(child_hhld_5y), twopar5y:=	ifelse(child_hhld_5y == 1, 1, 0)]

elfe[!is.na(twopar2m) & !is.na(twopar1y) & !is.na(twopar2y) & !is.na(twopar3y), 
     twopar:= ifelse(twopar2m == 1 & twopar1y == 1 & twopar2y == 1 & twopar3y == 1, 1, 0)]

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

mcs[!is.na(famstr9m), twopar9m:= ifelse(famstr9m == 1, 1, 0)]
mcs[!is.na(famstr3y), twopar3y:= ifelse(famstr3y == 1, 1, 0)]
mcs[!is.na(famstr5y), twopar5y:= ifelse(famstr5y == 1, 1, 0)]

mcs[!is.na(twopar9m) & !is.na(twopar3y), twopar:= ifelse(twopar9m == 1 & twopar3y, 1, 0)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "famstr2m", "famstr1y", "famstr2y", "famstr3y", "famstr5y",
                    # "Two natural parents", "Mother only", "Father only", "Other"
                    
                    "twopar2m", "twopar1y", "twopar2y", "twopar3y", "twopar5y",
                    # "Two natural parents", "No"
                    
                    "twopar"
                    ) # family structure different from 2 bio parents at some point before 5

mcs_variables <- c(mcs_variables, 
                   
                   "famstr9m", "famstr3y", "famstr5y",
                   # "Two natural parents", "Mother only", "Father only", "Other"
                   
                   "twopar9m", "twopar3y", "twopar5y",
                   # "Two natural parents", "No"
                   
                   "twopar"
                   ) # family structure different from 2 bio parents at some point before 5


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
                    
                    "meduc3", # "Low", "Medium", "High"
                    
                    "meduc")  # "<=bepc",  "cap-bep", "bac", "bac+2", "bac+3/4", ">bac+4"

mcs_variables <- c(mcs_variables,
                   
                   "meduc3", # "Low", "Medium", "High"
                   
                   "meduc")  # "None", "Other", "GCSE D-G", "GCSE A-C", "Trade", "A-level", "HE below deg", "Bach", "Higher deg"

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

# 2m mother

elfe2[!is.na(M02M_LIENTYP_3), matleave2m:= ifelse(M02M_LIENTYP_3 == 2 & M02M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:12) {
  lientyp <- paste0("M02M_LIENTYP_", i)
  cong  <- paste0("M02M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & matleave2m == 0,
        matleave2m:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:12) {
  lientyp <- paste0("M02P_LIENTYP_", i)
  cong  <- paste0("M02P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (matleave2m == 0 | is.na(matleave2m)),
        matleave2m:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

elfe2[matleave2m == 0 & M02M_CONGMAT == 1, matleave2m:= 1]
elfe2[matleave2m == 0 & M02M_ACTIV %in% c(4, 5), matleave2m:= 1]

# 2m father

elfe2[!is.na(M02M_LIENTYP_3), patleave2m:= ifelse(M02M_LIENTYP_3 == 1 & M02M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:12) {
  lientyp <- paste0("M02M_LIENTYP_", i)
  cong  <- paste0("M02M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & patleave2m == 0,
        patleave2m:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:12) {
  lientyp <- paste0("M02P_LIENTYP_", i)
  cong  <- paste0("M02P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (patleave2m == 0 | is.na(patleave2m)),
        patleave2m:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

elfe2[patleave2m == 0 & M02M_CONGPAT %in% c(1, 2), patleave2m:= 1]

# 1y mother

elfe2[!is.na(A01M_LIENTYP_3), matleave1y:= ifelse(A01M_LIENTYP_3 == 2 & A01M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:15) {
  lientyp <- paste0("A01M_LIENTYP_", i)
  cong  <- paste0("A01M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & matleave1y == 0,
        matleave1y:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:15) {
  lientyp <- paste0("A01P_LIENTYP_", i)
  cong  <- paste0("A01P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (matleave1y == 0 | is.na(matleave1y)),
        matleave1y:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

# 1y father

elfe2[!is.na(A01M_LIENTYP_3), patleave1y:= ifelse(A01M_LIENTYP_3 == 1 & A01M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:15) {
  lientyp <- paste0("A01M_LIENTYP_", i)
  cong  <- paste0("A01M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & patleave1y == 0,
        patleave1y:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:15) {
  lientyp <- paste0("A01P_LIENTYP_", i)
  cong  <- paste0("A01P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (patleave1y == 0 | is.na(patleave1y)),
        patleave1y:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

# 2y mother

elfe2[!is.na(A02M_LIENTYP_3), matleave2y:= ifelse(A02M_LIENTYP_3 == 2 & A02M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A02M_LIENTYP_", i)
  cong  <- paste0("A02M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & matleave2y == 0,
        matleave2y:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:20) {
  lientyp <- paste0("A02P_LIENTYP_", i)
  cong  <- paste0("A02P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (matleave2y == 0 | is.na(matleave2y)),
        matleave2y:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

# 2y father

elfe2[!is.na(A02M_LIENTYP_3), patleave2y:= ifelse(A02M_LIENTYP_3 == 1 & A02M_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A02M_LIENTYP_", i)
  cong  <- paste0("A02M_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & patleave2y == 0,
        patleave2y:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

for (i in 3:20) {
  lientyp <- paste0("A02P_LIENTYP_", i)
  cong  <- paste0("A02P_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & (patleave2y == 0 | is.na(patleave2y)),
        patleave2y:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

# 3y mother

elfe2[!is.na(A03R_LIENTYP_3), matleave3y:= ifelse(A03R_LIENTYP_3 == 2 & A03R_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A03R_LIENTYP_", i)
  cong  <- paste0("A03R_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & matleave3y == 0,
        matleave3y:= ifelse(get(lientyp) == 2 & get(cong) %in% c(1, 2), 1, 0)]
}

# 3y father

elfe2[!is.na(A03R_LIENTYP_3), patleave3y:= ifelse(A03R_LIENTYP_3 == 1 & A03R_CONGMATPAR_3 %in% c(1, 2), 1, 0)]

for (i in 4:20) {
  lientyp <- paste0("A03R_LIENTYP_", i)
  cong  <- paste0("A03R_CONGMATPAR_", i)
  
  elfe2[!is.na(get(lientyp)) & patleave3y == 0,
        patleave3y:= ifelse(get(lientyp) == 1 & get(cong) %in% c(1, 2), 1, 0)]
}

# overall mothers

elfe2[!is.na(matleave2m) & !is.na(matleave1y) & !is.na(matleave2y) & !is.na(matleave3y), matleave:= ifelse(matleave2m == 1 | matleave1y == 1
                            | matleave2y == 1 | matleave3y == 1, 1, 0)]

# overall fathers

elfe2[!is.na(patleave2m) & !is.na(patleave1y) & !is.na(patleave2y) & !is.na(patleave3y), patleave:= ifelse(patleave2m == 1 | patleave1y == 1
                            | patleave2y == 1 | patleave3y == 1, 1, 0)]

# who took it

elfe2[matleave2m == 1 & patleave2m == 1, leave2m:= 1]
elfe2[matleave2m == 0 & patleave2m == 1, leave2m:= 2]
elfe2[matleave2m == 1 & patleave2m == 0, leave2m:= 3]
elfe2[matleave2m == 0 & patleave2m == 0, leave2m:= 4]

elfe2[matleave1y == 1 & patleave1y == 1, leave1y:= 1]
elfe2[matleave1y == 0 & patleave1y == 1, leave1y:= 2]
elfe2[matleave1y == 1 & patleave1y == 0, leave1y:= 3]
elfe2[matleave1y == 0 & patleave1y == 0, leave1y:= 4]

elfe2[matleave2y == 1 & patleave2y == 1, leave2y:= 1]
elfe2[matleave2y == 0 & patleave2y == 1, leave2y:= 2]
elfe2[matleave2y == 1 & patleave2y == 0, leave2y:= 3]
elfe2[matleave2y == 0 & patleave2y == 0, leave2y:= 4]

elfe2[matleave3y == 1 & patleave3y == 1, leave3y:= 1]
elfe2[matleave3y == 0 & patleave3y == 1, leave3y:= 2]
elfe2[matleave3y == 1 & patleave3y == 0, leave3y:= 3]
elfe2[matleave3y == 0 & patleave3y == 0, leave3y:= 4]


elfe2[matleave == 1 & patleave == 1, leave:= 1]
elfe2[matleave == 0 & patleave == 1, leave:= 2]
elfe2[matleave == 1 & patleave == 0, leave:= 3]
elfe2[matleave == 0 & patleave == 0, leave:= 4]

elfe2 <- elfe2 %>% 
  select("matleave2m", "matleave1y", "matleave2y", "matleave3y", 
         
         "matleave",
         
         "patleave2m", "patleave1y", "patleave2y", "patleave3y", 
         
         "patleave",
         
         "leave2m", "leave1y", "leave2y", "leave3y", 
         
         "leave",
         
         "id_DEM_1066_LP")

# merge these variables with main dataframe
elfe <- full_join(elfe, elfe2, by = "id_DEM_1066_LP")

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

# overall mothers

elfe[!is.na(emp2m) & !is.na(emp1y) & !is.na(emp2y) & !is.na(emp3y), motheremp:= ifelse(emp2m %in% c(1, 3) | emp1y %in% c(1, 3)
                            | emp2y %in% c(1, 3) | emp3y %in% c(1, 3), 1, 0)]

# overall fathers

elfe[!is.na(emp2m) & !is.na(emp1y) & !is.na(emp2y) & !is.na(emp3y), fatheremp:= ifelse(emp2m %in% c(1, 2) | emp1y %in% c(1, 2)
                            | emp2y %in% c(1, 2) | emp3y %in% c(1, 2), 1, 0)]

# who works

elfe[motheremp == 1 & fatheremp == 1, emp:= 1]
elfe[motheremp == 0 & fatheremp == 1, emp:= 2]
elfe[motheremp == 1 & fatheremp == 0, emp:= 3]
elfe[motheremp == 0 & fatheremp == 0, emp:= 4]

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

# overall mothers

elfe[!is.na(mpartemp2m) & !is.na(mpartemp1y) & !is.na(mpartemp2y) & !is.na(mpartemp3y), mpartemp:= ifelse(mpartemp2m == 1 | mpartemp1y == 1
                            | mpartemp2y == 1 | mpartemp3y == 0, 1, 0)]

# overall fathers

elfe[!is.na(fpartemp2m) & !is.na(fpartemp1y) & !is.na(fpartemp2y) & !is.na(fpartemp3y), fpartemp:= ifelse(fpartemp2m == 1 | fpartemp1y == 1
                            | fpartemp2y == 1 | fpartemp3y == 1, 1, 0)]

# who took it

elfe[mpartemp == 1 & fpartemp == 1, partemp:= 1]
elfe[mpartemp == 0 & fpartemp == 1, partemp:= 2]
elfe[mpartemp == 1 & fpartemp == 0, partemp:= 3]
elfe[mpartemp == 0 & fpartemp == 0, partemp:= 4]

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
  mcs[get(source_var) %in% c(1, 2, 3), (new_var) := fifelse(
    get(source_var) == 1, 1,
    fifelse(get(source_var) == 2, 3,
            fifelse(get(source_var) == 3, 2, NA_integer_)))
  ]
}

# Maternity/paternity leave

# 9 months 

mcs[!is.na(APMACT00_m), matleave9m:= ifelse(APMACT00_m == 2, 1, 0)]
mcs[!is.na(APFILE00_m) & matleave9m != 1, matleave9m:= ifelse(APFILE00_m == 2, 1, 0)]
mcs[matleave9m != 1, matleave9m:= ifelse(APWHRM0A_m == 1 | APWHRM0B_m == 1 | APWHRM0C_m == 1 | APWHRM0D_m == 1
                                         | APWHRM0E_m == 1 | APWHRM0F_m == 1, 1, 0)]
mcs[matleave9m != 1, matleave9m:= ifelse(APFLXB0A_m %in% c(8, 9) | APFLXB0B_m %in% c(8, 9) | APFLXB0C_m %in% c(8, 9) | 
                                           APFLXB0D_m %in% c(8, 9) | APFLXB0E_m %in% c(8, 9) | APFLXB0F_m %in% c(8, 9), 1, 0)]

mcs[!is.na(APLEWT0A_p), patleave9m:= 
      ifelse(APLEWT0A_p %in% c(1, 2) | APLEWT0B_p %in% c(1, 2) | APLEWT0C_p %in% c(1, 2) | APLEWT0D_p %in% c(1, 2), 1, 0)]
mcs[patleave9m != 1, patleave9m:= ifelse(APFLXB0A_p %in% c(8, 9) | APFLXB0B_p %in% c(8, 9) | APFLXB0C_p %in% c(8, 9) | 
                                           APFLXB0D_p %in% c(8, 9) | APFLXB0E_p %in% c(8, 9) | APFLXB0F_p %in% c(8, 9), 1, 0)]

# 3 years old

mcs[!is.na(BPAWWY00_m), matleave3y:= ifelse(BPAWWY00_m %in% c(1, 2), 1, 0)]
mcs[matleave3y != 1, matleave3y:= ifelse(BPFLXB0G_m == 1 | BPFLXB0H_m == 1 | BPFLXB0I_m == 1, 1, 0)] # why do I have 0s here when I have 1s above?


mcs[!is.na(BPAWWY00_p), patleave3y:= ifelse(BPAWWY00_p %in% c(1, 2), 1, 0)]
mcs[patleave3y != 1, patleave3y:= ifelse(BHFEAC00_p == 2, 1, 0)]
# mcs[patleave3y != 1, patleave3y:= ifelse(BPFLXB0G_p == 1 | BPFLXB0H_p == 1 | BPFLXB0I_p == 1, 1, 0)] # only -1s

# overall mothers

mcs[!is.na(matleave9m) & !is.na(matleave3y), matleave:= ifelse(matleave9m == 1 | matleave3y == 1, 1, 0)]

# overall fathers

mcs[!is.na(patleave9m) & !is.na(patleave3y), patleave:= ifelse(patleave9m == 1 | patleave3y == 1, 1, 0)]

# who took it

mcs[matleave9m == 1 & patleave9m == 1, leave9m:= 1]
mcs[matleave9m == 0 & patleave9m == 1, leave9m:= 2]
mcs[matleave9m == 1 & patleave9m == 0, leave9m:= 3]
mcs[matleave9m == 0 & patleave9m == 0, leave9m:= 4]

mcs[matleave3y == 1 & patleave3y == 1, leave3y:= 1]
mcs[matleave3y == 0 & patleave3y == 1, leave3y:= 2]
mcs[matleave3y == 1 & patleave3y == 0, leave3y:= 3]
mcs[matleave3y == 0 & patleave3y == 0, leave3y:= 4]

mcs[matleave == 1 & patleave == 1, leave:= 1]
mcs[matleave == 0 & patleave == 1, leave:= 2]
mcs[matleave == 1 & patleave == 0, leave:= 3]
mcs[matleave == 0 & patleave == 0, leave:= 4]


# Who's working? Mother, father, both, neither?
# Mix with family structure here?

mcs[ADCWRK00 == 1, emp9m:= 1] # (1.0) Both in work 
mcs[ADCWRK00 == 3, emp9m:= 2] # (3.0) Partner in work, main not
mcs[ADCWRK00 %in% c(2, 5, 9), emp9m:= 3] # (2.0) Main in work, partner not (5.0) Main in work or on leave, no partner (9.0) Main in work, partner status unknown    
mcs[ADCWRK00 %in% c(4, 6, 10), emp9m:= 4] # (4.0) Both not in work (6.0) Main not on work nor on leave, no partner (10.0) Main not in work, partner status unknown

mcs[BDCWRK00 == 1, emp3y:= 1]
mcs[BDCWRK00 == 3, emp3y:= 2]
mcs[BDCWRK00 %in% c(2, 5, 9), emp3y:= 3]
mcs[BDCWRK00 %in% c(4, 6, 10), emp3y:= 4]

# overall mothers

mcs[!is.na(emp9m) & !is.na(emp3y), motheremp:= ifelse(emp9m %in% c(1, 3) | emp3y %in% c(1, 3), 1, 0)]

# overall fathers

mcs[!is.na(emp9m) & !is.na(emp3y), fatheremp:= ifelse(emp9m %in% c(1, 2) | emp3y %in% c(1, 2), 1, 0)]

# who works

mcs[motheremp == 1 & fatheremp == 1, emp:= 1]
mcs[motheremp == 0 & fatheremp == 1, emp:= 2]
mcs[motheremp == 1 & fatheremp == 0, emp:= 3]
mcs[motheremp == 0 & fatheremp == 0, emp:= 4]

# Part-time

mcs[ADWKST00_m %in% c(1, 2), mpartemp9m:= ifelse(APFLXW0A_m == 1 | APFLXW0B_m == 1 | APFLXW0C_m == 1 | 
                            APFLXW0D_m == 1 | APFLXW0E_m == 1 | APFLXW0F_m == 1, 1, 0)]

mcs[ADWKST00_p %in% c(1, 2), fpartemp9m:= ifelse(APFLXW0A_p == 1 | APFLXW0B_p == 1 | APFLXW0C_p == 1 | 
                            APFLXW0D_p == 1 | APFLXW0E_p == 1 | APFLXW0F_p == 1, 1, 0)]

mcs[BDWKST00_m %in% c(1, 2), mpartemp3y:= ifelse(BPFLXW0A_m == 1, 1, 0)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    # "nappies2m", "tuckin2m", "bath2m", "walk2m", "night2m", "doctor2m", "dishes2m", "groceries2m", "cook2m", "laundry2m", "clean2m", "diy2m", 
                    # Who does what? "Always the mother", "Most often the mother", "Both", "Most often the father", "Always the father"
                    
                    "nappies2m3", "tuckin2m3", "bath2m3", "walk2m3", "night2m3", "doctor2m3", "dishes2m3", "groceries2m3", "cook2m3", "laundry2m3", "clean2m3", "diy2m3",
                    # Who does what (3 categories)? "Mother mostly", "Balanced, "Father mostly"
                    
                    "matleave2m", "matleave1y", "matleave2y", "matleave3y", 
                    # maternity leave? "No", "Yes"
                    
                    "matleave",
                    # maternity leave at least at one survey? "No", "Yes"
                    
                    "patleave2m", "patleave1y", "patleave2y", "patleave3y", 
                    # paternity leave? "No", "Yes"
                    
                    "patleave",
                    # paternity leave at least at one survey? "No", "Yes",
                    
                    "leave2m", "leave1y", "leave2y", "leave3y",
                    # Who's on leave? "Both parents", "Father only", "Mother only", "Neither"
                    
                    "leave",
                    # Who's on mat/paternity leave at least at one survey? "Both parents", "Father only", "Mother only", "Neither"
                    
                    "emp2m", "emp1y", "emp2y", "emp3y", "emp5y",
                    # Who's working? "Both parents", "Father only", "Mother only", "Neither"
                    
                    "motheremp", "fatheremp",
                    # working at least at one survey? "No", "Yes"
                    
                    "emp",
                    # Who's working at least at one survey? "Both parents", "Father only", "Mother only", "Neither"
                    
                    "mpartemp2m", "fpartemp2m", "mpartemp1y", "fpartemp1y", "mpartemp2y", "fpartemp2y", "mpartemp3y", "fpartemp3y",
                    # Part-time: "No", "Yes"
                    
                    "mpartemp", "fpartemp",
                    # part time working at least at one survey? "No", "Yes"
                    
                    "partemp") # Who's part time at least at one survey? "Both parents", "Father only", "Mother only", "Neither"

mcs_variables <- c(mcs_variables,
                   
                   "nappies9m3", "night9m3", "cook9m3", "clean9m3", "laundry9m3", "diy9m3", "budgetting9m3", "doctor9m3", "lookafter9m3",
                   # Who does what (3 categories)? "Mother mostly", "Balanced, "Father mostly"
                   
                   "matleave9m", "matleave3y", 
                   # maternity leave? "No", "Yes"
                   
                   "matleave",
                   # maternity leave at least at one survey? "No", "Yes"
                   
                   "patleave9m", "patleave3y", 
                   # paternity leave? "No", "Yes"
                   
                   "leave9m", "leave3y",
                   # Who's on leave? "Both parents", "Father only", "Mother only", "Neither"
                   
                   "patleave",
                   # paternity leave at least at one survey? "No", "Yes",
                   
                   "leave",
                   # Who's on mat/paternity leave at least at one survey? "Both parents", "Father only", "Mother only", "Neither"
                   
                   "emp9m", "emp3y",
                   # Who's working? "Both parents", "Father only", "Mother only", "Neither"
                   
                   "emp",
                   # Who's working at least at one survey? "Both parents", "Father only", "Mother only", "Neither"
                   
                   "mpartemp9m", "fpartemp9m", "mpartemp3y"
                   # Part-time: "No", "Yes"
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
  elfe[!is.na(M02M_SHBB1), (nm):= ifelse(M02M_SHBB1 == val | M02M_SHBB2 == val | M02M_SHBB3 == val, 1, 0)]
}

# social <- c(1, 2, 6, 7)
# accomplish <- c(3, 4)
# other <- c(5, 8, 9, 10)

# MCS

# Values to instill

original_vars <- c("BPINDE00_m", "BPOBRE00_m", "BPNEGO00_m", "BPRSPE00_m", "BPWESC00_m", "BPREVA00_m")
new_vars <- c("independence3y", "obedience3y", "negotiation3y", "respectelders3y", "dowellatschool3y", "instillreligiousvalues3y")

for (i in seq_along(original_vars)) {
  mcs[!is.na(get(original_vars[i])), (new_vars[i]):= ifelse(get(original_vars[i]) == 1, 1, 0)]
}

# Most important qualities

values <- 1:7
names <- c("bewellliked3y", "thinkforself3y", "workhard3y", "helpothers3y", "obeyparents3y", "qualityreligiousvalues3y")

for (i in seq_along(values)) {
  val <- values[i]
  nm <- names[i]
  mcs[!is.na(BPQUAL00_m), (nm):= ifelse(BPQUAL00_m == val | BPSEQU00_m == val | BPTHQU00_m == val, 1, 0)]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "socialsuccess2m", "lovelife2m", "interestingjob2m", "passion2m", "calmlife2m", "bigfamily2m", "lotsoffriends2m", "fairerworld2m", "goodhealth2m", "otherwish2m"
                    ) # Wish for the child, cited? "No", "Yes"

mcs_variables <- c(mcs_variables,
                   
                   "independence3y", "obedience3y", "negotiation3y", "respectelders3y", "dowellatschool3y", "instillreligiousvalues3y", 
                   # Values to instill, cited? "No", "Yes"
                   
                   "bewellliked3y", "thinkforself3y", "workhard3y", "helpothers3y", "obeyparents3y", "qualityreligiousvalues3y"
                   ) # Most important qualities, cited? "No", "Yes"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Children's access to resources ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Elfe

# Parent-child activity variables

activity_vars <- paste0("A03R_ACT", 1:7)
activity_names <- c("frpaint3y", "frread3y", "music3y", "readplus3y", "frcounting3y", "writing3y", "puzzle3y")

for (i in seq_along(activity_vars)) {
  elfe[!is.na(get(activity_vars[i])), (activity_names[i]):= ifelse(get(activity_vars[i]) == 1, 1, 0)]
}

# Extra activities 

elfe[, anyactivity3y:= ifelse(A03R_ACEXTRASC == 1, 1, 0)]

extra_vars <- paste0("A03R_ACEXTRASCP", 1:8) # for subsample of children who do extra activities
extra_names <- c("swimming3y", "gymnastics3y", "circus3y", "sportsinit3y", "musicclass3y", "danceclass3y", "visualarts3y", "horseriding3y")

for (i in seq_along(extra_vars)) {
  elfe[!is.na(get(extra_vars[i])), (extra_names[i]):= ifelse(get(extra_vars[i]) == 1, 1, 0)]
  elfe[is.na(get(extra_names[i])), (extra_names[i]):= ifelse(anyactivity3y == 0, 0, get(extra_names[i]))]
}

# MCS

# age 3

mcs[BPOFRE00_m > 0, fqread3y:= BPOFRE00_m]
mcs[is.na(fqread3y) & BPOFRE00_p > 0, fqread3y:= BPOFRE00_p]

mcs[BPREOF00_m > 0 & fqread3y > BPREOF00_m, fqread3y:= BPREOF00_m]
mcs[BPREOF00_p > 0 & fqread3y > BPREOF00_p, fqread3y:= BPREOF00_p]

mcs[, fqread3y:= 6 - fqread3y]

mcs[!is.na(fqread3y), read3y:= ifelse(fqread3y %in% 1:5, 1, 0)]

vars <- c("library3y", "counting3y", "songs3y", "alphabet3y", "paint3y", "physical3y")
m_vars <- c("BPTOLI00_m", "BPNUMB00_m", "BPSONG00_m", "BPALPH00_m", "BPDRAW00_m", "BPSDPA00_m")
p_vars <- c("BPTOLI00_p", "BPNUMB00_p", "BPSONG00_p", "BPALPH00_p", "BPDRAW00_p", "BPSDPA00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[!is.na(get(m_var)), (new_var):= ifelse(get(m_var) == 1, 1, 0)]
  mcs[is.na(get(new_var)), (new_var):= ifelse(get(p_var) == 1, 1, 0)]
}

vars <- c("fqlibrary3y", "fqcounting3y", "fqsongs3y", "fqalphabet3y", "fqpaint3y")
yesnovars <- c("library3y", "counting3y", "songs3y", "alphabet3y", "paint3y")
m_vars <- c("BPOFLI00_m", "BPOFCO00_m", "BPOFSO00_m", "BPOFAB00_m", "BPPAMA00_m")
p_vars <- c("BPOFLI00_p", "BPOFCO00_p", "BPOFSO00_p", "BPOFAB00_p", "BPPAMA00_p")

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  yesno_var <- yesnovars[i]
  new_var <- vars[i]
  
  mcs[!is.na(get(m_var)), (new_var):= ifelse(get(m_var) > 0, get(m_var), NA)]
  mcs[is.na(get(new_var)), (new_var):= ifelse(get(p_var) > 0, get(p_var), NA)]
  mcs[, (new_var):= ifelse(get(yesno_var) == 0, 0, get(new_var))]
}

# age 5

vars <- c("read5y", "stories5y", "songs5y", "paint5y", "physical5y", "indoor5y", "park5y")
fqvars <- c("fqread5y", "fqstories5y", "fqsongs5y", "fqpaint5y", "fqphysical5y", "fqindoor5y", "fqpark5y")
m_vars <- c("CPREOF00_m", "CPSITS00_m", "CPPLMU00_m", "CPPAMA00_m", "CPACTI00_m", "CPGAME00_m", "CPWALK00_m")
p_vars <- c("CPREOF00_p", "CPSITS00_p", "CPPLMU00_p", "CPPAMA00_p", "CPACTI00_p", "CPGAME00_p", "CPWALK00_p")

for (i in seq_along(fqvars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- fqvars[i]
  
  mcs[!is.na(get(m_var)), (new_var) := fifelse(get(m_var) > 0, 6 - get(m_var), NA_integer_)]
  mcs[is.na(get(new_var)) & get(p_var) > 0, (new_var) := 6 - get(p_var)]
}

for (i in seq_along(vars)) {
  m_var <- m_vars[i]
  p_var <- p_vars[i]
  new_var <- vars[i]
  
  mcs[get(m_var) > 0, (new_var) := fifelse(get(m_var) %in% 1:5, 1, 0)]
  mcs[is.na(get(new_var)) & get(p_var) > 0, (new_var) := fifelse(get(p_var) %in% 1:5, 1, 0)]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe_variables <- c(elfe_variables,
                    
                    "frpaint3y", "frread3y", "music3y", "readplus3y", "frcounting3y", "writing3y", "puzzle3y",
                    # Parent-child activities: "No", "Yes"
                    
                    "anyactivity3y", 
                    # extra activities? "No", "Yes"
                    
                    "swimming3y", "gymnastics3y", "circus3y", "sportsinit3y", "musicclass3y", "danceclass3y", "visualarts3y", "horseriding3y"
                    ) # child's activities in clubs: "No", "Yes"

mcs_variables <- c(mcs_variables,
                   
                   "read3y", "library3y", "counting3y", "songs3y", "alphabet3y", "paint3y", "physical3y",
                   # anyone does this with child? "No", "Yes"
                   
                   "read5y", "stories5y", "songs5y", "paint5y", "physical5y", "indoor5y", "park5y",
                   # anyone does this with child? "No", "Yes"  
                   
                   "fqread3y", 
                   # how often? "Not at all", "Less often", "Once or twice a month", "Once or twice a week", "Several times a week", "Every day"

                   "fqlibrary3y", 
                   # how often? "Not at all" "On special occasions", "Once a month", "Once a fortnight", "Once a week"
                   
                   "fqcounting3y", "fqsongs3y", "fqalphabet3y", "fqpaint3y",
                   # how often? "Not at all" "Occasionally or less than once a week", "1 - 2 days per week", "3 times a week", "4 times a week", "5 times a week", "6 times a week", "7 times a week/constantly"
                   
                   "fqread5y", "fqstories5y", "fqsongs5y", "fqpaint5y", "fqphysical5y", "fqindoor5y", "fqpark5y"
                   ) # how often do you these with child? "Not at all", "Less often", "Once or twice a month", "Once or twice a week", "Several times a week", "Every day"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save before imputation of covariates missings ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
beep()

names(mcs)[names(mcs) == "" | is.na(names(mcs))] <- paste0("unnamed_", seq_len(sum(names(mcs) == "" | is.na(names(mcs)))))

mcs <- mcs %>%
  filter(!is.na(sex))

elfemini_beforeimp <- elfe[, ..elfe_variables]
mcsmini_beforeimp <- mcs[, ..mcs_variables]

elfe <- as.data.frame(elfe)
save(elfe, file = "data/analysisdata/elfe.Rdata")

mcs <- as.data.frame(mcs)
save(mcs, file = "data/analysisdata/mcs.Rdata")

elfemini_beforeimp <- as.data.frame(elfemini_beforeimp)
save(elfemini_beforeimp, file = "data/analysisdata/elfemini_beforeimp.Rdata")

mcsmini_beforeimp <- as.data.frame(mcsmini_beforeimp)
save(mcsmini_beforeimp, file = "data/analysisdata/mcsmini_beforeimp.Rdata")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Missingness ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfe <- as.data.table(elfe)
mcs <- as.data.table(mcs)

# Work status

elfe[is.na(emp2m), emp2m:= 1]
 
mcs[is.na(emp9m) & meduc3 == 1, emp9m:= 2]
mcs[is.na(emp9m) & meduc3 == 2, emp9m:= 1]
mcs[is.na(emp9m) & meduc3 == 3, emp9m:= 1]

# Leave

elfe[is.na(leave2m), leave2m:= 1]


mcs[is.na(leave9m) & meduc3 == 1, leave9m:= 4]
mcs[is.na(leave9m) & meduc3 == 2, leave9m:= 1]
mcs[is.na(leave9m) & meduc3 == 3, leave9m:= 1]

# Family structure

elfe[is.na(twopar2m), twopar2m:= 1]
mcs[is.na(twopar9m), twopar9m:= 1]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save after imputation of covariates missings ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

elfemini <- elfe[, ..elfe_variables]
mcsmini <- mcs[, ..mcs_variables]

elfemini <- as.data.frame(elfemini)
save(elfemini, file = "data/analysisdata/elfemini.Rdata")

mcsmini <- as.data.frame(mcsmini)
save(mcsmini, file = "data/analysisdata/mcsmini.Rdata")

gc()

beep()









