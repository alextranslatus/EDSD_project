
library(readr) # read csv
library(haven) # read dta
library(dplyr) # basics
library(data.table) # data.table

# Loading Elfe
base <- read_csv("data/20250610DEM_1066_LP/DATA_DEM_1066_LP.csv")
dict_elfe <- read_delim("data/20250610DEM_1066_LP/CCT_DEM_1066_LP.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

eqr0 <- read_csv("data/20250610DEM_1066_LP/EQR0_SOCIODEMO.csv")
eqr5 <- read_csv("data/20250610DEM_1066_LP/EQR5_CODEAGEPROFESSIONSP.csv")
eqr12 <- read_csv("data/20250610DEM_1066_LP/EQR12_VARIABLESOCIODEMO.csv")
eqr28 <- read_csv("data/20250610DEM_1066_LP/EQR28_MCARTHUR.csv")
eqr39 <- read_csv("data/20250610DEM_1066_LP/EQR39_SCOREIDE.csv")

base <- full_join(base, eqr0, by = "id_DEM_1066_LP")
base <- full_join(base, eqr5, by = "id_DEM_1066_LP")
base <- full_join(base, eqr12, by = "id_DEM_1066_LP")
base <- full_join(base, eqr28, by = "id_DEM_1066_LP")
base <- full_join(base, eqr39, by = "id_DEM_1066_LP")

rm(list = ls(pattern = "^eqr"))

# Loading MCS
mcs <- read_dta("data/mcs123_clean.dta")

# To data.table
base <- as.data.table(base)
mcs <- as.data.table(mcs)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weights ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe: A01E_PONDREF A02E_PONDREF A03E_PONDREF A05E_PONDREF
# mcs: wgt9m wgt3 wgt5

# Elfe


# MCS

mcs <- mcs %>% rename(wgt9m = AOVWT2, wgt3 = BOVWT2, wgt5 = COVWT2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe & mcs
# sex: 1 Male 2 Female

# Elfe

base <- base %>% rename(sex = SEXE_ENF) 

# MCS

mcs <- mcs %>% rename(sex = AHCSEX00)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# age1y age2y age3y 
# 
# mcs
# age9m age3y age5y

# Elfe

base <- combine("A01M_AGE1A", "A01P_AGE1A", "age1y")
base <- combine("A02M_AGE2A", "A02P_AGE2A", "age2y")
base$age3y <- base$A03R_AGE3A

# MCS

mcs[, age9m:= round(AHCAGE00 / 30.44, 2)]
mcs[, age3y:= round(BHCAGE00 / 30.44, 2)]
mcs[CHCAGE00 > 0, age5y:= round(CHCAGE00 / 30.44, 2)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Potty variables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# A01C_POT A02C_POT: 1 Never 2 Sometimes 3 Often 4 Always
# A02C_COUCHNUI A02C_COUCHJOU (porte des couches ?): 1 Yes, always 2 Yes, sometimes 3 No
# bin_pot_1: 0 Never use potty 1 Ever use potty
# bin_pot_2: 0 Never/sometimes use potty 1 Often/always use potty
# bin_couchn_2 bin_couchj_2: 0 Always in nappies 1 No/sometimes
# bin_cdi8_3 bin_cdi8_5: 0 Doesn't 1 Fully controls bladder and bowel, day and night
# 
# mcs
# dry3 clean3 (dry3rev clean3rev): 1 Always 2 Sometimes 3 Never
# wetday5 wetnight5 (wetday5rev wetnight5rev): 1 Never wets during the day 2 Occasionally wets during the day 3 Wets during the day once or twice a wee 4 Wets during the day 3 or more times a w 5 Wears nappies or pull-ups during the da 
# bin_dry3 bin_clean3: 0 Not always 1 Always
# bin_wetday5 bin_wetnight5: 0 Ever 1 Never

# Elfe

base <- combine("A01M_POT", "A01P_POT", "A01C_POT")
base <- combine("A02M_POT", "A02P_POT", "A02C_POT")
base <- combine("A02M_COUCHNUI", "A02P_COUCHNUI", "A02C_COUCHNUI")
base <- combine("A02M_COUCHJOU", "A02P_COUCHJOU", "A02C_COUCHJOU")

base[A01C_POT == 1, bin_pot_1:= 0]
base[A01C_POT %in% c(2, 3, 4), bin_pot_1:= 1]

base[A02C_POT %in% c(1, 2), bin_pot_2:= 0]
base[A02C_POT %in% c(3, 4), bin_pot_2:= 1]

base[A02C_COUCHNUI == 1, bin_couchn_2:= 0]
base[A02C_COUCHNUI %in% c(2, 3), bin_couchn_2:= 1]

base[A02C_COUCHJOU == 1, bin_couchj_2:= 0]
base[A02C_COUCHJOU %in% c(2, 3), bin_couchj_2:= 1]

base[A03R_CDI8 == 1, bin_cdi8_3:= 1]
base[A03R_CDI8 == 2, bin_cdi8_3:= 0]

base[A05R_CDI8 == 1, bin_cdi8_5:= 1]
base[A05R_CDI8 == 2, bin_cdi8_5:= 0]

# Additive index

base$sc_p1 <- recode(base$A01C_POT, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN)
base$sc_p2 <- recode(base$A02C_POT, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN)
base$sc_cn <- recode(base$A02C_COUCHNUI, `1` = 0, `2` = 1, `3` = 2, .default = NaN)
base$sc_cj <- recode(base$A02C_COUCHJOU, `1` = 0, `2` = 1, `3` = 2, .default = NaN)
base$sc_cdi <- recode(base$bin_cdi8_3, `0` = 0, `1` = 1, .default = NaN)

base$sc <- base$sc_p1 + base$sc_p2 + base$sc_cn + base$sc_cj + base$sc_cdi
base$sc1 <- base$sc_p1 + base$sc_p2 + base$sc_cn + base$sc_cj
base$sc2 <- base$sc_cdi

base$std_sc <- base$sc
base$std_sc1 <- base$sc1
base$std_sc2 <- base$sc2

base <- base %>% mutate_at(c('std_sc'), ~(scale(.) %>% as.vector))
base <- base %>% mutate_at(c('std_sc1'), ~(scale(.) %>% as.vector))
base <- base %>% mutate_at(c('std_sc2'), ~(scale(.) %>% as.vector))


# MCS

mcs <- mcs %>% rename(dad9m = APNACH00, 
                      resp9m = APCHNA00,
                      dry3 = BPDRDA00,
                      clean3 = BPCLDA00,
                      wetday5 = CPDRYD00,
                      wetnight5  = CPDRYN00)

mcs[dry3 < 0 | dry3 == 4, dry3:= NA]
mcs[clean3 < 0 | clean3 == 4, clean3:= NA]
mcs[wetday5 < 0, wetday5:= NA]
mcs[wetnight5 < 0, wetnight5:= NA]

mcs[, dry3rev:= -(dry3 - 4)]
mcs[, clean3rev:= -(clean3 - 4)]
mcs[, wetday5rev:= -(wetday5 - 6)]
mcs[, wetnight5rev:= -(wetnight5 - 6)]

mcs[clean3 < 0 | clean3 == 4, clean3:= NA]
mcs[wetday5 < 0, wetday5:= NA]
mcs[wetnight5 < 0, wetnight5:= NA]

mcs[dry3 == 1, bin_dry3:= 1]
mcs[dry3 %in% c(2, 3), bin_dry3:= 0]

mcs[clean3 == 1, bin_clean3:= 1]
mcs[clean3 %in% c(2, 3), bin_clean3:= 0]

mcs[wetday5 == 1, bin_wetday5:= 1]
mcs[wetday5 %in% c(2, 3, 4, 5), bin_wetday5:= 0]

mcs[wetnight5 == 1, bin_wetnight5:= 1]
mcs[wetnight5 %in% c(2, 3, 4, 5), bin_wetnight5:= 0]

# Additive index

mcs$sc_d <- recode(mcs$dry3, `1` = 2, `2` = 1, `3` = 0, .default = NaN)
mcs$sc_c <- recode(mcs$clean3, `1` = 2, `2` = 1, `3` = 0, .default = NaN)
mcs$sc_wd <- recode(mcs$wetday5, `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0, .default = NaN)
mcs$sc_wn <- recode(mcs$wetnight5, `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0, .default = NaN)

mcs$sc <- mcs$sc_d + mcs$sc_c + mcs$sc_wd + mcs$sc_wn
mcs$sc1 <- mcs$sc_d + mcs$sc_c
mcs$sc2 <- mcs$sc_wd + mcs$sc_wn

mcs$std_sc <- mcs$sc
mcs$std_sc1 <- mcs$sc1
mcs$std_sc2 <- mcs$sc2

mcs <- mcs %>% mutate_at(c('std_sc'), ~(scale(.) %>% as.vector))
mcs <- mcs %>% mutate_at(c('std_sc1'), ~(scale(.) %>% as.vector))
mcs <- mcs %>% mutate_at(c('std_sc2'), ~(scale(.) %>% as.vector))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Education ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# meduc: 1 >bac+4 2 bac+3/4 3 bac+2 4 bac 5 cap-bep 6 <=bepc
# meduc3: 1 low 2 med 3 high
# mcs
# meduc: 1 higher deg 2 bach 3 HE below deg 4 a-level 5 trade 6 GCSE A-C 7 GCSE D-G 8 Other 9 None
# meduc3: 1 low 2 med 3 high

# Elfe

base[!is.na(meducaf_2m), meducaf_imp:= meducaf_2m]
base[is.na(meducaf_imp) & !is.na(meducaf_1y), meducaf_imp:= meducaf_1y]
base[is.na(meducaf_imp) & !is.na(meducaf_2y), meducaf_imp:= meducaf_2y]
base[is.na(meducaf_imp) & !is.na(meducaf_3y), meducaf_imp:= meducaf_3y]

base[meducaf_imp == 6, meduc:= 1]
base[meducaf_imp == 5, meduc:= 2]
base[meducaf_imp == 4, meduc:= 3]
base[meducaf_imp == 3, meduc:= 4]
base[meducaf_imp == 2, meduc:= 5]
base[meducaf_imp == 1, meduc:= 6]

base[meduc %in% c(4, 5, 6), meduc3:= 1] # up to and including bac
base[meduc == 3, meduc3:= 2] # bac+2
base[meduc %in% c(1, 2), meduc3:= 3] # higher than bac+2

# Bac could be put in the middle category? Would put us in line with dice

# MCS

mcs[APACQU00 == 96 | APVCQU00 == 96, meduc:= 9] # None of these qualifications | None of these qualifications 
mcs[APACQU00 == 95 | APVCQU00 == 95, meduc:= 8] # Other academic qualifications | Other vocational qualifications
mcs[APACQU00 == 6 | APVCQU00 == 6, meduc:= 7] # GCSE grades D-G | NVQ / SVQ / GSVQ level 1
mcs[APACQU00 == 5 | APVCQU00 == 5, meduc:= 6] # O level / GCSE grades A-C | NVQ / SVQ / GSVQ level 2
mcs[                APVCQU00 == 4, meduc:= 5] # Trade apprenticeships
mcs[APACQU00 == 4 | APVCQU00 == 3, meduc:= 4] # A / AS / S levels | NVQ / SVQ / GSVQ level 3
mcs[APACQU00 == 3 | APVCQU00 == 2, meduc:= 3] # Dipl in higher ed | Nursing / other medical qualifications
mcs[APACQU00 == 2 | APVCQU00 == 1, meduc:= 2] # First degree |  Professional quals at degree level
mcs[APACQU00 == 1                , meduc:= 1] # Higher degree

## 3 categories, low, med, high
mcs[meduc %in% c(6,7,8,9), meduc3:= 1] # Up to and including GCSEs
mcs[meduc %in% c(3,4,5), meduc3:= 2] # Trade, A-level and HE below degree
mcs[meduc %in% c(1,2), meduc3:= 3] # Bachelor's degree or Higher degree
mcs[meduc == 9 & APLFTE00 > 16 & !is.na(APLFTE00), meduc3:= 2]

# Cross-checked with DICE documentation: the code is correct. Differences with DICE tabulations
# are because they look at parental edu and not just maternal edu.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Employment ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# memp2m memp1y memp2y memp3y: 1 In a job/self-employed/student 2 Stay-at-home/at home for some reason 3 On maternity leave 
# mptime1y mptime2y mptime3y: 0 No 1 Yes
# 
# mcs
# memp9m memp3y: 1 In a job/self-employed/student 2 Stay-at-home/at home for some reason 3 On maternity leave 
# mptime9m mptime3y: 0 No 1 Yes

# Elfe

base[M02M_LIENTYP_3 == 2, congmat_m_2m:= M02M_CONGMATPAR_3]
base[M02P_LIENTYP_3 == 2 & is.na(congmat_m_2m), congmat_m_2m:= M02P_CONGMATPAR_3]
base[M02M_LIENTYP_3 == 2, congmat_m_1y:= A01M_CONGMATPAR_3]
base[A02M_LIENTYP_3 == 2, congmat_m_2y:= A02M_CONGMATPAR_3]
base[A03R_LIENTYP_3 == 2, congmat_m_3y:= A03R_CONGMATPAR_3]
base[A03R_LIENTYP_4 == 2, congmat_m_3y:= A03R_CONGMATPAR_4]

base[mother_occup_status_2m %in% c(1,4), memp2m:= 1]
base[mother_occup_status_2m %in% c(2,3), memp2m:= 2]
base[congmat_m_2m %in% c(1, 2), memp2m:= 3]

base[mother_occup_status_1y %in% c(1,4), memp1y:= 1]
base[mother_occup_status_1y %in% c(2,3), memp1y:= 2]
base[congmat_m_1y %in% c(1, 2), memp1y:= 3]

base[mother_occup_status_2y %in% c(1,4), memp2y:= 1]
base[mother_occup_status_2y %in% c(2,3), memp2y:= 2]
base[congmat_m_2y %in% c(1, 2), memp2y:= 3]

base[mother_occup_status_3y %in% c(1,4), memp3y:= 1]
base[mother_occup_status_3y %in% c(2,3), memp3y:= 2]
base[congmat_m_3y %in% c(1, 2), memp3y:= 3]

## part-time 

base[M02M_LIENTYP_3 == 2, txpart_mere_1y:= A01M_EMPLTX_3]

base[A02M_LIENTYP_3 == 2, txpart_mere_2y:= A02M_EMPLTX_3]
base[A02P_LIENTYP_3 == 2 & is.na(txpart_mere_2y), txpart_mere_2y:= A02P_EMPLTX_3]

base[A03R_LIENTYP_3 == 2, txpart_mere_3y:= A03R_EMPLTX_3]
base[A03R_LIENTYP_4 == 2 & is.na(txpart_mere_3y), txpart_mere_3y:= A03R_EMPLTX_4]

base$mptime1y <- with(base, ifelse(!is.na(txpart_mere_1y) | A01M_EMPL_3 == 2, 1, 0))
base$mptime2y <- with(base, ifelse(!is.na(txpart_mere_2y) | A02M_EMPL_3 == 2, 1, 0))
base$mptime3y <- with(base, ifelse(!is.na(txpart_mere_3y) | A03R_EMPL_3 == 2, 1, 0))

# MCS

mcs[APMACT00 %in% c(1, 3, 4), memp9m:= 1]
mcs[APMACT00 == 5, memp9m:= 2]
mcs[APMACT00 == 2, memp9m:= 3]

mcs[ACREWK00 == 2, memp9m:= 3]

mcs[BDDACT00 %in% c(1, 2, 5, 6), memp3y:= 1]
mcs[BDDACT00 %in% c(7, 3, 4, 8, 9), memp3y:= 2]
mcs[BPAWWY00 %in% c(1, 2, 3), memp3y:= 3]

## part-time

mcs$mptime9m <- with(mcs, ifelse(APFLXW0A == 1 | APFLXW0B == 1 | APFLXW0C == 1 | APFLXW0D == 1 | APFLXW0E == 1 | APFLXW0F == 1, 1, 0))
# mcs$mremw9m <- with(mcs, ifelse(APFLXW0A %in% c(4, 5) | APFLXW0B %in% c(4, 5) | APFLXW0C %in% c(4, 5) | APFLXW0D %in% c(4, 5) | APFLXW0E %in% c(4, 5) | APFLXW0F %in% c(4, 5), 1, 0))

mcs$mptime3y <- with(mcs, ifelse(BPFLXW0A == 1, 1, 0))
# mcs$mremw3 <- with(mcs, ifelse(BPFLXW0D == 1 | BPFLXW0E == 1, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Income ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# incq2m incq1y incq2y incq3y: 5 quintiles
# 
# mcs
# incq9m incq3y incq5y: 5 quintiles

# Elfe
base[, incq2m:= revenu_part_qui_2m]
base[, incq1y:= revenu_part_qui_1y]
base[, incq2y:= revenu_part_qui_2y]
base[, incq3y:= revenu_part_qui_3y]

# MCS

mcs[AOECDUK0 >= 1, incq9m:= AOECDUK0]
mcs[BOECDUK0 >= 1, incq3y:= BOECDUK0]
mcs[COECDUK0 >= 1, incq5y:= COECDUK0]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Migration ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe and mcs
# mborn fborn: 0 No 1 Yes

# Elfe

base$mborn <- with(base, ifelse(mbirthfr == 1, 1, 0))
base$fborn <- with(base, ifelse(fbirthfr == 1, 1, 0))

# MCS

mcs$mborn <- with(mcs, ifelse(BPREBO00 == 1, 1, 0))
mcs$fborn <- with(mcs, ifelse(BPREBO00_p == 1, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Siblings ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe and mcs
# sibs: 1. 0 2. 1 boy 3. 1 girl 4. 2 boys 5. 2 girls 6. 2 mix

# Elfe

columns <- 4:13
for (i in 4:13) {
  col_suffix <- paste0("_", i)
  
  base[base[[paste0("A01M_LIENTYP", col_suffix)]] %in% c(3, 4, 5, 6), paste0("enf_1y", col_suffix) := 1]
  base[is.na(base[[paste0("enf_1y", col_suffix)]]) & base[[paste0("A01P_LIENTYP", col_suffix)]] %in% c(3, 4, 5, 6), paste0("enf_1y", col_suffix) := 1]
  base[is.na(base[[paste0("enf_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("enf_1y", col_suffix) := 0]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01M_SEXE", col_suffix)]] == 1, paste0("sexem_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_adelp_1y", col_suffix) := 0]
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01M_SEXE", col_suffix)]] == 2, paste0("sexef_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_adelp_1y", col_suffix) := 0]
  
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01P_SEXE", col_suffix)]] == 1, paste0("sexem_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_adelp_1y", col_suffix) := 0]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01P_SEXE", col_suffix)]] == 2, paste0("sexef_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_adelp_1y", col_suffix) := 0]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1, paste0("annee_nais_1y", col_suffix) := get(paste0("A01M_ANAIS", col_suffix))]
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & is.na(base[[paste0("annee_nais_1y", col_suffix)]]), paste0("annee_nais_1y", col_suffix) := get(paste0("A01P_ANAIS", col_suffix))]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("annee_nais_1y", col_suffix)]] < 2011, paste0("aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("aine_1y", col_suffix) := 0]
  
  base[base[[paste0("aine_1y", col_suffix)]] == 1 & base[[paste0("sexem_adelp_1y", col_suffix)]] == 1, paste0("sexem_aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("sexem_aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_aine_1y", col_suffix) := 0]
  base[base[[paste0("aine_1y", col_suffix)]] == 1 & base[[paste0("sexef_adelp_1y", col_suffix)]] == 1, paste0("sexef_aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("sexef_aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_aine_1y", col_suffix) := 0]
  
}

base[(sexem_aine_1y_4 == 1 | enf_1y_4 == 0) &
       (sexem_aine_1y_5 == 1 | enf_1y_5 == 0) &
       (sexem_aine_1y_6 == 1 | enf_1y_6 == 0) &
       (sexem_aine_1y_7 == 1 | enf_1y_7 == 0) &
       (sexem_aine_1y_8 == 1 | enf_1y_8 == 0) &
       (sexem_aine_1y_9 == 1 | enf_1y_9 == 0) &
       (sexem_aine_1y_10 == 1 | enf_1y_10 == 0) &
       (sexem_aine_1y_11 == 1 | enf_1y_11 == 0) &
       (sexem_aine_1y_12 == 1 | enf_1y_12 == 0) &
       (sexem_aine_1y_13 == 1 | enf_1y_13 == 0), comp_sex_aines_1y:= 1] # Que des frères

base[(sexef_aine_1y_4 == 1 | enf_1y_4 == 0) &
       (sexef_aine_1y_5 == 1 | enf_1y_5 == 0) &
       (sexef_aine_1y_6 == 1 | enf_1y_6 == 0) &
       (sexef_aine_1y_7 == 1 | enf_1y_7 == 0) &
       (sexef_aine_1y_8 == 1 | enf_1y_8 == 0) &
       (sexef_aine_1y_9 == 1 | enf_1y_9 == 0) &
       (sexef_aine_1y_10 == 1 | enf_1y_10 == 0) &
       (sexef_aine_1y_11 == 1 | enf_1y_11 == 0) &
       (sexef_aine_1y_12 == 1 | enf_1y_12 == 0) &
       (sexef_aine_1y_13 == 1 | enf_1y_13 == 0), comp_sex_aines_1y:= 2] # Que des sœurs

base[((sexem_aine_1y_4 == 1 | sexef_aine_1y_4 == 1) | 
        (sexem_aine_1y_5 == 1 | sexef_aine_1y_5 == 1) | 
        (sexem_aine_1y_6 == 1 | sexef_aine_1y_6 == 1) | 
        (sexem_aine_1y_7 == 1 | sexef_aine_1y_7 == 1) | 
        (sexem_aine_1y_8 == 1 | sexef_aine_1y_8 == 1) | 
        (sexem_aine_1y_9 == 1 | sexef_aine_1y_9 == 1) | 
        (sexem_aine_1y_10 == 1 | sexef_aine_1y_10 == 1) | 
        (sexem_aine_1y_11 == 1 | sexef_aine_1y_11 == 1) | 
        (sexem_aine_1y_12 == 1 | sexef_aine_1y_12 == 1) | 
        (sexem_aine_1y_13 == 1 | sexef_aine_1y_13 == 1)) & 
       comp_sex_aines_1y %nin% c(1, 2), comp_sex_aines_1y:= 3] # Aînés mixte

base[aine_1y_4 == 0 & 
       aine_1y_5 == 0 &
       aine_1y_6 == 0 & 
       aine_1y_7 == 0 & 
       aine_1y_8 == 0 & 
       aine_1y_9 == 0 & 
       aine_1y_10 == 0 & 
       aine_1y_11 == 0 & 
       aine_1y_12 == 0 & 
       aine_1y_13 == 0, comp_sex_aines_1y:= 4] # Pas d'aînés

base[, nb_aines_1y:= aine_1y_4 + aine_1y_5 + aine_1y_6 + aine_1y_7 + aine_1y_8 + aine_1y_9 + aine_1y_10 + aine_1y_11 + aine_1y_12 + aine_1y_13]

base[nb_aines_1y == 0, comp_sex_aines3_1y:= 1] # Pas d'aîné
base[comp_sex_aines_1y == 1 & nb_aines_1y == 1, comp_sex_aines3_1y:= 2] # 1 aîné
base[comp_sex_aines_1y == 2 & nb_aines_1y == 1, comp_sex_aines3_1y:= 3] # 1 aînée
base[comp_sex_aines_1y == 1 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 4] # 2 aînés ou +
base[comp_sex_aines_1y == 2 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 5] # 2 aînées ou +
base[comp_sex_aines_1y == 3 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 6] # Aînés mixte (2 ou +)

base[, sibs_d:= comp_sex_aines3_1y]

base[sibs_d == 1, sibs:= 0] # no older siblings

base[sibs_d == 2 & sex == 1, sibs:= 1] # 1 older sib, same sex
base[sibs_d == 3 & sex == 2, sibs:= 1] # same

base[sibs_d == 4 & sex == 1, sibs:= 2] # 2+ older sibs, same sex
base[sibs_d == 5 & sex == 2, sibs:= 2] # same

base[sibs_d == 2 & sex == 2, sibs:= 3] # 1 older sib, different sex
base[sibs_d == 3 & sex == 1, sibs:= 3] # same

base[sibs_d == 4 & sex == 2, sibs:= 4] # 2+ older sibs, different sex
base[sibs_d == 5 & sex == 1, sibs:= 4] # same

base[sibs_d == 6, sibs:= 5] # 2+ older sibs, mixed 

# MCS

# print_table <- function(variable) {
#   cat("Table for", variable, "\n")
#   print(table(mcs[[variable]]))
#   cat("\n")
# }
# 
# variable_names <- paste0("AHPSEX00_", 1:9, "f")
# lapply(variable_names, print_table)

mcs$AHPSEX00_1f <- with(mcs, ifelse(AHPSEX00_1 == 2, 1, 0)) # constructing indicators I can then add to count sisters
mcs$AHPSEX00_2f <- with(mcs, ifelse(AHPSEX00_2 == 2, 1, 0))
mcs$AHPSEX00_3f <- with(mcs, ifelse(AHPSEX00_3 == 2, 1, 0))
mcs$AHPSEX00_4f <- with(mcs, ifelse(AHPSEX00_4 == 2, 1, 0))
mcs$AHPSEX00_5f <- with(mcs, ifelse(AHPSEX00_5 == 2, 1, 0))
mcs$AHPSEX00_6f <- with(mcs, ifelse(AHPSEX00_6 == 2, 1, 0))
mcs$AHPSEX00_7f <- with(mcs, ifelse(AHPSEX00_7 == 2, 1, 0))
mcs$AHPSEX00_8f <- with(mcs, ifelse(AHPSEX00_8 == 2, 1, 0))
mcs$AHPSEX00_9f <- with(mcs, ifelse(AHPSEX00_9 == 2, 1, 0))

mcs$AHPSEX00_1m <- with(mcs, ifelse(AHPSEX00_1 == 1, 1, 0)) # constructing indicators I can then add to count brothers
mcs$AHPSEX00_2m <- with(mcs, ifelse(AHPSEX00_2 == 1, 1, 0))
mcs$AHPSEX00_3m <- with(mcs, ifelse(AHPSEX00_3 == 1, 1, 0))
mcs$AHPSEX00_4m <- with(mcs, ifelse(AHPSEX00_4 == 1, 1, 0))
mcs$AHPSEX00_5m <- with(mcs, ifelse(AHPSEX00_5 == 1, 1, 0))
mcs$AHPSEX00_6m <- with(mcs, ifelse(AHPSEX00_6 == 1, 1, 0))
mcs$AHPSEX00_7m <- with(mcs, ifelse(AHPSEX00_7 == 1, 1, 0))
mcs$AHPSEX00_8m <- with(mcs, ifelse(AHPSEX00_8 == 1, 1, 0))
mcs$AHPSEX00_9m <- with(mcs, ifelse(AHPSEX00_9 == 1, 1, 0))

mcs$AHPDBY00_1o <- with(mcs, ifelse(AHPDBY00_1 < AHCDBY00, 1, 0)) # older than MCS
mcs$AHPDBY00_2o <- with(mcs, ifelse(AHPDBY00_2 < AHCDBY00, 1, 0))
mcs$AHPDBY00_3o <- with(mcs, ifelse(AHPDBY00_3 < AHCDBY00, 1, 0))
mcs$AHPDBY00_4o <- with(mcs, ifelse(AHPDBY00_4 < AHCDBY00, 1, 0))
mcs$AHPDBY00_5o <- with(mcs, ifelse(AHPDBY00_5 < AHCDBY00, 1, 0))
mcs$AHPDBY00_6o <- with(mcs, ifelse(AHPDBY00_6 < AHCDBY00, 1, 0))
mcs$AHPDBY00_7o <- with(mcs, ifelse(AHPDBY00_7 < AHCDBY00, 1, 0))
mcs$AHPDBY00_8o <- with(mcs, ifelse(AHPDBY00_8 < AHCDBY00, 1, 0))
mcs$AHPDBY00_9o <- with(mcs, ifelse(AHPDBY00_9 < AHCDBY00, 1, 0))

mcs$oldsis_1 <- with(mcs, ifelse(AHPSEX00_1f == 1 & AHPDBY00_1o == 1, 1, 0)) # older sisters indicators
mcs$oldsis_2 <- with(mcs, ifelse(AHPSEX00_2f == 1 & AHPDBY00_2o == 1, 1, 0))
mcs$oldsis_3 <- with(mcs, ifelse(AHPSEX00_3f == 1 & AHPDBY00_3o == 1, 1, 0))
mcs$oldsis_4 <- with(mcs, ifelse(AHPSEX00_4f == 1 & AHPDBY00_4o == 1, 1, 0))
mcs$oldsis_5 <- with(mcs, ifelse(AHPSEX00_5f == 1 & AHPDBY00_5o == 1, 1, 0))
mcs$oldsis_6 <- with(mcs, ifelse(AHPSEX00_6f == 1 & AHPDBY00_6o == 1, 1, 0))
mcs$oldsis_7 <- with(mcs, ifelse(AHPSEX00_7f == 1 & AHPDBY00_7o == 1, 1, 0))
mcs$oldsis_8 <- with(mcs, ifelse(AHPSEX00_8f == 1 & AHPDBY00_8o == 1, 1, 0))
mcs$oldsis_9 <- with(mcs, ifelse(AHPSEX00_9f == 1 & AHPDBY00_9o == 1, 1, 0))

mcs$oldbro_1 <- with(mcs, ifelse(AHPSEX00_1m == 1 & AHPDBY00_1o == 1, 1, 0)) # older brothers indicators
mcs$oldbro_2 <- with(mcs, ifelse(AHPSEX00_2m == 1 & AHPDBY00_2o == 1, 1, 0))
mcs$oldbro_3 <- with(mcs, ifelse(AHPSEX00_3m == 1 & AHPDBY00_3o == 1, 1, 0))
mcs$oldbro_4 <- with(mcs, ifelse(AHPSEX00_4m == 1 & AHPDBY00_4o == 1, 1, 0))
mcs$oldbro_5 <- with(mcs, ifelse(AHPSEX00_5m == 1 & AHPDBY00_5o == 1, 1, 0))
mcs$oldbro_6 <- with(mcs, ifelse(AHPSEX00_6m == 1 & AHPDBY00_6o == 1, 1, 0))
mcs$oldbro_7 <- with(mcs, ifelse(AHPSEX00_7m == 1 & AHPDBY00_7o == 1, 1, 0))
mcs$oldbro_8 <- with(mcs, ifelse(AHPSEX00_8m == 1 & AHPDBY00_8o == 1, 1, 0))
mcs$oldbro_9 <- with(mcs, ifelse(AHPSEX00_9m == 1 & AHPDBY00_9o == 1, 1, 0))

columns <- 1:9
for (i in 1:9) {
  col_suffix <- paste0("_", i)
  
  mcs[is.na(mcs[[paste0("AHPSEX00", col_suffix, "f")]]) & AHCPRS00 == 1, paste0("AHPSEX00", col_suffix, "f") := 0]
  mcs[is.na(mcs[[paste0("AHPSEX00", col_suffix, "m")]]) & AHCPRS00 == 1, paste0("AHPSEX00", col_suffix, "m") := 0]
  mcs[is.na(mcs[[paste0("AHPDBY00", col_suffix, "o")]]) & AHCPRS00 == 1, paste0("AHPDBY00", col_suffix, "o") := 0]
  mcs[is.na(mcs[[paste0("oldsis", col_suffix)]]) & AHCPRS00 == 1, paste0("oldsis", col_suffix) := 0]
  mcs[is.na(mcs[[paste0("oldbro", col_suffix)]]) & AHCPRS00 == 1, paste0("oldbro", col_suffix) := 0]
  
}

mcs[, oldsis_n:= oldsis_1 + oldsis_2 + oldsis_3 + oldsis_4 + oldsis_5 + oldsis_6 + oldsis_7 + oldsis_8 + oldsis_9]
mcs[, oldbro_n:= oldbro_1 + oldbro_2 + oldbro_3 + oldbro_4 + oldbro_5 + oldbro_6 + oldbro_7 + oldbro_8 + oldbro_9]

mcs[oldsis_n == 0 & oldbro_n == 0, sibs_d:= 1]
mcs[oldsis_n == 0 & oldbro_n == 1, sibs_d:= 2]
mcs[oldsis_n == 1 & oldbro_n == 0, sibs_d:= 3] 
mcs[oldsis_n == 0 & oldbro_n == 2, sibs_d:= 4]
mcs[oldsis_n == 2 & oldbro_n == 0, sibs_d:= 5]
mcs[oldsis_n >= 1 & oldbro_n >= 1, sibs_d:= 6]

mcs[sibs_d == 1, sibs:= 0] # no older siblings

mcs[sibs_d == 2 & sex == 1, sibs:= 1] # 1 older sib, same sex
mcs[sibs_d == 3 & sex == 2, sibs:= 1] # same

mcs[sibs_d == 4 & sex == 1, sibs:= 2] # 2+ older sibs, same sex
mcs[sibs_d == 5 & sex == 2, sibs:= 2] # same

mcs[sibs_d == 2 & sex == 2, sibs:= 3] # 1 older sib, different sex
mcs[sibs_d == 3 & sex == 1, sibs:= 3] # same

mcs[sibs_d == 4 & sex == 2, sibs:= 4] # 2+ older sibs, different sex
mcs[sibs_d == 5 & sex == 1, sibs:= 4] # same

mcs[sibs_d == 6, sibs:= 5] # 2+ older sibs, mixed 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Family structure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# lonemum2m lonemum1y lonemum2y lonemum3y: 0 No 1 Yes
#
# mcs
# lonemum9m lonemum3y lonemum5y: 0 No 1 Yes

# Elfe

base$lonemum2m <- with(base, ifelse(Child_hhld_2m == 2, 1, 0))
base$lonemum1y <- with(base, ifelse(child_hhld_1y == 2, 1, 0))
base$lonemum2y <- with(base, ifelse(child_hhld_2y == 2, 1, 0))
base$lonemum3y <- with(base, ifelse(child_hhld_3y == 2, 1, 0))

# MCS

mcs$lonemum9m <- with(mcs, ifelse(ADHTYP00 == 10, 1, 0))
mcs$lonemum3y <- with(mcs, ifelse(BDHTYP00 == 15, 1, 0))
mcs$lonemum5y <- with(mcs, ifelse(CDHTYP00 == 15, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Childcare ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# care1y care2y care3y: 1 Home 2 Childminder 3 Collective 4 Other
# care2y_home care2y_chmind care2y_collec care2y_oth care3y_home care3y_chmind care3y_collec care3y_oth: 0 No 1 Yes
#
# mcs
# care9m care3y
# care3y_home care3y_chmind care3y_collec care3y_oth: 0 No 1 Yes

# Elfe

base <- combine("A01M_GARDENF", "A01P_GARDENF", "A01C_GARDENF")
base[A01C_GARDENF %in% c(1, 2, 3, 4, 5, 8), care1y:= 1] 
base[A01C_GARDENF == 6, care1y:= 2] 
base[A01C_GARDENF == 7, care1y:= 3] 
base[A01C_GARDENF == 9, care1y:= 4]

base <- combine("A02M_GARDENF", "A02P_GARDENF", "A02C_GARDENF")
base[A02C_GARDENF %in% c(1, 2, 3, 4, 5, 8), care2y:= 1] 
base[A02C_GARDENF == 6, care2y:= 2] 
base[A02C_GARDENF == 7, care2y:= 3] 
base[A02C_GARDENF == 9, care2y:= 4]

base[A03R_GARDENF %in% c(1, 2, 3, 4, 5, 8), care3y:= 1] 
base[A03R_GARDENF == 6, care3y:= 2] 
base[A03R_GARDENF == 7, care3y:= 3] 
base[A03R_GARDENF %in% c(9, 10, 11), care3y:= 4]

base$care1y_home <- with(base, ifelse(care1y == 1, 1, 0))
base$care1y_chmind <- with(base, ifelse(care1y == 2, 1, 0))
base$care1y_collec <- with(base, ifelse(care1y == 3, 1, 0))
base$care1y_oth <- with(base, ifelse(care1y == 4, 1, 0))

base$care2y_home <- with(base, ifelse(care1y == 1 | care2y == 1, 1, 0))
base$care2y_chmind <- with(base, ifelse(care1y == 2 | care2y == 2, 1, 0))
base$care2y_collec <- with(base, ifelse(care1y == 3 | care2y == 3, 1, 0))
base$care2y_oth <- with(base, ifelse(care1y == 4 | care2y == 4, 1, 0))

base$care3y_home <- with(base, ifelse(care1y == 1 | care2y == 1 | care3y == 1, 1, 0))
base$care3y_chmind <- with(base, ifelse(care1y == 2 | care2y == 2 | care3y == 2, 1, 0))
base$care3y_collec <- with(base, ifelse(care1y == 3 | care2y == 3 | care3y == 3, 1, 0))
base$care3y_oth <- with(base, ifelse(care1y == 4 | care2y == 4 | care3y == 4, 1, 0))

# MCS

mcs[ACCAWM0A %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) | 
      ACCAWM0B %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACCAWM0C %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACCAWM0D %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACCAWM0E %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACCAWM0F %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACCAWM0G %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) |
      ACOTLM0A %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16) |
      ACOTLM0B %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16) |
      ACOTLM0C %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16) |
      ACOTLM0D %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16) |
      ACOTLM0E %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16) |
      ACOTLM0F %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 16), care9m:= 1] # in a home setting

mcs[ACCAWM0A %in% c(14, 15) | 
      ACCAWM0B %in% c(14, 15) |
      ACCAWM0C %in% c(14, 15) |
      ACCAWM0D %in% c(14, 15) |
      ACCAWM0E %in% c(14, 15) |
      ACCAWM0F %in% c(14, 15) |
      ACCAWM0G %in% c(14, 15) |
      ACOTLM0A %in% c(12, 13) |
      ACOTLM0B %in% c(12, 13) |
      ACOTLM0C %in% c(12, 13) |
      ACOTLM0D %in% c(12, 13) |
      ACOTLM0E %in% c(12, 13) |
      ACOTLM0F %in% c(12, 13), care9m:= 2]

mcs[ACCAWM0A %in% c(16, 17, 18) | 
      ACCAWM0B %in% c(16, 17, 18) |
      ACCAWM0C %in% c(16, 17, 18) |
      ACCAWM0D %in% c(16, 17, 18) |
      ACCAWM0E %in% c(16, 17, 18) |
      ACCAWM0F %in% c(16, 17, 18) |
      ACCAWM0G %in% c(16, 17, 18) |
      ACOTLM0A %in% c(14, 15) |
      ACOTLM0B %in% c(14, 15) |
      ACOTLM0C %in% c(14, 15) |
      ACOTLM0D %in% c(14, 15) |
      ACOTLM0E %in% c(14, 15) |
      ACOTLM0F %in% c(14, 15), care9m:= 3]

mcs[ACCAWM0A %in% c(51, 52, 85, 86, 95) | 
      ACCAWM0B %in% c(51, 52, 85, 86, 95) |
      ACCAWM0C %in% c(51, 52, 85, 86, 95) |
      ACCAWM0D %in% c(51, 52, 85, 86, 95) |
      ACCAWM0E %in% c(51, 52, 85, 86, 95) |
      ACCAWM0F %in% c(51, 52, 85, 86, 95) |
      ACCAWM0G %in% c(51, 52, 85, 86, 95) |
      ACOTLM0A %in% c(86, 95) |
      ACOTLM0B %in% c(86, 95) |
      ACOTLM0C %in% c(86, 95) |
      ACOTLM0D %in% c(86, 95) |
      ACOTLM0E %in% c(86, 95) |
      ACOTLM0F %in% c(86, 95), care9m:= 4]

mcs[BHFFCC00 %in% c(1, 2, 4, 6, 10, 11) | BPCLST00 %in% c(1, 2, 3, 4, 5, 6, 7, 8), care3y:= 1]  
mcs[BHFFCC00 %in% c(14, 15) | BPCLST00 == 9, care3y:= 2]  
mcs[BHFFCC00 %in% c(16, 17, 18) | BPCLST00 == 10, care3y:= 3]  
mcs[BHFFCC00 %in% c(95, 20) | BPCLST00 == 18, care3y:= 4]  

mcs$care3y_home <- with(mcs, ifelse(care9m == 1 | care3y == 1, 1, 0))
mcs$care3y_chmind <- with(mcs, ifelse(care9m == 2 | care3y == 2, 1, 0))
mcs$care3y_collec <- with(mcs, ifelse(care9m == 3 | care3y == 3, 1, 0))
mcs$care3y_oth <- with(mcs, ifelse(care9m == 4 | care3y == 4, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# School ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# school2y school3y: 0 No 1 Yes
# start_school: 1 Two 2 Two and a half 3 Three 4 Three and a half
# 
# mcs
# school5y: 0 No 1 Yes
# start_school: 1 Four 2 Four and a half 3 Five

# Elfe

base$school2y <- with(base, ifelse(A02C_GARDENF == 9, 1, 0))
base$school3y <- with(base, ifelse(A03R_SCOL %in% c(1, 2, 3), 1, 0))

base[A03R_MATERN != 9, start_school:= A03R_MATERN]

# MCS

mcs$school5y <- with(mcs, ifelse(CPSTSC00 == 1, 1, 0))

# CPSTWY00 year started school
# CPSTWM00 month started school
# "AHCDBM00", "AHCDBY00" année et mois de naissance

mcs$start_schoolyear <- with(mcs, ifelse(CPSTWY00 >= 2003 & CPSTWY00 <= 2006, CPSTWY00, NA))
mcs$start_schoolmonth <- with(mcs, ifelse(CPSTWM00 >= 1 & CPSTWM00 <= 12, CPSTWM00, NA))

mcs$startcont_school <- round(with(mcs, start_schoolyear - AHCDBY00 + ((start_schoolmonth - AHCDBM00)/ 12)), 2)

mcs[startcont_school < 4.5, start_school:= 1]
mcs[startcont_school >= 4.5 & startcont_school < 5, start_school:= 2]
mcs[startcont_school >= 5, start_school:= 3]


mcs[, age3y:= BHCAGE00/365]
mcs[, age5y:= CHCAGE00/365]

mcs[]

# age at interview in days BHCAGE00


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base <- as.data.frame(base)
basepot <- base
save(basepot, file = "C:/Users/sheridan_ale/Desktop/Sogenre/R/Data/Elfe/basepot.Rdata")
rm("base")

mcs <- as.data.frame(mcs)
mcspot <- mcs
save(mcspot, file = "C:/Users/sheridan_ale/Desktop/Sogenre/R/Data/MCS/mcspot.Rdata")
rm("mcs", "mcs_orig")

gc()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other hygiene variables Elfe ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


base <- read_sas("./Data/Elfe/basecomp.sas7bdat", NULL)
eqr12 <- read_sas("./Data/Elfe/eqr12_20221121.sas7bdat", NULL)
# douche3 <- read_sas("./Data/Elfe/douche.sas7bdat", NULL)
# 
# 
# douchevars <- colnames(douche3)

eqr12vars <- colnames(eqr12)
eqr12vars <- eqr12vars[-1]
base <- base[ , !(names(base) %in% eqr12vars)]
base <- full_join(base, eqr12, by = "ID_Dem631_379_OS")
rm("eqr12")

base <- as.data.table(base)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe & mcs
# sex: 1 Male 2 Female

# Elfe

base <- base %>% rename(sex = SEXE_ENF) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Age ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# age1y age2y age3y 
# 
# mcs
# age9m age3y age5y

# Elfe

# base <- combine("A01M_AGE1A", "A01P_AGE1A", "age1y")
# base <- combine("A02M_AGE2A", "A02P_AGE2A", "age2y")
# base$age3y <- base$A03R_AGE3A

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Education ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# meduc: 1 >bac+4 2 bac+3/4 3 bac+2 4 bac 5 cap-bep 6 <=bepc
# meduc3: 1 low 2 med 3 high
# mcs
# meduc: 1 higher deg 2 bach 3 HE below deg 4 a-level 5 trade 6 GCSE A-C 7 GCSE D-G 8 Other 9 None
# meduc3: 1 low 2 med 3 high

# Elfe

base[!is.na(meducaf_2m), meducaf_imp:= meducaf_2m]
base[is.na(meducaf_imp) & !is.na(meducaf_1y), meducaf_imp:= meducaf_1y]
base[is.na(meducaf_imp) & !is.na(meducaf_2y), meducaf_imp:= meducaf_2y]
base[is.na(meducaf_imp) & !is.na(meducaf_3y), meducaf_imp:= meducaf_3y]

base[meducaf_imp == 6, meduc:= 1]
base[meducaf_imp == 5, meduc:= 2]
base[meducaf_imp == 4, meduc:= 3]
base[meducaf_imp == 3, meduc:= 4]
base[meducaf_imp == 2, meduc:= 5]
base[meducaf_imp == 1, meduc:= 6]

base[meduc %in% c(4, 5, 6), meduc3:= 1] # up to and including bac
base[meduc == 3, meduc3:= 2] # bac+2
base[meduc %in% c(1, 2), meduc3:= 3] # higher than bac+2

# Bac could be put in the middle category? Would put us in line with dice

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Employment ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# memp2m memp1y memp2y memp3y: 1 In a job/self-employed/student 2 Stay-at-home/at home for some reason 3 On maternity leave 
# mptime1y mptime2y mptime3y: 0 No 1 Yes
# 
# mcs
# memp9m memp3y: 1 In a job/self-employed/student 2 Stay-at-home/at home for some reason 3 On maternity leave 
# mptime9m mptime3y: 0 No 1 Yes

# Elfe

base[M02M_LIENTYP_3 == 2, congmat_m_2m:= M02M_CONGMATPAR_3]
base[M02P_LIENTYP_3 == 2 & is.na(congmat_m_2m), congmat_m_2m:= M02P_CONGMATPAR_3]
base[M02M_LIENTYP_3 == 2, congmat_m_1y:= A01M_CONGMATPAR_3]
base[A02M_LIENTYP_3 == 2, congmat_m_2y:= A02M_CONGMATPAR_3]
base[A03R_LIENTYP_3 == 2, congmat_m_3y:= A03R_CONGMATPAR_3]
base[A03R_LIENTYP_4 == 2, congmat_m_3y:= A03R_CONGMATPAR_4]

base[mother_occup_status_2m %in% c(1,4), memp2m:= 1]
base[mother_occup_status_2m %in% c(2,3), memp2m:= 2]
base[congmat_m_2m %in% c(1, 2), memp2m:= 3]

base[mother_occup_status_1y %in% c(1,4), memp1y:= 1]
base[mother_occup_status_1y %in% c(2,3), memp1y:= 2]
base[congmat_m_1y %in% c(1, 2), memp1y:= 3]

base[mother_occup_status_2y %in% c(1,4), memp2y:= 1]
base[mother_occup_status_2y %in% c(2,3), memp2y:= 2]
base[congmat_m_2y %in% c(1, 2), memp2y:= 3]

base[mother_occup_status_3y %in% c(1,4), memp3y:= 1]
base[mother_occup_status_3y %in% c(2,3), memp3y:= 2]
base[congmat_m_3y %in% c(1, 2), memp3y:= 3]

## part-time 

base[M02M_LIENTYP_3 == 2, txpart_mere_1y:= A01M_EMPLTX_3]

base[A02M_LIENTYP_3 == 2, txpart_mere_2y:= A02M_EMPLTX_3]
base[A02P_LIENTYP_3 == 2 & is.na(txpart_mere_2y), txpart_mere_2y:= A02P_EMPLTX_3]

base[A03R_LIENTYP_3 == 2, txpart_mere_3y:= A03R_EMPLTX_3]
base[A03R_LIENTYP_4 == 2 & is.na(txpart_mere_3y), txpart_mere_3y:= A03R_EMPLTX_4]

# base$mptime1y <- with(base, ifelse(!is.na(txpart_mere_1y) | A01M_EMPL_3 == 2, 1, 0))
# base$mptime2y <- with(base, ifelse(!is.na(txpart_mere_2y) | A02M_EMPL_3 == 2, 1, 0))
# base$mptime3y <- with(base, ifelse(!is.na(txpart_mere_3y) | A03R_EMPL_3 == 2, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Income ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# incq2m incq1y incq2y incq3y: 5 quintiles
# 
# mcs
# incq9m incq3y incq5y: 5 quintiles

# Elfe
base[, incq2m:= revenu_part_qui_2m]
base[, incq1y:= revenu_part_qui_1y]
base[, incq2y:= revenu_part_qui_2y]
base[, incq3y:= revenu_part_qui_3y]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Migration ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe and mcs
# mborn fborn: 0 No 1 Yes

# Elfe

base$mborn <- with(base, ifelse(mbirthfr == 1, 1, 0))
base$fborn <- with(base, ifelse(fbirthfr == 1, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Siblings ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe and mcs
# sibs: 1. 0 2. 1 boy 3. 1 girl 4. 2 boys 5. 2 girls 6. 2 mix

# Elfe

columns <- 4:13
for (i in 4:13) {
  col_suffix <- paste0("_", i)
  
  base[base[[paste0("A01M_LIENTYP", col_suffix)]] %in% c(3, 4, 5, 6), paste0("enf_1y", col_suffix) := 1]
  base[is.na(base[[paste0("enf_1y", col_suffix)]]) & base[[paste0("A01P_LIENTYP", col_suffix)]] %in% c(3, 4, 5, 6), paste0("enf_1y", col_suffix) := 1]
  base[is.na(base[[paste0("enf_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("enf_1y", col_suffix) := 0]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01M_SEXE", col_suffix)]] == 1, paste0("sexem_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_adelp_1y", col_suffix) := 0]
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01M_SEXE", col_suffix)]] == 2, paste0("sexef_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_adelp_1y", col_suffix) := 0]
  
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01P_SEXE", col_suffix)]] == 1, paste0("sexem_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexem_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_adelp_1y", col_suffix) := 0]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("A01P_SEXE", col_suffix)]] == 2, paste0("sexef_adelp_1y", col_suffix) := 1]
  base[is.na(base[[paste0("sexef_adelp_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_adelp_1y", col_suffix) := 0]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1, paste0("annee_nais_1y", col_suffix) := get(paste0("A01M_ANAIS", col_suffix))]
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & is.na(base[[paste0("annee_nais_1y", col_suffix)]]), paste0("annee_nais_1y", col_suffix) := get(paste0("A01P_ANAIS", col_suffix))]
  
  base[base[[paste0("enf_1y", col_suffix)]] == 1 & base[[paste0("annee_nais_1y", col_suffix)]] < 2011, paste0("aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("aine_1y", col_suffix) := 0]
  
  base[base[[paste0("aine_1y", col_suffix)]] == 1 & base[[paste0("sexem_adelp_1y", col_suffix)]] == 1, paste0("sexem_aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("sexem_aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexem_aine_1y", col_suffix) := 0]
  base[base[[paste0("aine_1y", col_suffix)]] == 1 & base[[paste0("sexef_adelp_1y", col_suffix)]] == 1, paste0("sexef_aine_1y", col_suffix):= 1]
  base[is.na(base[[paste0("sexef_aine_1y", col_suffix)]]) & !is.na(A01E_PONDREF), paste0("sexef_aine_1y", col_suffix) := 0]
  
}

base[(sexem_aine_1y_4 == 1 | enf_1y_4 == 0) &
       (sexem_aine_1y_5 == 1 | enf_1y_5 == 0) &
       (sexem_aine_1y_6 == 1 | enf_1y_6 == 0) &
       (sexem_aine_1y_7 == 1 | enf_1y_7 == 0) &
       (sexem_aine_1y_8 == 1 | enf_1y_8 == 0) &
       (sexem_aine_1y_9 == 1 | enf_1y_9 == 0) &
       (sexem_aine_1y_10 == 1 | enf_1y_10 == 0) &
       (sexem_aine_1y_11 == 1 | enf_1y_11 == 0) &
       (sexem_aine_1y_12 == 1 | enf_1y_12 == 0) &
       (sexem_aine_1y_13 == 1 | enf_1y_13 == 0), comp_sex_aines_1y:= 1] # Que des frères

base[(sexef_aine_1y_4 == 1 | enf_1y_4 == 0) &
       (sexef_aine_1y_5 == 1 | enf_1y_5 == 0) &
       (sexef_aine_1y_6 == 1 | enf_1y_6 == 0) &
       (sexef_aine_1y_7 == 1 | enf_1y_7 == 0) &
       (sexef_aine_1y_8 == 1 | enf_1y_8 == 0) &
       (sexef_aine_1y_9 == 1 | enf_1y_9 == 0) &
       (sexef_aine_1y_10 == 1 | enf_1y_10 == 0) &
       (sexef_aine_1y_11 == 1 | enf_1y_11 == 0) &
       (sexef_aine_1y_12 == 1 | enf_1y_12 == 0) &
       (sexef_aine_1y_13 == 1 | enf_1y_13 == 0), comp_sex_aines_1y:= 2] # Que des sœurs

base[((sexem_aine_1y_4 == 1 | sexef_aine_1y_4 == 1) | 
        (sexem_aine_1y_5 == 1 | sexef_aine_1y_5 == 1) | 
        (sexem_aine_1y_6 == 1 | sexef_aine_1y_6 == 1) | 
        (sexem_aine_1y_7 == 1 | sexef_aine_1y_7 == 1) | 
        (sexem_aine_1y_8 == 1 | sexef_aine_1y_8 == 1) | 
        (sexem_aine_1y_9 == 1 | sexef_aine_1y_9 == 1) | 
        (sexem_aine_1y_10 == 1 | sexef_aine_1y_10 == 1) | 
        (sexem_aine_1y_11 == 1 | sexef_aine_1y_11 == 1) | 
        (sexem_aine_1y_12 == 1 | sexef_aine_1y_12 == 1) | 
        (sexem_aine_1y_13 == 1 | sexef_aine_1y_13 == 1)) & 
       comp_sex_aines_1y %nin% c(1, 2), comp_sex_aines_1y:= 3] # Aînés mixte

base[aine_1y_4 == 0 & 
       aine_1y_5 == 0 &
       aine_1y_6 == 0 & 
       aine_1y_7 == 0 & 
       aine_1y_8 == 0 & 
       aine_1y_9 == 0 & 
       aine_1y_10 == 0 & 
       aine_1y_11 == 0 & 
       aine_1y_12 == 0 & 
       aine_1y_13 == 0, comp_sex_aines_1y:= 4] # Pas d'aînés

base[, nb_aines_1y:= aine_1y_4 + aine_1y_5 + aine_1y_6 + aine_1y_7 + aine_1y_8 + aine_1y_9 + aine_1y_10 + aine_1y_11 + aine_1y_12 + aine_1y_13]

base[nb_aines_1y == 0, comp_sex_aines3_1y:= 1] # Pas d'aîné
base[comp_sex_aines_1y == 1 & nb_aines_1y == 1, comp_sex_aines3_1y:= 2] # 1 aîné
base[comp_sex_aines_1y == 2 & nb_aines_1y == 1, comp_sex_aines3_1y:= 3] # 1 aînée
base[comp_sex_aines_1y == 1 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 4] # 2 aînés ou +
base[comp_sex_aines_1y == 2 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 5] # 2 aînées ou +
base[comp_sex_aines_1y == 3 & nb_aines_1y >= 2, comp_sex_aines3_1y:= 6] # Aînés mixte (2 ou +)

base[, sibs_d:= comp_sex_aines3_1y]

base[sibs_d == 1, sibs:= 0] # no older siblings

base[sibs_d == 2 & sex == 1, sibs:= 1] # 1 older sib, same sex
base[sibs_d == 3 & sex == 2, sibs:= 1] # same

base[sibs_d == 4 & sex == 1, sibs:= 2] # 2+ older sibs, same sex
base[sibs_d == 5 & sex == 2, sibs:= 2] # same

base[sibs_d == 2 & sex == 2, sibs:= 3] # 1 older sib, different sex
base[sibs_d == 3 & sex == 1, sibs:= 3] # same

base[sibs_d == 4 & sex == 2, sibs:= 4] # 2+ older sibs, different sex
base[sibs_d == 5 & sex == 1, sibs:= 4] # same

base[sibs_d == 6, sibs:= 5] # 2+ older sibs, mixed 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Childcare ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# care1y care2y care3y: 1 Home 2 Childminder 3 Collective 4 Other
# care2y_home care2y_chmind care2y_collec care2y_oth care3y_home care3y_chmind care3y_collec care3y_oth: 0 No 1 Yes
#
# mcs
# care9m care3y
# care3y_home care3y_chmind care3y_collec care3y_oth: 0 No 1 Yes

# Elfe

base <- combine("A01M_GARDENF", "A01P_GARDENF", "A01C_GARDENF")
base[A01C_GARDENF %in% c(1, 2, 3, 4, 5, 8), care1y:= 1] 
base[A01C_GARDENF == 6, care1y:= 2] 
base[A01C_GARDENF == 7, care1y:= 3] 
base[A01C_GARDENF == 9, care1y:= 4]

base <- combine("A02M_GARDENF", "A02P_GARDENF", "A02C_GARDENF")
base[A02C_GARDENF %in% c(1, 2, 3, 4, 5, 8), care2y:= 1] 
base[A02C_GARDENF == 6, care2y:= 2] 
base[A02C_GARDENF == 7, care2y:= 3] 
base[A02C_GARDENF == 9, care2y:= 4]

base[A03R_GARDENF %in% c(1, 2, 3, 4, 5, 8), care3y:= 1] 
base[A03R_GARDENF == 6, care3y:= 2] 
base[A03R_GARDENF == 7, care3y:= 3] 
base[A03R_GARDENF %in% c(9, 10, 11), care3y:= 4]

base$care1y_home <- with(base, ifelse(care1y == 1, 1, 0))
base$care1y_chmind <- with(base, ifelse(care1y == 2, 1, 0))
base$care1y_collec <- with(base, ifelse(care1y == 3, 1, 0))
base$care1y_oth <- with(base, ifelse(care1y == 4, 1, 0))

base$care2y_home <- with(base, ifelse(care1y == 1 | care2y == 1, 1, 0))
base$care2y_chmind <- with(base, ifelse(care1y == 2 | care2y == 2, 1, 0))
base$care2y_collec <- with(base, ifelse(care1y == 3 | care2y == 3, 1, 0))
base$care2y_oth <- with(base, ifelse(care1y == 4 | care2y == 4, 1, 0))

base$care3y_home <- with(base, ifelse(care1y == 1 | care2y == 1 | care3y == 1, 1, 0))
base$care3y_chmind <- with(base, ifelse(care1y == 2 | care2y == 2 | care3y == 2, 1, 0))
base$care3y_collec <- with(base, ifelse(care1y == 3 | care2y == 3 | care3y == 3, 1, 0))
base$care3y_oth <- with(base, ifelse(care1y == 4 | care2y == 4 | care3y == 4, 1, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Family structure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# elfe
# lonemum2m lonemum1y lonemum2y lonemum3y: 0 No 1 Yes
#
# mcs
# lonemum9m lonemum3y lonemum5y: 0 No 1 Yes

# Elfe

base$lonemum2m <- with(base, ifelse(Child_hhld_2m == 2, 1, 0))
base$lonemum1y <- with(base, ifelse(child_hhld_1y == 2, 1, 0))
base$lonemum2y <- with(base, ifelse(child_hhld_2y == 2, 1, 0))
base$lonemum3y <- with(base, ifelse(child_hhld_2y == 2, 1, 0))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## School ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base <- combine("A02M_GARDENF", "A02P_GARDENF", "A02C_GARDENF")

base$school2y <- with(base, ifelse(A02C_GARDENF == 9, 1, 0))
base$school3y <- with(base, ifelse(A03R_SCOL %in% c(1, 2, 3), 1, 0))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fréquence et rythme des soins, conversion vers une seule unité ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# BAINS A 1 AN ET 3,5 ANS

# table(base$A01M_FQBAIN, base$A01M_RBAIN)
# 
#      0    1    2    3    4    5    6    7   10   11   12   13   14   23   30   74
# 1    2 8634  409   38   13    3    0   37    0    1    1    3    2    1    1    0 # par jour
# 2    0   48  372 3357  655  130   36  253    1    0    0    0    0    0    0    1 # par semaine
# 3    0    0    1    2    0    0    0    0    1    0    0    0    0    0    0    0 # par mois
# 253 enfants prennent 7 bains par semaine
# 8634 enfants prennent 1 bain par jour
# Ca revient au même : on va donc les mettre dans la même case
# Je prends pour unité semaine

base[A01M_FQBAIN == 1, m_bain_1y:= round(A01M_RBAIN * 7, digits = 0)]
base[A01M_FQBAIN == 2, m_bain_1y:= A01M_RBAIN]
base[A01M_FQBAIN == 3, m_bain_1y:= round(A01M_RBAIN / 4, digits = 0)]
base[m_bain_1y == 74, m_bain_1y:= 7]
base[m_bain_1y > 35, m_bain_1y:= 35]

# Choix :
# 74 bains par semaine requalifiés en 7 bains par semaine
# Je stoppe à 35 bains par semaine

# table(base$A03R_FREQBAIN, base$A03R_RBAIN)
# 
#       1    2    3    4    9
# 1  1214 2212  182    0    0
# 2    24 2191  230    0    0
# 3     4 1926   40    0    0
# 4     3  909   14    0    0

# 5     8  610    8    0    0
# 6     0  404    3    0    0
# 7     0   62    2    0    0
# 8     0    1    3    0    0

# 9     0    0    1    0    0
# 10    0    3    3    0    0
# 14    0    0    1    0    0
# 15    0    0    1    0    0
# 20    0    0    2    0    0
# 23    0    0    1    0    0
# 24    0    0    1    0    0
# 25    0    0    4    0    0
# 26    0    0    1    0    0
# 27    0    0    2    0    0
# 28    0    0    3    0    0

base[A03R_RBAIN == 1, r_bain_3y:= round(A03R_FREQBAIN * 7, digits = 0)]
base[A03R_RBAIN == 2, r_bain_3y:= A03R_FREQBAIN]
base[A03R_RBAIN == 3, r_bain_3y:= round(A03R_FREQBAIN / 4, digits = 0)]
base[r_bain_3y == 8, r_bain_3y:= 35]
base[r_bain_3y > 35, r_bain_3y:= 35]

base[A03R_RDOUCHE == 1, r_douche_3y:= round(A03R_FREQDOUCH * 7, digits = 0)]
base[A03R_RDOUCHE == 2, r_douche_3y:= A03R_FREQDOUCH]
base[A03R_RDOUCHE == 3, r_douche_3y:= round(A03R_FREQDOUCH / 4, digits = 0)]
base[r_douche_3y == 8, r_douche_3y:= 35]
base[r_douche_3y > 35, r_douche_3y:= 35]

base[r_douche_3y > 35, r_DOUCHBAIN_3y:= 35]

base$r_douchebain_3y <- base$r_douche_3y + base$r_bain_3y

# DOUCHES A 3,5 ANS

# table(A03R_FREQDOUCH, A03R_RDOUCHE)
# Manque A03R_FREQDOUCH

# SHAMPOOINGS A 1 AN ET 3,5 ANS

# > table(base$A01M_FQCHEV, base$A01M_RCHEV)
# 
#      1    2    3    4    5    6    7    8   10   11   13   30   53
# 1 6572  192   39   13    2    0   29    0    0    1    1    1    0
# 2  336  938 4721  793  127   24  180    0    1    0    0    0    1
# 3    2    9    5    0    0    0    1    1    0    0    0    0    0
# 6    0    0    0    0    0    0    0    0    0    0    0    0    0

base[A01M_FQCHEV == 1, m_shamp_1y:= round(A01M_RCHEV * 7, digits = 0)]
base[A01M_FQCHEV == 2, m_shamp_1y:= A01M_RCHEV]
base[A01M_FQCHEV == 3, m_shamp_1y:= round(A01M_RCHEV / 4, digits = 0)]
base[m_shamp_1y > 14, m_shamp_1y:= 14]
base[m_shamp_1y == 30, m_shamp_1y:= 3]
base[m_shamp_1y == 54, m_shamp_1y:= 5]

# Choix :
# 53 shamp par semaine requalifiés en 5 par semaine, 30 en 3
# Je borne à 14 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# > table(base$A03R_FQCHEV, base$A03R_RCHEV)
# 
#       1    2    3    4    9
# 1  2286 1485   21    0    0
# 2    70 3010   47    0    0
# 3     0 3928   16    0    0
# 4     0  739    4    0    0
# 5     0  165    0    0    0
# 6     0   54    4    0    0
# 7     0  132    2    0    0
# 8     0    0    1    0    0
# 10    0    1    2    0    0
# 12    0    2    0    0    0
# 14    0    0    1    0    0
# 21    0    0    1    0    0

base[A01M_FQCHEV == 1, r_shamp_3y:= round(A01M_RCHEV * 7, digits = 0)]
base[A01M_FQCHEV == 2, r_shamp_3y:= A01M_RCHEV]
base[A01M_FQCHEV == 3, r_shamp_3y:= round(A01M_RCHEV / 4, digits = 0)]
base[r_shamp_3y > 14, r_shamp_3y:= 14]
base[r_shamp_3y == 30, r_shamp_3y:= 3]
base[r_shamp_3y == 54, r_shamp_3y:= 5]

base[A03R_RCHEV == 3 & A03R_FQCHEV %in% c(1, 2), r_shamp_3y:= 0]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 1 | (A03R_RCHEV == 3 & A03R_FQCHEV %in% c(3, 4, 6)), r_shamp_3y:= 1]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 2 | (A03R_RCHEV == 3 & A03R_FQCHEV %in% c(7, 10)), r_shamp_3y:= 2]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 3 | (A03R_RCHEV == 3 & A03R_FQCHEV == 14), r_shamp_3y:= 3]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 4, r_shamp_3y:= 4]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 5 | (A03R_RCHEV == 3 & A03R_FQCHEV == 21), r_shamp_3y:= 5]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 6, r_shamp_3y:= 6]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 7, r_shamp_3y:= 7]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 10, r_shamp_3y:= 10]
base[A03R_RCHEV == 2 & A03R_FQCHEV == 12 | (A03R_RCHEV == 1 & A03R_FQCHEV == 2), r_shamp_3y:= 14]

# COUPE D'ONGLES A 1 AN ET 3,5 ANS

# > table(base$A01M_FQONG, base$A01M_RONG)
# 
#      0    1    2    3    4    5    6    7    8    9   10   12   13   15   23   25   30
# 1    0   45   10    6    0    1    0    2    0    0    3    0    0    1    0    0    1
# 2    0 8523 1287  450   37    1    1    0    0    0    1    1    1    1    3    1    0
# 3    1  647 2286  450   47    4    4    4    2    1    4    0    0    2    0    0    0
# 6    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0

base[A01M_FQONG == 3 & A01M_RONG %in% c(1, 2), m_ongles_1y:= 0]
base[A01M_FQONG == 2 & A01M_RONG == 1 | (A01M_FQONG == 3 & A01M_RONG %in% c(3, 4, 5, 6)), m_ongles_1y:= 1]
base[A01M_FQONG == 2 & A01M_RONG == 2 | (A01M_FQONG == 3 & A01M_RONG == 10), m_ongles_1y:= 2]
base[A01M_FQONG == 2 & A01M_RONG == 3 | (A01M_FQONG == 3 & A01M_RONG %in% c(12, 13)), m_ongles_1y:= 3]
base[A01M_FQONG == 2 & A01M_RONG == 4 | (A01M_FQONG == 3 & A01M_RONG == 15), m_ongles_1y:= 4]
base[A01M_FQONG == 2 & A01M_RONG == 5, m_ongles_1y:= 5]
base[A01M_FQONG == 2 & A01M_RONG == 6 | (A01M_FQONG == 3 & A01M_RONG %in% c(23, 25)), m_ongles_1y:= 6]
base[A01M_FQONG == 2 & A01M_RONG >= 7 | (A01M_FQONG == 1 & A01M_RONG >= 1), m_ongles_1y:= 7]

# Choix :
# Je borne à 7 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# > table(base$A03R_FQONG, base$A03R_RONG)
# 
#       1    2    3    4    8    9
# 1    27 5872 1166    0    0    0
# 2     0  507 3293    0    0    0
# 3     0  112  620    0    0    0
# 4     0    9   28    0    0    0
# 5     0    4    5    0    0    0
# 6     0    0    3    0    0    0
# 7     0    1    1    0    0    0
# 8     0    0    1    0    0    0
# 12    0    0    1    0    0    0
# 15    0    0    1    0    0    0

base[A03R_RONG == 3 & A03R_FQONG %in% c(1, 2), r_ongles_3y:= 0]
base[A03R_RONG == 2 & A03R_FQONG == 1 | (A03R_RONG == 3 & A03R_FQONG %in% c(3, 4, 5, 6)), r_ongles_3y:= 1]
base[A03R_RONG == 2 & A03R_FQONG == 2 | (A03R_RONG == 3 & A03R_FQONG %in% c(7, 8)), r_ongles_3y:= 2]
base[A03R_RONG == 2 & A03R_FQONG == 3 | (A03R_RONG == 3 & A03R_FQONG == 12), r_ongles_3y:= 3]
base[A03R_RONG == 2 & A03R_FQONG == 4 | (A03R_RONG == 3 & A03R_FQONG == 15), r_ongles_3y:= 4]
base[A03R_RONG == 2 & A03R_FQONG == 5, r_ongles_3y:= 5]
base[A03R_RONG == 2 & A03R_FQONG == 6, r_ongles_3y:= 6]
base[A03R_RONG == 2 & A03R_FQONG == 7 | (A03R_RONG == 1 & A03R_FQONG == 1), r_ongles_3y:= 7]

# LAVAGE DE DENTS A 1 AN ET 3,5 ANS

# > table(base$A01M_FQDENT, base$A01M_RDENT)
# 
#      0    1    2    3    4    5    6    7    8    9   10   15
# 1    4 1939  242   42    6    0    0    8    0    0    1    1
# 2    2  729  329  551   94   32    3   48    0    0    1    0
# 3    5  160  102   21    7    3    2    0    1    1    0    0
# 6    0    0    0    0    0    0    0    0    0    0    0    0
# 8    0    0    0    0    0    0    0    0    0    0    0    0

base[A01M_RDENT == 0 | (A01M_FQDENT == 3 & A01M_RDENT %in% c(1, 2)), m_dents_1y:= 0]
base[A01M_FQDENT == 2 & A01M_RDENT == 1 | (A01M_FQDENT == 3 & A01M_RDENT %in% c(3, 4, 5, 6)), m_dents_1y:= 1]
base[A01M_FQDENT == 2 & A01M_RDENT == 2 | (A01M_FQDENT == 3 & A01M_RDENT %in% c(8, 9)), m_dents_1y:= 2]
base[A01M_FQDENT == 2 & A01M_RDENT == 3, m_dents_1y:= 3]
base[A01M_FQDENT == 2 & A01M_RDENT == 4, m_dents_1y:= 4]
base[A01M_FQDENT == 2 & A01M_RDENT == 5, m_dents_1y:= 5]
base[A01M_FQDENT == 2 & A01M_RDENT == 6, m_dents_1y:= 6]
base[A01M_FQDENT == 2 & A01M_RDENT == 7 | (A01M_FQDENT == 1 & A01M_RDENT == 1), m_dents_1y:= 7]
base[A01M_FQDENT == 2 & A01M_RDENT == 10, m_dents_1y:= 10]
base[A01M_FQDENT == 1 & A01M_RDENT == 2, m_dents_1y:= 14]
base[A01M_FQDENT == 1 & A01M_RDENT == 3, m_dents_1y:= 21]
base[A01M_FQDENT == 1 & A01M_RDENT == 4, m_dents_1y:= 28]
base[A01M_FQDENT == 1 & A01M_RDENT >= 5, m_dents_1y:= 35]

# Par jour
base[m_dents_1y == 0, m_dents_j_1y:= 0]
base[m_dents_1y %in% c(1, 2, 3), m_dents_j_1y:= 0.5]
base[m_dents_1y %in% c(4, 5, 6, 7), m_dents_j_1y:= 1]
base[m_dents_1y == 14, m_dents_j_1y:= 2]
base[m_dents_1y == 21, m_dents_j_1y:= 3]
base[m_dents_1y == 28, m_dents_j_1y:= 4]
base[m_dents_1y == 35, m_dents_j_1y:= 5]

# Choix :
# Je borne à 35 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# > table(base$A03R_FQDENT, base$A03R_RDENT)
# 
#       1    2    3    4    8    9
# 1  5062  126   11    0    0    0
# 2  5510  139   12    0    0    0
# 3   360  352    6    0    0    0
# 4     8  163    0    0    0    0
# 5     8   76    0    0    0    0
# 6     0   11    2    0    0    0
# 7     0   68    0    0    0    0
# 10    0    2    1    0    0    0
# 11    0    1    0    0    0    0
# 14    0    2    0    0    0    0

base[A03R_RDENT == 3 & A03R_FQDENT %in% c(1, 2), r_dents_3y:= 0]
base[A03R_RDENT == 2 & A03R_FQDENT == 1 | (A03R_RDENT == 3 & A03R_FQDENT %in% c(3, 4, 5, 6)), r_dents_3y:= 1]
base[A03R_RDENT == 2 & A03R_FQDENT == 2 | (A03R_RDENT == 3 & A03R_FQDENT == 10), r_dents_3y:= 2]
base[A03R_RDENT == 2 & A03R_FQDENT == 3, r_dents_3y:= 3]
base[A03R_RDENT == 2 & A03R_FQDENT == 4, r_dents_3y:= 4]
base[A03R_RDENT == 2 & A03R_FQDENT == 5, r_dents_3y:= 5]
base[A03R_RDENT == 2 & A03R_FQDENT == 6, r_dents_3y:= 6]
base[A03R_RDENT == 2 & A03R_FQDENT == 7 | (A03R_RDENT == 1 & A03R_FQDENT == 1), r_dents_3y:= 7]
base[A03R_RDENT == 2 & A03R_FQDENT == 10, r_dents_3y:= 10]
base[A03R_RDENT == 2 & A03R_FQDENT == 11, r_dents_3y:= 11]
base[A03R_RDENT == 2 & A03R_FQDENT == 14 | (A03R_RDENT == 1 & A03R_FQDENT == 2), r_dents_3y:= 14]
base[A03R_RDENT == 1 & A03R_FQDENT == 3, r_dents_3y:= 21]
base[A03R_RDENT == 1 & A03R_FQDENT == 4, r_dents_3y:= 28]
base[A03R_RDENT == 1 & A03R_FQDENT == 5, r_dents_3y:= 35]

# Par jour
base[r_dents_3y == 0, r_dents_j_3y:= 0]
base[r_dents_3y %in% c(1, 2, 3), r_dents_j_3y:= 0.5]
base[r_dents_3y %in% c(4, 5, 6, 7), r_dents_j_3y:= 1]
base[r_dents_3y == 14, r_dents_j_3y:= 2]
base[r_dents_3y == 21, r_dents_j_3y:= 3]
base[r_dents_3y == 28, r_dents_j_3y:= 4]
base[r_dents_3y == 35, r_dents_j_3y:= 5]

# LAVAGE DES SOUS-VETEMENTS A 2 ANS

# table(base$A02M_FQCHGSVN, base$A02M_FQCHGSVP)
# 
#       1    2    3    9
# 0     2    0    0    0
# 1  9455   30    0    0
# 2  1249   83    0    0
# 3   143  436    0    0
# 4    36  393    0    0
# 5    25   46    0    0
# 6    10   10    0    0
# 7     0   77    0    0
# 8     2    1    0    0
# 9     0    1    0    0
# 10    1    1    0    0
# 11    2    0    0    0
# 12    1    0    0    0
# 14    0    1    0    0
# 35    0    0    1    0

base[A02M_FQCHGSVN == 0, m_sousvet_2y:= 0]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 1, m_sousvet_2y:= 1]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 2, m_sousvet_2y:= 2]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 3, m_sousvet_2y:= 3]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 4, m_sousvet_2y:= 4]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 5, m_sousvet_2y:= 5]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 6, m_sousvet_2y:= 6]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 7 | (A02M_FQCHGSVP == 1 & A02M_FQCHGSVN == 1), m_sousvet_2y:= 7]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 8, m_sousvet_2y:= 8]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 9 | (A02M_FQCHGSVP == 3 & A02M_FQCHGSVN == 35), m_sousvet_2y:= 9]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 10, m_sousvet_2y:= 10]
base[A02M_FQCHGSVP == 2 & A02M_FQCHGSVN == 14 | (A02M_FQCHGSVP == 1 & A02M_FQCHGSVN == 2), m_sousvet_2y:= 14]
base[A02M_FQCHGSVP == 1 & A02M_FQCHGSVN == 3, m_sousvet_2y:= 21]
base[A02M_FQCHGSVP == 1 & A02M_FQCHGSVN == 4, m_sousvet_2y:= 28]
base[A02M_FQCHGSVP == 1 & A02M_FQCHGSVN == 5, m_sousvet_2y:= 35]
base[A02M_FQCHGSVP == 1 & A02M_FQCHGSVN >= 6, m_sousvet_2y:= 42]

# LAVAGE DES AUTRES VETEMENTS A 2 ANS

# > table(base$A02M_FQCHGVETN, base$A02M_FQCHGVETP)
# 
#       1    2    3    9
# 0     2    0    0    0
# 1  7411   99    0    0
# 2   602  310    1    0
# 3    77 1829    2    0
# 4    17 1323    1    0
# 5     4  224    0    0
# 6     1   32    0    0
# 7     2   57    0    0
# 10    1    0    0    0
# 11    3    0    0    0
# 12    1    0    0    0
# 13    2    0    0    0
# 14    1    2    0    0
# 23    1    0    0    0
# 40    0    0    1    0
# 43    0    1    0    0
# 74    0    1    0    0


base[A02M_FQCHGVETN == 0 | (A02M_FQCHGVETP == 3 & A02M_FQCHGVETN == 2), m_vet_2y:= 0]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 1 | (A02M_FQCHGVETP == 3 & A02M_FQCHGVETN %in% c(3, 4)), m_vet_2y:= 1]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 2, m_vet_2y:= 2]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 3, m_vet_2y:= 3]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 4, m_vet_2y:= 4]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 5, m_vet_2y:= 5]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 6, m_vet_2y:= 6]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN %in% c(7, 74) | (A02M_FQCHGVETP == 1 & A02M_FQCHGVETN == 1), m_vet_2y:= 7]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 14 | (A02M_FQCHGVETP == 1 & A02M_FQCHGVETN == 2), m_vet_2y:= 14]
base[A02M_FQCHGVETP == 1 & A02M_FQCHGVETN == 3, m_vet_2y:= 21]
base[A02M_FQCHGVETP == 1 & A02M_FQCHGVETN == 4, m_vet_2y:= 28]
base[A02M_FQCHGVETP == 1 & A02M_FQCHGVETN == 5, m_vet_2y:= 35]
base[A02M_FQCHGVETP == 2 & A02M_FQCHGVETN == 43 | (A02M_FQCHGVETP == 1 & A02M_FQCHGVETN >= 6) | (A02M_FQCHGVETP == 3 & A02M_FQCHGVETN == 40), m_vet_2y:= 42]

# PERE

# table(base$A01P_FQBAIN, base$A01P_RBAIN) BAINS A 1 AN
# 
#     1   2   3   4   5   7
# 1 114  13   3   0   0   0
# 2   5   5  25   8   1   1

base[A01P_FQBAIN == 2 & A01P_RBAIN == 1, p_bain_1y:= 1]
base[A01P_FQBAIN == 2 & A01P_RBAIN == 2, p_bain_1y:= 2]
base[A01P_FQBAIN == 2 & A01P_RBAIN == 3, p_bain_1y:= 3]
base[A01P_FQBAIN == 2 & A01P_RBAIN == 4, p_bain_1y:= 4]
base[A01P_FQBAIN == 2 & A01P_RBAIN == 5, p_bain_1y:= 5]
base[A01P_FQBAIN == 2 & A01P_RBAIN == 7 | (A01P_FQBAIN == 1 & A01P_RBAIN == 1), p_bain_1y:= 7]
base[A01P_FQBAIN == 1 & A01P_RBAIN == 2, p_bain_1y:= 14]
base[A01P_FQBAIN == 1 & A01P_RBAIN == 3, p_bain_1y:= 21]

# Je stoppe à 35 bains par semaine

# > table(base$A01P_FQCHEV, base$A01P_RCHEV) SHAMPOOINGS A 1 AN 
# 
#    1  2  3  4  5  7
# 1 96  8  3  0  0  0
# 2 11  8 39  7  1  1
# 6  0  0  0  0  0  0

base[A01P_FQCHEV == 6, p_shamp_1y:= 0]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 1, p_shamp_1y:= 1]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 2, p_shamp_1y:= 2]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 3, p_shamp_1y:= 3]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 4, p_shamp_1y:= 4]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 5, p_shamp_1y:= 5]
base[A01P_FQCHEV == 2 & A01P_RCHEV == 7 | (A01P_FQCHEV == 1 & A01P_RCHEV == 1), p_shamp_1y:= 7]
base[A01P_FQCHEV == 1 & A01P_RCHEV >= 2, p_shamp_1y:= 14]

# Je borne à 14 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# > table(base$A01P_FQONG, base$A01P_RONG) COUPE D'ONGLES A 1 AN
# 
#    0  1  2  3
# 1  0  0  1  0
# 2  0 69 10  5
# 3  1 11 41  7
# 6  0  0  0  0
# 9  0  0  0  0

base[A01P_FQONG == 3 & A01P_RONG %in% c(0, 1, 2), p_ongles_1y:= 0]
base[A01P_FQONG == 2 & A01P_RONG == 1 | (A01P_FQONG == 3 & A01P_RONG == 3), p_ongles_1y:= 1]
base[A01P_FQONG == 2 & A01P_RONG == 2, p_ongles_1y:= 2]
base[A01P_FQONG == 2 & A01P_RONG == 3, p_ongles_1y:= 3]
base[A01P_FQONG == 1 & A01P_RONG == 2, p_ongles_1y:= 7]

# Je borne à 7 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# > table(base$A01P_FQDENT, base$A01P_RDENT) LAVAGE DE DENTS A 1 AN
# 
#    1  2  3  4
# 1 43  3  3  0
# 2 11  2  8  1
# 3  0  4  0  0
# 6  0  0  0  0
# 9  0  0  0  0

base[A01P_FQDENT == 3 & A01P_RDENT == 2, p_dents_1y:= 0]
base[A01P_FQDENT == 2 & A01P_RDENT == 1, p_dents_1y:= 1]
base[A01P_FQDENT == 2 & A01P_RDENT == 2, p_dents_1y:= 2]
base[A01P_FQDENT == 2 & A01P_RDENT == 3, p_dents_1y:= 3]
base[A01P_FQDENT == 2 & A01P_RDENT == 4, p_dents_1y:= 4]
base[A01P_FQDENT == 1 & A01P_RDENT == 1, p_dents_1y:= 7]
base[A01P_FQDENT == 1 & A01P_RDENT == 2, p_dents_1y:= 14]
base[A01P_FQDENT == 1 & A01P_RDENT == 3, p_dents_1y:= 21]

# Par jour
base[p_dents_1y == 0, p_dents_j_1y:= 0]
base[p_dents_1y %in% c(1, 2, 3), p_dents_j_1y:= 0.5]
base[p_dents_1y %in% c(4, 5, 6, 7), p_dents_j_1y:= 1]
base[p_dents_1y == 14, p_dents_j_1y:= 2]
base[p_dents_1y == 21, p_dents_j_1y:= 3]
base[p_dents_1y == 28, p_dents_j_1y:= 4]
base[p_dents_1y == 35, p_dents_j_1y:= 5]

# Je borne à 35 par semaine comme c'est le cas à 3,5 ans dans l'enquête

# table(base$A02P_FQCHGSVN, base$A02P_FQCHGSVP) LAVAGE DES SOUS-VETEMENTS A 2 ANS
# 
#       1    2    3    8    9
# 1  7871   22    1    0    0
# 2  1126   83    1    0    0
# 3   175  427    0    0    0
# 4    43  316    0    0    0
# 5    21   43    0    0    0
# 6     5    7    0    0    0
# 7     0   68    0    0    0
# 9     1    0    0    0    0
# 10    2    1    0    0    0
# 13    3    0    0    0    0
# 14    1    1    0    0    0
# 23    1    0    0    0    0
# 43    0    1    0    0    0
# 44    1    0    0    0    0

base[A02P_FQCHGSVP == 3 & A02P_FQCHGSVN %in% c(1, 2), p_sousvet_2y:= 0]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 1, p_sousvet_2y:= 1]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 2, p_sousvet_2y:= 2]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 3, p_sousvet_2y:= 3]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 4, p_sousvet_2y:= 4]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 5, p_sousvet_2y:= 5]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 6, p_sousvet_2y:= 6]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 7 | (A02P_FQCHGSVP == 1 & A02P_FQCHGSVN == 1), p_sousvet_2y:= 7]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 10, p_sousvet_2y:= 10]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 14 | (A02P_FQCHGSVP == 1 & A02P_FQCHGSVN == 2), p_sousvet_2y:= 14]
base[A02P_FQCHGSVP == 1 & A02P_FQCHGSVN == 3, p_sousvet_2y:= 21]
base[A02P_FQCHGSVP == 1 & A02P_FQCHGSVN == 4, p_sousvet_2y:= 28]
base[A02P_FQCHGSVP == 1 & A02P_FQCHGSVN == 5, p_sousvet_2y:= 35]
base[A02P_FQCHGSVP == 2 & A02P_FQCHGSVN == 43 | (A02P_FQCHGSVP == 1 & A02P_FQCHGSVN >= 6), p_sousvet_2y:= 42]

# > table(base$A02P_FQCHGVETN, base$A02P_FQCHGVETP) LAVAGE DES AUTRES VETEMENTS A 2 ANS
# 
#       1    2    3    8    9
# 0     1    0    0    0    0
# 1  5605  119    1    0    0
# 2   465  406    5    0    0
# 3    54 2074    0    0    0
# 4    14 1238    0    0    0
# 5     4  149    0    0    0
# 6     2   14    0    0    0
# 7     0   39    0    0    0
# 10    0    2    0    0    0
# 13    3    0    0    0    0
# 14    1    0    0    0    0
# 15    0    0    1    0    0
# 23    0    1    0    0    0
# 43    0    2    0    0    0
# 88    0    1    0    0    0

base[A02P_FQCHGVETN == 0 | (A02P_FQCHGVETP == 3 & A02P_FQCHGVETN %in% c(1, 2)), p_vet_2y:= 0]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 1 | (A02P_FQCHGVETP == 3 & A02P_FQCHGVETN %in% c(3, 4)), p_vet_2y:= 1]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 2, p_vet_2y:= 2]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 3, p_vet_2y:= 3]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 4 | (A02P_FQCHGVETP == 3 & A02P_FQCHGVETN == 15), p_vet_2y:= 4]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 5, p_vet_2y:= 5]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 6, p_vet_2y:= 6]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 7 | (A02P_FQCHGVETP == 1 & A02P_FQCHGVETN == 1), p_vet_2y:= 7]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 10, p_vet_2y:= 10]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 14 | (A02P_FQCHGVETP == 1 & A02P_FQCHGVETN == 2), p_vet_2y:= 14]
base[A02P_FQCHGVETP == 1 & A02P_FQCHGVETN == 3 | (A02P_FQCHGVETP == 2 & A02P_FQCHGVETN == 23), p_vet_2y:= 21]
base[A02P_FQCHGVETP == 1 & A02P_FQCHGVETN == 4, p_vet_2y:= 28]
base[A02P_FQCHGVETP == 1 & A02P_FQCHGVETN == 5, p_vet_2y:= 35]
base[A02P_FQCHGVETP == 2 & A02P_FQCHGVETN >= 43 | (A02P_FQCHGVETP == 1 & A02P_FQCHGVETN >= 6), p_vet_2y:= 42]

# Fréquence des soins, par semaine, agrégation

base[m_sousvet_2y < 7, m_sousvet_2y_CAT:= 0]
base[m_sousvet_2y == 7, m_sousvet_2y_CAT:= 1]
base[m_sousvet_2y > 7, m_sousvet_2y_CAT:= 2]

base[p_sousvet_2y < 7, p_sousvet_2y_CAT:= 0]
base[p_sousvet_2y == 7, p_sousvet_2y_CAT:= 1]
base[p_sousvet_2y > 7, p_sousvet_2y_CAT:= 2]

base[m_vet_2y < 7, m_vet_2y_CAT:= 0]
base[m_vet_2y == 7, m_vet_2y_CAT:= 1]
base[m_vet_2y > 7, m_vet_2y_CAT:= 2]

base[p_vet_2y < 7, p_vet_2y_CAT:= 0]
base[p_vet_2y == 7, p_vet_2y_CAT:= 1]
base[p_vet_2y > 7, p_vet_2y_CAT:= 2]

base[, c_bain_1y:= m_bain_1y]
base[is.na(m_bain_1y) & !is.na(p_bain_1y), c_bain_1y:= p_bain_1y]

base[, c_shamp_1y:= m_shamp_1y]
base[is.na(m_shamp_1y) & !is.na(p_shamp_1y), c_shamp_1y:= p_shamp_1y]

base[, c_ongles_1y:= m_ongles_1y]
base[is.na(m_ongles_1y) & !is.na(p_ongles_1y), c_ongles_1y:= p_ongles_1y]

base[, c_dents_1y:= m_dents_1y]
base[is.na(m_dents_1y) & !is.na(p_dents_1y), c_dents_1y:= p_dents_1y]

base[, c_dents_j_1y:= m_dents_j_1y]
base[is.na(m_dents_j_1y) & !is.na(p_dents_j_1y), c_dents_j_1y:= p_dents_j_1y]

base[, c_sousvet_2y:= m_sousvet_2y]
base[is.na(m_sousvet_2y) & !is.na(p_sousvet_2y), c_sousvet_2y:= p_sousvet_2y]

base[, c_vet_2y:= m_vet_2y]
base[is.na(m_vet_2y) & !is.na(p_vet_2y), c_vet_2y:= p_vet_2y]


base[c_bain_1y %in% c(1, 2), c_bain_1y_cat:= 1]
base[c_bain_1y %in% c(3, 4, 5), c_bain_1y_cat:= 2]
base[c_bain_1y %in% c(6, 7), c_bain_1y_cat:= 3]
base[c_bain_1y > 7, c_bain_1y_cat:= 4]

base[c_shamp_1y %in% c(1, 2), c_shamp_1y_cat:= 1]
base[c_shamp_1y %in% c(3, 4, 5), c_shamp_1y_cat:= 2]
base[c_shamp_1y %in% c(6, 7), c_shamp_1y_cat:= 3]
base[c_shamp_1y > 7, c_shamp_1y_cat:= 4]

base[c_ongles_1y == 0, c_ongles_1y_cat:= 1]
base[c_ongles_1y == 1, c_ongles_1y_cat:= 2]
base[c_ongles_1y > 1, c_ongles_1y_cat:= 3]

base[c_dents_1y < 7, c_dents_1y_cat:= 1]
base[c_dents_1y == 7, c_dents_1y_cat:= 2]
base[c_dents_1y > 7, c_dents_1y_cat:= 3]

base[r_douchebain_3y %in% c(1, 2), r_bain_3y_cat:= 1]
base[r_douchebain_3y %in% c(3, 4, 5), r_bain_3y_cat:= 2]
base[r_douchebain_3y %in% c(6, 7), r_bain_3y_cat:= 3]
base[r_douchebain_3y > 7, r_bain_3y_cat:= 4]

base[r_shamp_3y %in% c(1, 2), r_shamp_3y_cat:= 1]
base[r_shamp_3y %in% c(3, 4, 5), r_shamp_3y_cat:= 2]
base[r_shamp_3y %in% c(6, 7), r_shamp_3y_cat:= 3]
base[r_shamp_3y > 7, r_shamp_3y_cat:= 4]

base[r_ongles_3y == 0, r_ongles_3y_cat:= 1]
base[r_ongles_3y == 1, r_ongles_3y_cat:= 2]
base[r_ongles_3y > 1, r_ongles_3y_cat:= 3]

base[r_dents_3y < 7, r_dents_3y_cat:= 1]
base[r_dents_3y == 7, r_dents_3y_cat:= 2]
base[r_dents_3y > 7, r_dents_3y_cat:= 3]

base[c_sousvet_2y < 7, c_sousvet_2y_cat:= 1]
base[c_sousvet_2y == 7, c_sousvet_2y_cat:= 2]
base[c_sousvet_2y > 7, c_sousvet_2y_cat:= 3]

base[c_vet_2y < 7, c_vet_2y_cat:= 1]
base[c_vet_2y == 7, c_vet_2y_cat:= 2]
base[c_vet_2y > 7, c_vet_2y_cat:= 3]



# Additive index

base$sc_bain1 <- recode(base$c_bain_1y_cat, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN) #1
base$sc_shamp1 <- recode(base$c_shamp_1y_cat, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN) #1
base$sc_ongles1 <- recode(base$c_ongles_1y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN)  #2
# base$sc_dents1 <- recode(base$c_dents_1y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN)
base$sc_bain3 <- recode(base$r_bain_3y_cat, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN) #1
base$sc_shamp3 <- recode(base$r_shamp_3y_cat, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .default = NaN) #1
base$sc_ongles3 <- recode(base$r_ongles_3y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN)  #2
base$sc_dents3 <- recode(base$r_dents_3y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN) #3
base$sc_sv2 <- recode(base$c_sousvet_2y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN) #3
base$sc_v2 <- recode(base$c_vet_2y_cat, `1` = 0, `2` = 1, `3` = 2, .default = NaN) #3

table(base$sc_bain1, useNA = "always")
table(base$sc_shamp1, useNA = "always")
table(base$sc_ongles1, useNA = "always")
# table(base$sc_dents1, useNA = "always")
table(base$sc_bain3, useNA = "always")
table(base$sc_shamp3, useNA = "always")
table(base$sc_ongles3, useNA = "always")
table(base$sc_dents3, useNA = "always")
table(base$sc_sv2, useNA = "always")
table(base$sc_v2, useNA = "always")

base$schyg <- base$sc_bain1 + base$sc_shamp1 + base$sc_ongles1 + base$sc_bain3 + base$sc_shamp3 + base$sc_ongles3 + base$sc_dents3 + base$sc_sv2 + base$sc_v2
base$schyg2 <- base$sc_bain1 + base$sc_shamp1 + base$sc_ongles1 + base$sc_bain3                   + base$sc_ongles3 + base$sc_dents3 + base$sc_sv2 + base$sc_v2

base$std_schyg <- base$schyg
base$std_schyg2 <- base$schyg2

base <- base %>% mutate_at(c('std_schyg'), ~(scale(.) %>% as.vector))
base <- base %>% mutate_at(c('std_schyg2'), ~(scale(.) %>% as.vector))

base <- as.data.frame(base)
basehyg <- base
save(basehyg, file = "C:/Users/sheridan_ale/Desktop/Sogenre/R/Data/Elfe/basehyg.Rdata")
rm("base")

