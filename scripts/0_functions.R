
############# SCRIPT 0 : Fonctions #############

lientyp <- function(i) {
  print(paste(paste0("M02M_LIENTYP_1", " "), sum(elfe$M02M_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_2", " "), sum(elfe$M02M_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_3", " "), sum(elfe$M02M_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_4", " "), sum(elfe$M02M_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_5", " "), sum(elfe$M02M_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_6", " "), sum(elfe$M02M_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_7", " "), sum(elfe$M02M_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_8", " "), sum(elfe$M02M_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_9", " "), sum(elfe$M02M_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_10"), sum(elfe$M02M_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_11"), sum(elfe$M02M_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_12"), sum(elfe$M02M_LIENTYP_12 == i, na.rm=TRUE)))
  
  print(paste(paste0("M02P_LIENTYP_1", " "), sum(elfe$M02P_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_2", " "), sum(elfe$M02P_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_3", " "), sum(elfe$M02P_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_4", " "), sum(elfe$M02P_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_5", " "), sum(elfe$M02P_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_6", " "), sum(elfe$M02P_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_7", " "), sum(elfe$M02P_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_8", " "), sum(elfe$M02P_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_9", " "), sum(elfe$M02P_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_10"), sum(elfe$M02P_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_11"), sum(elfe$M02P_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_12"), sum(elfe$M02P_LIENTYP_12 == i, na.rm=TRUE)))
  
  print(paste(paste0("A01M_LIENTYP_1", " "), sum(elfe$A01M_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_2", " "), sum(elfe$A01M_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_3", " "), sum(elfe$A01M_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_4", " "), sum(elfe$A01M_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_5", " "), sum(elfe$A01M_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_6", " "), sum(elfe$A01M_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_7", " "), sum(elfe$A01M_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_8", " "), sum(elfe$A01M_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_9", " "), sum(elfe$A01M_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_10"), sum(elfe$A01M_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_11"), sum(elfe$A01M_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_12"), sum(elfe$A01M_LIENTYP_12 == i, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_13"), sum(elfe$A01M_LIENTYP_13 == i, na.rm=TRUE)))
  
  print(paste(paste0("A01P_LIENTYP_1", " "), sum(elfe$A01P_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_2", " "), sum(elfe$A01P_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_3", " "), sum(elfe$A01P_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_4", " "), sum(elfe$A01P_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_5", " "), sum(elfe$A01P_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_6", " "), sum(elfe$A01P_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_7", " "), sum(elfe$A01P_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_8", " "), sum(elfe$A01P_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_9", " "), sum(elfe$A01P_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_10"), sum(elfe$A01P_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_11"), sum(elfe$A01P_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_12"), sum(elfe$A01P_LIENTYP_12 == i, na.rm=TRUE)))
  
  print(paste(paste0("A02M_LIENTYP_1", " "), sum(elfe$A02M_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_2", " "), sum(elfe$A02M_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_3", " "), sum(elfe$A02M_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_4", " "), sum(elfe$A02M_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_5", " "), sum(elfe$A02M_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_6", " "), sum(elfe$A02M_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_7", " "), sum(elfe$A02M_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_8", " "), sum(elfe$A02M_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_9", " "), sum(elfe$A02M_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_10"), sum(elfe$A02M_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_11"), sum(elfe$A02M_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_12"), sum(elfe$A02M_LIENTYP_12 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_13"), sum(elfe$A02M_LIENTYP_13 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_14"), sum(elfe$A02M_LIENTYP_14 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_15"), sum(elfe$A02M_LIENTYP_15 == i, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_16"), sum(elfe$A02M_LIENTYP_16 == i, na.rm=TRUE)))
  
  print(paste(paste0("A02P_LIENTYP_1", " "), sum(elfe$A02P_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_2", " "), sum(elfe$A02P_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_3", " "), sum(elfe$A02P_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_4", " "), sum(elfe$A02P_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_5", " "), sum(elfe$A02P_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_6", " "), sum(elfe$A02P_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_7", " "), sum(elfe$A02P_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_8", " "), sum(elfe$A02P_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_9", " "), sum(elfe$A02P_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_10"), sum(elfe$A02P_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_11"), sum(elfe$A02P_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_12"), sum(elfe$A02P_LIENTYP_12 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_13"), sum(elfe$A02P_LIENTYP_13 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_14"), sum(elfe$A02P_LIENTYP_14 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_15"), sum(elfe$A02P_LIENTYP_15 == i, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_16"), sum(elfe$A02P_LIENTYP_16 == i, na.rm=TRUE)))
  
  print(paste(paste0("A03R_LIENTYP_1", " "), sum(elfe$A03R_LIENTYP_1 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_2", " "), sum(elfe$A03R_LIENTYP_2 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_3", " "), sum(elfe$A03R_LIENTYP_3 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_4", " "), sum(elfe$A03R_LIENTYP_4 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_5", " "), sum(elfe$A03R_LIENTYP_5 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_6", " "), sum(elfe$A03R_LIENTYP_6 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_7", " "), sum(elfe$A03R_LIENTYP_7 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_8", " "), sum(elfe$A03R_LIENTYP_8 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_9", " "), sum(elfe$A03R_LIENTYP_9 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_10"), sum(elfe$A03R_LIENTYP_10 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_11"), sum(elfe$A03R_LIENTYP_11 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_12"), sum(elfe$A03R_LIENTYP_12 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_13"), sum(elfe$A03R_LIENTYP_13 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_14"), sum(elfe$A03R_LIENTYP_14 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_15"), sum(elfe$A03R_LIENTYP_15 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_16"), sum(elfe$A03R_LIENTYP_16 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_17"), sum(elfe$A03R_LIENTYP_17 == i, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_18"), sum(elfe$A03R_LIENTYP_18 == i, na.rm=TRUE)))
}

condlientyp <- function(i, j) {
  print(paste(paste0("M02M_LIENTYP_4", " "), sum(elfe$M02M_LIENTYP_4 == i & elfe$M02M_SEXEC1_4 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_5", " "), sum(elfe$M02M_LIENTYP_5 == i & elfe$M02M_SEXEC1_5 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_6", " "), sum(elfe$M02M_LIENTYP_6 == i & elfe$M02M_SEXEC1_6 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_7", " "), sum(elfe$M02M_LIENTYP_7 == i & elfe$M02M_SEXEC1_7 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_8", " "), sum(elfe$M02M_LIENTYP_8 == i & elfe$M02M_SEXEC1_8 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_9", " "), sum(elfe$M02M_LIENTYP_9 == i & elfe$M02M_SEXEC1_9 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_10"), sum(elfe$M02M_LIENTYP_10 == i & elfe$M02M_SEXEC1_10 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_11"), sum(elfe$M02M_LIENTYP_11 == i & elfe$M02M_SEXEC1_11 == j, na.rm=TRUE)))
  print(paste(paste0("M02M_LIENTYP_12"), sum(elfe$M02M_LIENTYP_12 == i & elfe$M02M_SEXEC1_12 == j, na.rm=TRUE)))
  
  print(paste(paste0("M02P_LIENTYP_4", " "), sum(elfe$M02P_LIENTYP_4 == i & elfe$M02P_SEXEC2_4 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_5", " "), sum(elfe$M02P_LIENTYP_5 == i & elfe$M02P_SEXEC2_5 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_6", " "), sum(elfe$M02P_LIENTYP_6 == i & elfe$M02P_SEXEC2_6 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_7", " "), sum(elfe$M02P_LIENTYP_7 == i & elfe$M02P_SEXEC2_7 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_8", " "), sum(elfe$M02P_LIENTYP_8 == i & elfe$M02P_SEXEC2_8 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_9", " "), sum(elfe$M02P_LIENTYP_9 == i & elfe$M02P_SEXEC2_9 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_10"), sum(elfe$M02P_LIENTYP_10 == i & elfe$M02P_SEXEC2_10 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_11"), sum(elfe$M02P_LIENTYP_11 == i & elfe$M02P_SEXEC2_11 == j, na.rm=TRUE)))
  print(paste(paste0("M02P_LIENTYP_12"), sum(elfe$M02P_LIENTYP_12 == i & elfe$M02P_SEXEC2_12 == j, na.rm=TRUE)))
  
  print(paste(paste0("A01M_LIENTYP_4", " "), sum(elfe$A01M_LIENTYP_4 == i & elfe$A01M_SEXE_4 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_5", " "), sum(elfe$A01M_LIENTYP_5 == i & elfe$A01M_SEXE_5 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_6", " "), sum(elfe$A01M_LIENTYP_6 == i & elfe$A01M_SEXE_6 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_7", " "), sum(elfe$A01M_LIENTYP_7 == i & elfe$A01M_SEXE_7 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_8", " "), sum(elfe$A01M_LIENTYP_8 == i & elfe$A01M_SEXE_8 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_9", " "), sum(elfe$A01M_LIENTYP_9 == i & elfe$A01M_SEXE_9 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_10"), sum(elfe$A01M_LIENTYP_10 == i & elfe$A01M_SEXE_10 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_11"), sum(elfe$A01M_LIENTYP_11 == i & elfe$A01M_SEXE_11 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_12"), sum(elfe$A01M_LIENTYP_12 == i & elfe$A01M_SEXE_12 == j, na.rm=TRUE)))
  print(paste(paste0("A01M_LIENTYP_13"), sum(elfe$A01M_LIENTYP_13 == i & elfe$A01M_SEXE_13 == j, na.rm=TRUE)))
  
  print(paste(paste0("A01P_LIENTYP_4", " "), sum(elfe$A01P_LIENTYP_4 == i & elfe$A01P_SEXE_4 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_5", " "), sum(elfe$A01P_LIENTYP_5 == i & elfe$A01P_SEXE_5 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_6", " "), sum(elfe$A01P_LIENTYP_6 == i & elfe$A01P_SEXE_6 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_7", " "), sum(elfe$A01P_LIENTYP_7 == i & elfe$A01P_SEXE_7 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_8", " "), sum(elfe$A01P_LIENTYP_8 == i & elfe$A01P_SEXE_8 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_9", " "), sum(elfe$A01P_LIENTYP_9 == i & elfe$A01P_SEXE_9 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_10"), sum(elfe$A01P_LIENTYP_10 == i & elfe$A01P_SEXE_10 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_11"), sum(elfe$A01P_LIENTYP_11 == i & elfe$A01P_SEXE_11 == j, na.rm=TRUE)))
  print(paste(paste0("A01P_LIENTYP_12"), sum(elfe$A01P_LIENTYP_12 == i & elfe$A01P_SEXE_12 == j, na.rm=TRUE)))
  
  print(paste(paste0("A02M_LIENTYP_4", " "), sum(elfe$A02M_LIENTYP_4 == i & elfe$A02M_SEXE_4 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_5", " "), sum(elfe$A02M_LIENTYP_5 == i & elfe$A02M_SEXE_5 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_6", " "), sum(elfe$A02M_LIENTYP_6 == i & elfe$A02M_SEXE_6 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_7", " "), sum(elfe$A02M_LIENTYP_7 == i & elfe$A02M_SEXE_7 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_8", " "), sum(elfe$A02M_LIENTYP_8 == i & elfe$A02M_SEXE_8 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_9", " "), sum(elfe$A02M_LIENTYP_9 == i & elfe$A02M_SEXE_9 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_10"), sum(elfe$A02M_LIENTYP_10 == i & elfe$A02M_SEXE_10 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_11"), sum(elfe$A02M_LIENTYP_11 == i & elfe$A02M_SEXE_11 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_12"), sum(elfe$A02M_LIENTYP_12 == i & elfe$A02M_SEXE_12 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_13"), sum(elfe$A02M_LIENTYP_13 == i & elfe$A02M_SEXE_13 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_14"), sum(elfe$A02M_LIENTYP_14 == i & elfe$A02M_SEXE_14 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_15"), sum(elfe$A02M_LIENTYP_15 == i & elfe$A02M_SEXE_15 == j, na.rm=TRUE)))
  print(paste(paste0("A02M_LIENTYP_16"), sum(elfe$A02M_LIENTYP_16 == i & elfe$A02M_SEXE_16 == j, na.rm=TRUE)))
  
  print(paste(paste0("A02P_LIENTYP_4", " "), sum(elfe$A02P_LIENTYP_4 == i & elfe$A02P_SEXE_4 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_5", " "), sum(elfe$A02P_LIENTYP_5 == i & elfe$A02P_SEXE_5 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_6", " "), sum(elfe$A02P_LIENTYP_6 == i & elfe$A02P_SEXE_6 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_7", " "), sum(elfe$A02P_LIENTYP_7 == i & elfe$A02P_SEXE_7 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_8", " "), sum(elfe$A02P_LIENTYP_8 == i & elfe$A02P_SEXE_8 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_9", " "), sum(elfe$A02P_LIENTYP_9 == i & elfe$A02P_SEXE_9 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_10"), sum(elfe$A02P_LIENTYP_10 == i & elfe$A02P_SEXE_10 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_11"), sum(elfe$A02P_LIENTYP_11 == i & elfe$A02P_SEXE_11 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_12"), sum(elfe$A02P_LIENTYP_12 == i & elfe$A02P_SEXE_12 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_13"), sum(elfe$A02P_LIENTYP_13 == i & elfe$A02P_SEXE_13 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_14"), sum(elfe$A02P_LIENTYP_14 == i & elfe$A02P_SEXE_14 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_15"), sum(elfe$A02P_LIENTYP_15 == i & elfe$A02P_SEXE_15 == j, na.rm=TRUE)))
  print(paste(paste0("A02P_LIENTYP_16"), sum(elfe$A02P_LIENTYP_16 == i & elfe$A02P_SEXE_16 == j, na.rm=TRUE)))
  
  print(paste(paste0("A03R_LIENTYP_4", " "), sum(elfe$A03R_LIENTYP_4 == i & elfe$A03R_SEXE_4 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_5", " "), sum(elfe$A03R_LIENTYP_5 == i & elfe$A03R_SEXE_5 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_6", " "), sum(elfe$A03R_LIENTYP_6 == i & elfe$A03R_SEXE_6 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_7", " "), sum(elfe$A03R_LIENTYP_7 == i & elfe$A03R_SEXE_7 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_8", " "), sum(elfe$A03R_LIENTYP_8 == i & elfe$A03R_SEXE_8 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_9", " "), sum(elfe$A03R_LIENTYP_9 == i & elfe$A03R_SEXE_9 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_10"), sum(elfe$A03R_LIENTYP_10 == i & elfe$A03R_SEXE_10 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_11"), sum(elfe$A03R_LIENTYP_11 == i & elfe$A03R_SEXE_11 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_12"), sum(elfe$A03R_LIENTYP_12 == i & elfe$A03R_SEXE_12 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_13"), sum(elfe$A03R_LIENTYP_13 == i & elfe$A03R_SEXE_13 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_14"), sum(elfe$A03R_LIENTYP_14 == i & elfe$A03R_SEXE_14 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_15"), sum(elfe$A03R_LIENTYP_15 == i & elfe$A03R_SEXE_15 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_16"), sum(elfe$A03R_LIENTYP_16 == i & elfe$A03R_SEXE_16 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_17"), sum(elfe$A03R_LIENTYP_17 == i & elfe$A03R_SEXE_17 == j, na.rm=TRUE)))
  print(paste(paste0("A03R_LIENTYP_18"), sum(elfe$A03R_LIENTYP_18 == i & elfe$A03R_SEXE_18 == j, na.rm=TRUE)))
}

sexe_labeller <- function(variable,value){
  return(sexe_names[value])
}

get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

combine <- function(mothervar, fathervar, combvar) {
  elfe[, (combvar) := get(mothervar)]
  elfe[is.na(get(combvar)) & !is.na(get(fathervar)),
       (combvar) := get(fathervar)]
  return(elfe)
}

tabulate_with_labels <- function(vars, data, labels_list, names_list) {
  results <- list()
  
  for (var in vars) {
    if (!var %in% colnames(data)) next  # Skip variables not in data
    
    var_labels <- labels_list[[var]]  # Get label mapping
    var_name <- names_list[[var]]      # Get human-readable name
    
    tab <- table(data[[var]], useNA = "ifany")  # Include NA values
    tab_names <- as.character(names(tab))  # Convert table names to character
    tab_labels <- ifelse(tab_names %in% names(var_labels), var_labels[tab_names], tab_names)  # Replace values with labels safely
    
    names(tab) <- tab_labels  # Assign new names
    
    tab_df <- as.data.frame(tab)
    colnames(tab_df) <- c("Response", "Frequency")
    tab_df$Variable_Name <- var_name
    
    results[[var]] <- tab_df
  }
  
  final_result <- do.call(rbind, results)
  return(final_result)
}



tabulate_mcsvariable <- function(var_name) {
  var_label <- names_list[[var_name]]  # Get variable label
  value_labels <- labels_list[[var_name]]  # Get value labels
  
  # Convert to factor with labels
  mcs[[var_name]] <- factor(mcs[[var_name]], levels = names(value_labels), labels = value_labels, exclude = NULL)
  
  # Tabulate with NA counts
  tab <- table(mcs[[var_name]], useNA = "ifany")
  
  # Print result
  cat("\n", var_label, "\n")
  for (i in names(tab)) {
    cat(i, tab[i], "\n")
  }
}



tabulate_elfevariable <- function(var_name) {
  var_label <- names_list[[var_name]]  # Get variable label
  value_labels <- labels_list[[var_name]]  # Get value labels
  
  # Convert to factor with labels
  elfe[[var_name]] <- factor(elfe[[var_name]], levels = names(value_labels), labels = value_labels, exclude = NULL)
  
  # Tabulate with NA counts
  tab <- table(elfe[[var_name]], useNA = "ifany")
  
  # Print result
  cat("\n", var_label, "\n")
  for (i in names(tab)) {
    cat(i, tab[i], "\n")
  }
}

# # Example usage:
# # Define labels for the variables
# labels_list <- list(
#   "BPINDE00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPOBRE00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPNEGO00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPRSPE00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPWESC00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPREVA00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No", "3" = "Don't know"),
#   "BPRULE00_m" = c("-1" = "Not Applicable", "1" = "Lots of rules", "2" = "Not many rules", "3" = "It varies"),
#   "BPENFO00_m" = c("-1" = "Not Applicable", "1" = "Strictly enforced", "2" = "Not very strictly enforced", "3" = "It varies"),
#   "BPQUAL00_m" = c("-1" = "Not Applicable", "1" = "To be well liked or popular", "2" = "To think for {him her}self", 
#                    "3" = "To work hard", "4" = "To help others when they need help", "5" = "To obey {his her} parents", 
#                    "6" = "To learn religious values", "7" = "Don't Know"),
#   "BPSEQU00_m" = c("-1" = "Not Applicable", "1" = "To be well liked or popular", "2" = "To think for {him her}self", 
#                    "3" = "To work hard", "4" = "To help others when they need help", "5" = "To obey {his her} parents", 
#                    "6" = "To learn religious values", "7" = "Don't Know"),
#   "BPTHQU00_m" = c("-1" = "Not Applicable", "1" = "To be well liked or popular", "2" = "To think for {him her}self", 
#                    "3" = "To work hard", "4" = "To help others when they need help", "5" = "To obey {his her} parents", 
#                    "6" = "To learn religious values", "7" = "Don't Know"),
#   "BPLEIM00_m" = c("-1" = "Not Applicable", "1" = "To be well liked or popular", "2" = "To think for {him her}self", 
#                    "3" = "To work hard", "4" = "To help others when they need help", "5" = "To obey {his her} parents", 
#                    "6" = "To learn religious values", "7" = "Don't Know"),
#   "BPTWSA00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No"),
#   "BPTRSA00_m" = c("-1" = "Not Applicable", "1" = "Yes", "2" = "No")
# )
# 
# # Define names for each variable
# names_list <- c(
#   "BPINDE00_m" = "M Values to instil: independence",
#   "BPOBRE00_m" = "M Values to instil obedience and respect",
#   "BPNEGO00_m" = "M Values to instil: art of negotiation",
#   "BPRSPE00_m" = "M Values to instil: respect for elders",
#   "BPWESC00_m" = "M Values to instil: doing well at school",
#   "BPREVA00_m" = "M Values to instil: religious values",
#   "BPRULE00_m" = "M Family has lots/not many rules",
#   "BPENFO00_m" = "M Rules strictly/not strictly enforced",
#   "BPQUAL00_m" = "M Most important quality",
#   "BPSEQU00_m" = "M Next important quality",
#   "BPTHQU00_m" = "M Third important quality",
#   "BPLEIM00_m" = "M Least important quality",
#   "BPTWSA00_m" = "M Same for other twin/triplet",
#   "BPTRSA00_m" = "M Same for other twin/triplet"
# )
# 
# vars <- names(labels_list)  # All the variable names
# final_result <- tabulate_with_labels(vars, mcs, labels_list, names_list)
# print(final_result)

filter_positive_mcsvars <- function(vars, data = mcs) {
  # Keep only variables that do not contain only negative values
  positive_vars <- vars[sapply(vars, function(var) any(data[[var]] >= 0, na.rm = TRUE))]
  return(positive_vars)
}

sex_mcsttest <- function(variables, data = mcs, sex_var = "AHCSEX00", var_labels = names_list) {
  results_list <- list()
  
  for (var in variables) {
    
    t_test_result <- t.test(data[[var]] ~ data[[sex_var]])
    
    if (t_test_result$p.value < 0.05) {
      
      mean_sex1 <- mean(data[[var]][data[[sex_var]] == 1], na.rm = TRUE)
      mean_sex2 <- mean(data[[var]][data[[sex_var]] == 2], na.rm = TRUE)
      
      # Use variable label if available, otherwise use the variable name
      var_label <- ifelse(var %in% names(var_labels), var_labels[[var]], var)
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var_label, 
        Mean_Male = mean_sex1, 
        Mean_Female = mean_sex2, 
        P_Value = t_test_result$p.value
      )
    }
  }
  
  summary_result <- do.call(rbind, results_list)
  return(summary_result)
}

ses_mcsanova <- function(variables, data = mcs, group_var = "ADD07S00_m", var_labels = names_list) {
  results_list <- list()
  
  for (var in variables) {
    
    # Perform ANOVA
    anova_result <- aov(data[[var]] ~ as.factor(data[[group_var]]), data = data)
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Extract p-value
    
    if (p_value < 0.05) {  # Only keep significant results
      
      # Compute means for each SES group
      mean_sesminus1 <- mean(data[[var]][data[[group_var]] == -1], na.rm = TRUE)
      mean_ses1 <- mean(data[[var]][data[[group_var]] == 1], na.rm = TRUE)
      mean_ses2 <- mean(data[[var]][data[[group_var]] == 2], na.rm = TRUE)
      mean_ses3 <- mean(data[[var]][data[[group_var]] == 3], na.rm = TRUE)
      mean_ses4 <- mean(data[[var]][data[[group_var]] == 4], na.rm = TRUE)
      mean_ses5 <- mean(data[[var]][data[[group_var]] == 5], na.rm = TRUE)
      mean_ses6 <- mean(data[[var]][data[[group_var]] == 6], na.rm = TRUE)
      mean_ses7 <- mean(data[[var]][data[[group_var]] == 7], na.rm = TRUE)
      
      # Get the variable label
      var_label <- ifelse(var %in% names(var_labels), var_labels[[var]], var)
      
      # Create a results dataframe
      result_row <- data.frame(
        Variable = var_label,
        P_Value = p_value,
        Mean_NA = mean_sesminus1,
        Mean_HiManag = mean_ses1,
        Mean_LoManag = mean_ses2,
        Mean_Int = mean_ses3,
        Mean_SmallEmp = mean_ses4,
        Mean_LoSup = mean_ses5,
        Mean_SemiRout = mean_ses6,
        Mean_Rout = mean_ses7
      )
      
      results_list[[length(results_list) + 1]] <- result_row
    }
  }
  
  # Combine all results
  summary_result <- do.call(rbind, results_list)
  return(summary_result)
}



# # Example usage:
# variables_to_test <- c("BPINDE00_m", "BPOBRE00_m", "BPNEGO00_m", "BPRSPE00_m", "BPWESC00_m", 
#                        "BPREVA00_m", "BPRULE00_m", "BPENFO00_m", "BPQUAL00_m", "BPSEQU00_m", 
#                        "BPTHQU00_m", "BPLEIM00_m", "BPTWSA00_m")
# summary_result <- sex_ttest(variables_to_test)
# print(summary_result)


filter_positive_elfevars <- function(vars, data = elfe) {
  # Keep only variables that do not contain only negative values
  positive_vars <- vars[sapply(vars, function(var) any(data[[var]] >= 0, na.rm = TRUE))]
  return(positive_vars)
}

sex_elfettest <- function(variables, data = elfe, sex_var = "SEXE_ENF", var_labels = names_list) {
  results_list <- list()
  
  for (var in variables) {
    
    t_test_result <- t.test(data[[var]] ~ data[[sex_var]])
    
    if (t_test_result$p.value < 0.05) {
      
      mean_sex1 <- mean(data[[var]][data[[sex_var]] == 1], na.rm = TRUE)
      mean_sex2 <- mean(data[[var]][data[[sex_var]] == 2], na.rm = TRUE)
      
      # Use variable label if available, otherwise use the variable name
      var_label <- ifelse(var %in% names(var_labels), var_labels[[var]], var)
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Variable = var_label, 
        Mean_Male = mean_sex1, 
        Mean_Female = mean_sex2, 
        P_Value = t_test_result$p.value
      )
    }
  }
  
  summary_result <- do.call(rbind, results_list)
  return(summary_result)
}





ses_elfeanova <- function(variables, data = elfe, group_var = "M02X_PCS08_MERE", var_labels = names_list) {
  results_list <- list()
  
  for (var in variables) {
    
    # Perform ANOVA
    anova_result <- aov(data[[var]] ~ as.factor(data[[group_var]]), data = data)
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Extract p-value
    
    if (p_value < 0.05) {  # Only keep significant results
      
      # Compute means for each SES group
      mean_ses0 <- mean(data[[var]][data[[group_var]] == 0], na.rm = TRUE)
      mean_ses1 <- mean(data[[var]][data[[group_var]] == 1], na.rm = TRUE)
      mean_ses2 <- mean(data[[var]][data[[group_var]] == 2], na.rm = TRUE)
      mean_ses3 <- mean(data[[var]][data[[group_var]] == 3], na.rm = TRUE)
      mean_ses4 <- mean(data[[var]][data[[group_var]] == 4], na.rm = TRUE)
      mean_ses5 <- mean(data[[var]][data[[group_var]] == 5], na.rm = TRUE)
      mean_ses6 <- mean(data[[var]][data[[group_var]] == 6], na.rm = TRUE)
      mean_ses7 <- mean(data[[var]][data[[group_var]] == 7], na.rm = TRUE)
      mean_ses8 <- mean(data[[var]][data[[group_var]] == 8], na.rm = TRUE)
      
      # Get the variable label
      var_label <- ifelse(var %in% names(var_labels), var_labels[[var]], var)
      
      # Create a results dataframe
      result_row <- data.frame(
        Variable = var_label,
        P_Value = p_value,
        Mean_NoProf = mean_ses0,
        Mean_Farmer = mean_ses1,
        Mean_BusiOwner = mean_ses2,
        Mean_Exec = mean_ses3,
        Mean_Int = mean_ses4,
        Mean_Empl = mean_ses5,
        Mean_Worker = mean_ses6,
        Mean_Retired = mean_ses7,
        Mean_Other = mean_ses8
      )
      
      results_list[[length(results_list) + 1]] <- result_row
    }
  }
  
  # Combine all results
  summary_result <- do.call(rbind, results_list)
  return(summary_result)
}


# table(mcs$variable, useNA = "always")

tabm <- function(x) {
  table(mcs[[deparse(substitute(x))]], useNA = "always")
}


# table(elfe$variable, useNA = "always")

tabe <- function(x) {
  table(elfe[[deparse(substitute(x))]], useNA = "always")
}












