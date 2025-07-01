# Part time to take care of kids and home

# 2m mothers

table(elfe$mpartkids2m)

elfe[M02M_LIENTYP_3 == 2 & M02M_EMPLTX_3 %in% 1:100,
     mpartkids2m:= ifelse(M02M_PQPART_3 %in% c(4, 5), 1, 0)]

for (i in 3:12) {
  lientyp_col <- paste0("M02P_LIENTYP_", i)
  empltx_col  <- paste0("M02P_EMPLTX_", i)
  pqpart_col  <- paste0("M02P_PQPART_", i)
  
  elfe[get(lientyp_col) == 2 & get(empltx_col) %in% 1:100 & is.na(mpartkids2m),
       mpartkids2m:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}

# 2m fathers

elfe[M02M_LIENTYP_4 == 1 & M02M_EMPLTX_4 %in% 1:100,
     fpartkids2m:= ifelse(M02M_PQPART_4 %in% c(4, 5), 1, 0)]

elfe[M02P_LIENTYP_4 == 1 & M02P_EMPLTX_4 %in% 1:100 & is.na(fpartkids2m),
     fpartkids2m:= ifelse(M02P_PQPART_4 %in% c(4, 5), 1, 0)]

# 1y mothers

elfe[A01M_LIENTYP_3 == 2 & A01M_EMPLTX_3 %in% 1:100,
     mpartkids1y:= ifelse(A01M_PQPART_3 %in% c(4, 5), 1, 0)]

for (i in 3:15) {
  lientyp_col <- paste0("A01P_LIENTYP_", i)
  empltx_col  <- paste0("A01P_EMPLTX_", i)
  pqpart_col  <- paste0("A01P_PQPART_", i)
  
  elfe[get(lientyp_col) == 2 & get(empltx_col) %in% 1:100 & is.na(mpartkids1y),
       mpartkids1y:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}

# 1y fathers

elfe[A01M_LIENTYP_4 == 1 & A01M_EMPLTX_4 %in% 1:100,
     fpartkids1y:= ifelse(A01M_PQPART_4 %in% c(4, 5), 1, 0)]

for (i in 5:15) {
  lientyp_col <- paste0("A01P_LIENTYP_", i)
  empltx_col  <- paste0("A01P_EMPLTX_", i)
  pqpart_col  <- paste0("A01P_PQPART_", i)
  
  elfe[get(lientyp_col) == 1 & get(empltx_col) %in% 1:100 & is.na(fpartkids1y),
       fpartkids1y:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}

# 2y mothers

elfe[A02M_LIENTYP_3 == 2 & A02M_EMPLTX_3 %in% 1:100,
     mpartkids2y:= ifelse(A02M_PQPART_3 %in% c(4, 5), 1, 0)]

for (i in 3:20) {
  lientyp_col <- paste0("A02P_LIENTYP_", i)
  empltx_col  <- paste0("A02P_EMPLTX_", i)
  pqpart_col  <- paste0("A02P_PQPART_", i)
  
  elfe[get(lientyp_col) == 2 & get(empltx_col) %in% 1:100 & is.na(mpartkids2y),
       mpartkids2y:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}

# 2y fathers

elfe[A02M_LIENTYP_4 == 1 & A02M_EMPLTX_4 %in% 1:100,
     fpartkids2y:= ifelse(A02M_PQPART_4 %in% c(4, 5), 1, 0)]

for (i in 5:20) {
  lientyp_col <- paste0("A02P_LIENTYP_", i)
  empltx_col  <- paste0("A02P_EMPLTX_", i)
  pqpart_col  <- paste0("A02P_PQPART_", i)
  
  elfe[get(lientyp_col) == 1 & get(empltx_col) %in% 1:100 & is.na(fpartkids2y),
       fpartkids2y:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}


# 3y mothers

elfe[A03R_LIENTYP_3 == 2 & A03R_EMPLTX_3 %in% 1:100,
     mpartkids3y:= ifelse(A03R_PQPART_3 %in% c(4, 5), 1, 0)]

for (i in 4:20) {
  lientyp_col <- paste0("A03R_LIENTYP_", i)
  empltx_col  <- paste0("A03R_EMPLTX_", i)
  pqpart_col  <- paste0("A03R_PQPART_", i)
  
  elfe[get(lientyp_col) == 2 & get(empltx_col) %in% 1:100 & is.na(mpartkids3y),
       mpartkids3y:= ifelse(get(pqpart_col) %in% c(4, 5), 1, 0)]
}

# 3y fathers

elfe[A03R_LIENTYP_4 == 1 & A03R_EMPLTX_4 %in% 1:100,
     fpartkids3y:= ifelse(A03R_PQPART_4 %in% c(4, 5), 1, 0)]

for (i in 3:20) {
  lientyp_col <- paste0("A03R_LIENTYP_", i)
  empltx_col  <- paste0("A03R_EMPLTX_", i)
  pqpart_col  <- paste0("A03R_PQPART_", i)
  
  elfe[get(lientyp_col) == 1 & get(empltx_col) %in% 1:100 & is.na(fpartkids3y),
       fpartkids3y:= ifelse(get(pqpart_col) %in% c(4, 5), 1L, 0L)]
}

elfe$fpartkids3y <- as.numeric(elfe$fpartkids3y)