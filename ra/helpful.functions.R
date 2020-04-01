### helpful functions used


find_jags_data0 <- function(samples, samples_colnames, index){
  
  samples_result <- as.matrix(samples)
  samples_result <- samples_result[, colSums(samples_result != 0) > 0]
  
  y <- apply(samples_result, 2, mean); names(y) <- samples_colnames
  Sigma <- cov(samples_result); colnames(Sigma) <- rownames(Sigma) <- samples_colnames
  
  y_index <- y[index]
  Sigma_index <- Sigma[index, index]
  Omega_index <- solve(Sigma_index)
  
  return(list(y_index = y_index, Omega_index = Omega_index))
}


find_jags_data <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL, samples4 = NULL, samples5 = NULL,
                           samples1_colnames, samples2_colnames, samples3_colnames, samples4_colnames, samples5_colnames,
                           index1, index2, index3, index4, index5){
  
  r1 <- find_jags_data0(samples1, samples1_colnames, index1)
  r2 <- find_jags_data0(samples2, samples2_colnames, index2)
  r3 <- find_jags_data0(samples3, samples3_colnames, index3)
  r4 <- find_jags_data0(samples4, samples4_colnames, index4)

  # meta analyze treatment effect
  data_jags <- 
    list(y1 = r1[[1]],
         y2 = r2[[1]], 
         y3 = r3[[1]],
         y4 = r4[[1]],
         Omega1 = r1[[2]],
         Omega2 = r2[[2]],
         Omega3 = r3[[2]],
         Omega4 = r4[[2]])
  return(data_jags)
}



#BSRBR
BSRBR_result <- as.matrix(BSRBR_samples)
BSRBR_result <- BSRBR_result[, colSums(BSRBR_result != 0) > 0] #remove 0 columns

y1 <- apply(BSRBR_result, 2, mean); names(y1) <- full_colnames
Sigma1 <- cov(BSRBR_result); colnames(Sigma1) <- rownames(Sigma1) <- full_colnames

y1_treat <- y1[c("RTX", "TCZ")]
Sigma1_treat <- Sigma1[c("RTX", "TCZ"), c("RTX", "TCZ")]
Omega1_treat <- solve(Sigma1_treat)

y1_notreat <- y1[!names(y1) %in% c("RTX", "TCZ")]
Sigma1_notreat <- Sigma1[!names(y1) %in% c("RTX", "TCZ"), !names(y1) %in% c("RTX", "TCZ")]
Omega1_notreat <- solve(Sigma1_notreat)


#SCQM
SCQM_result <- as.matrix(SCQM_samples)
SCQM_result <- SCQM_result[, colSums(SCQM_result != 0) > 0] #remove 0 columns

y2 <- apply(SCQM_result, 2, mean); names(y2) <- full_colnames
Sigma2 <- cov(SCQM_result); colnames(Sigma2) <- rownames(Sigma2) <- full_colnames

y2_treat <- y2[c("RTX", "TCZ")]
Sigma2_treat <- Sigma2[c("RTX", "TCZ"), c("RTX", "TCZ")]
Omega2_treat <- solve(Sigma2_treat)

y2_notreat <- y2[!names(y2) %in% c("RTX", "TCZ")]
Sigma2_notreat <- Sigma2[!names(y2) %in% c("RTX", "TCZ"), !names(y2) %in% c("RTX", "TCZ")]
Omega2_notreat <- solve(Sigma2_notreat)


#REFLEX
REFLEX_result <- as.matrix(REFLEX_samples)
REFLEX_result <- REFLEX_result[, colSums(REFLEX_result != 0) > 0] #remove 0 columns

y3 <- apply(REFLEX_result, 2, mean); names(y3) <- partial_colnames1
Sigma3 <- cov(REFLEX_result); colnames(Sigma3) <- rownames(Sigma3) <- partial_colnames1

y3_treat <- y3["RTX"]
Sigma3_treat <- Sigma3["RTX", "RTX"]
Omega3_treat <- solve(Sigma3_treat) 

y3_notreat <- y3[!names(y3) %in% c("RTX")]
Sigma3_notreat <- Sigma3[!names(y3) %in% c("RTX"), !names(y3) %in% c("RTX")]
Omega3_notreat <- solve(Sigma3_notreat)

#TOWARD
TOWARD_result <- as.matrix(TOWARD_samples)
TOWARD_result <- TOWARD_result[, colSums(TOWARD_result != 0) > 0] #remove 0 columns

y4 <- apply(TOWARD_result, 2, mean); names(y4) <- partial_colnames2
Sigma4 <- cov(TOWARD_result); colnames(Sigma4) <- rownames(Sigma4) <- partial_colnames2

y4_treat <- y4["TCZ"]
Sigma4_treat <- Sigma4["TCZ", "TCZ"]
Omega4_treat <- solve(Sigma4_treat)

y4_notreat <- y4[!names(y4) %in% c("TCZ")]
Sigma4_notreat <- Sigma4[!names(y4) %in% c("TCZ"), !names(y4) %in% c("TCZ")]
Omega4_notreat <- solve(Sigma4_notreat)


# meta analyze treatment effect
data_jags2 <- 
  list(y1 = y1_treat,
       y2 = y2_treat, 
       y3 = y3_treat,
       y4 = y4_treat,
       Omega1 = Omega1_treat,
       Omega2 = Omega2_treat,
       Omega3 = Omega3_treat,
       Omega4 = Omega4_treat
  )
