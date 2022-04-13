
unstandardize_coefficients_frequentist <- function(X_mean = NULL, X_sd = NULL,
                                       y = NULL, Sigma = NULL,
                                       ntreat = NULL, nstudies = NULL){

  vec_length <- length(y)
  N_star <- matrix(0, nrow = vec_length, ncol = vec_length)

  # Intercepts
  for(k in 1:nstudies){
    dummyvec0 <- rep(0, nstudies)
    dummyvec0[k] <- 1
    N_star[k,] <- c(dummyvec0, -X_mean/X_sd, rep(0, vec_length - nstudies - length(X_mean)))
  }

  # Main effects
  for(k in 1:length(X_mean)){
    N_star[k+nstudies,] <- c(rep(0, nstudies),rep(0,k-1), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - nstudies - length(X_mean)))
  }  

  # Effect modifiers
  for(treatindex in 1:(ntreat-1)){
    for(k in 1:length(X_mean)){
      N_star[nstudies+treatindex*length(X_mean)+k,] <- c(rep(0, nstudies),rep(0, treatindex*length(X_mean)+k-1), 1/X_sd[k], rep(0, length(X_mean) - k), rep(0, vec_length - nstudies - (treatindex+1)*length(X_mean)))
    }      
  }

  # Treatment effect
  for(treatindex in 1:(ntreat-1)){
    dummyvec <- rep(0, ntreat-1)
    dummyvec[treatindex] <- 1
    N_star[nstudies+ntreat*length(X_mean)+treatindex,] <- c(rep(0, nstudies), rep(0, treatindex*length(X_mean)), -X_mean/X_sd, rep(0, vec_length - nstudies - (treatindex+1)*length(X_mean)- (ntreat-1)), dummyvec)
  }

  y_unstandardized <- N_star %*% y
  Sigma_unstandardized <- N_star %*% Sigma %*% t(N_star)

  return(list(y = y_unstandardized, Sigma = Sigma_unstandardized))
}