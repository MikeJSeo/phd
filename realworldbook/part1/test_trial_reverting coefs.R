library(lme4) #for fitting glmm

#function to generate example dataset
generate_ipdma_example2 <- function(type = "continuous"){
  
  N <- 100  #number of patients per trial
  N.trials <- 6 #number of trials
  studyid <- rep(1:6, each = N)
  
  if(type == "continuous"){
    
    alpha <- rep(c(11, 8, 10.5, 9.6, 12.9, 15.8), each = N) #study effects
    delta <- rep(rnorm(6, -2.5, 2), each = N) #treatment effects
    beta1 <- rep(c(0.24, 0.21, 0.20, 0.18, 0.25, 0.22), each = N) #prognostic effects of z1
    gamma1 <- rep(c(-0.9, -0.5, -0.6, -0.7, -0.1, -0.3), each = N) #interaction effects of z1
    beta2 <- rep(c(0.15, 0.21, 0.30, 0.38, 0.45, 0.42), each = N) #prognostic effects of z2
    gamma2 <- rep(c(0.9, 0.5, 0.5, 0.7, 0.1, 0.5), each = N) #interaction effects of z2
    
    z1 <- stats::rnorm(N*N.trials)
    z2 <- stats::rnorm(N*N.trials)
    treat <- rbinom(N*N.trials, 1, 0.5)
    
    y <- round(alpha + delta*treat + beta1*z1 + beta2*z2 + gamma1*z1*treat + gamma2*z2*treat) 
    ds <- as.data.frame(cbind(studyid, treat, z1, z2, y))
    ds$studyid <- as.factor(studyid)
  } else if (type == "binary"){

    alpha <- rep(c(0.11, 0.8, 1.05, 0.96, 0.129, 0.158), each = N) #study effects
    delta <- rep(rnorm(6, -1.0, 2.0), each = N) #treatment effects
    beta1 <- rep(c(0.24, 0.21, 0.20, 0.18, 0.25, 0.22), each = N) #prognostic effects of w1
    gamma1 <- rep(c(-0.9, -0.5, -0.6, -0.7, -0.1, -0.3), each = N) #interaction effects of w1
    beta2 <- rep(c(0.15, 0.21, 0.30, 0.38, 0.45, 0.42), each = N) #prognostic effects of w2
    gamma2 <- rep(c(0.9, 0.5, 0.5, 0.7, 0.1, 0.5), each = N) #interaction effects of w2
    
    w1 <- stats::rnorm(N*N.trials)
    w2 <- stats::rnorm(N*N.trials)
    treat <- rbinom(N*N.trials, 1, 0.5)
    
    expit <- function(x){
      exp(x)/(1+exp(x))
    }
    linearpredictor <- round(alpha + delta*treat + beta1*w1 + beta2*w2 + gamma1*w1*treat + gamma2*w2*treat)
    y <- stats::rbinom(N, 1, expit(linearpredictor))
    ds <- as.data.frame(cbind(studyid, treat, w1, w2, y))
    ds$studyid <- as.factor(studyid)
  }  
  return(ds)
}

# Function to revert coefficients
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


##continuous outcome
ds <- generate_ipdma_example2(type = "continuous")
ds0 <- ds

scale_mean <- apply(ds[,c("z1", "z2")], 2, mean)
scale_sd <- apply(ds[,c("z1", "z2")], 2, sd)
ds[,c("z1", "z2")] <- apply(ds[,c("z1", "z2")], 2, scale)

m2 <- lmer(y ~ -1 + studyid + (z1 + z2)*treat + (-1 + treat|studyid), data = ds)
summary(m2)

#reorganize the summary results (i.e. treatment effect should go last)
variable.order <- c(paste0("studyid",1:6), c("z1", "z2"),
                    paste0(c("z1", "z2"), ":treat"), "treat")

y <- summary(m2)$coefficients[variable.order,"Estimate"]
Sigma <- vcov(m2)[variable.order, variable.order]
X_mean <- scale_mean
X_sd <- scale_sd
unstandardized <- unstandardize_coefficients_frequentist(X_mean = X_mean, X_sd = X_sd,
                                       y = y, Sigma = Sigma, ntreat = 2, nstudies = 6)
unstandardized$y #coefficient estimate
sqrt(diag(unstandardized$Sigma)) #standard
                    
#compare this with the regression without standardizing from the beginning
m0 <- lmer(y ~ -1 + studyid + (z1 + z2)*treat + (-1 + treat|studyid), data = ds0)
summary(m0)



#########################
# binary case
ds <- generate_ipdma_example2(type = "binary")
ds0 <- ds

scale_mean <- apply(ds[,c("w1", "w2")], 2, mean)
scale_sd <- apply(ds[,c("w1", "w2")], 2, sd)
ds[,c("w1", "w2")] <- apply(ds[,c("w1", "w2")], 2, scale)

############## glmm_full
m2 <- glmer(y ~ -1 + studyid + (w1 + w2)*treat + (-1 + treat|studyid), data = ds, family = binomial)
summary(m2)

variable.order <- c(paste0("studyid",1:6), c("w1", "w2"),
                    paste0(c("w1", "w2"), ":treat"), "treat")

y <- summary(m2)$coefficients[variable.order,"Estimate"]
Sigma <- vcov(m2)[variable.order, variable.order]
X_mean <- scale_mean
X_sd <- scale_sd
unstandardized <- unstandardize_coefficients_frequentist(X_mean = X_mean, X_sd = X_sd,
                                       y = y, Sigma = Sigma, ntreat = 2, nstudies = 6)
unstandardized$y #coefficient estimate
sqrt(diag(unstandardized$Sigma)) #standard

#compare this with the regression without standardizing in the beginning
m0 <- glmer(y ~ -1 + studyid + (w1 + w2)*treat + (-1 + treat|studyid), data = ds0, family = binomial)
summary(m0)
