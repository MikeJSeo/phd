#helpful functions

####### convenient function to combine mcmc chains used in first stage analysis
add.mcmc <- function(x,y){
  
  n.chains <- length(x)
  n.var <- nvar(x)
  newobjects <- vector("list", length = n.chains)
  
  for(i in 1:n.chains){
    newobjects[[i]] <- matrix(NA, nrow = 0, ncol = n.var, dimnames = list(NULL, dimnames(x[[1]])[[2]]))
    newobjects[[i]] <- rbind(x[[i]], y[[i]])
    newobjects[[i]] <- mcmc(newobjects[[i]])
  }
  mcmc.list(newobjects)
}


######## first stage model
firstStage <- function(study_data, jags_file, mm = 20, index = c("a", "b", "c", "d", "sigma")){
  
  y <- study_data$DAS28
  X <- as.matrix(study_data[,c(-1,-2,-3)])
  X <- apply(X, 2, scale)
  XX <- mice(X, m = mm)
  
  if(length(unique(study_data$treat)) == "2"){
    study_data$treat[study_data$treat == "3"] <- "2"
  }
  
  jags_data <- list(
    N = length(y),
    treat = study_data$treat,
    Ncovariate = dim(X)[2],
    Ntreat = length(unique(study_data$treat)),
    y = y,
    X = complete(XX, 1)
  )
  mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = index, n.iter = 10000)
  
  # add more samples
  for(i in 2:mm){
    
    jags_data <- list(
      N = length(y),
      treat = study_data$treat,
      Ncovariate = dim(X)[2],
      Ntreat = length(unique(study_data$treat)),
      y = y,
      X = complete(XX, i)
    )
    mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
    stats::update(mod, 1000)
    sample_more <- coda.samples(mod, variable.names = index, n.iter = 10000)
    samples <- add.mcmc(samples, sample_more)
  }
  return(samples)
}


# summarize each first stage analysis

summarize_each_study <- function(samples){
  
  samples_result <- as.matrix(samples)
  samples_result <- samples_result[, colSums(samples_result != 0) > 0] #delete 0 value variables
  
  Vars <- grep("^a", colnames(samples_result))
  Vars <- c(Vars, grep("^b", colnames(samples_result)))
  Vars <- c(Vars, grep("^c", colnames(samples_result)))
  Vars <- c(Vars, grep("^d", colnames(samples_result)))
  
  samples_result <- samples_result[,Vars]
  y <- apply(samples_result, 2, mean)
  Sigma <- cov(samples_result)
  Omega <- solve(Sigma)
  
  return(list(y = y, Omega = Omega))
}

#second stage analysis using results from first stage analysis
secondStage <- function(y, Omega, y_weights_index = NULL, Omega_weights_index = NULL, jags_file = NULL, n.iter = 200000){
  
  if(!is.null(y_weights_index)){
    print("implement")
  }
  
  data_jags <- c(y, Omega)
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 20000)
  
  var.names <- c("alpha", "beta", "gamma", "delta")
  samples <- coda.samples(mod, variable.names = var.names, n.iter = n.iter, n.chains = 3)
  
  return(samples) 
}
  
# function to find predictions for a specified dataset using the second stage result
# Note that this function is specific to the RA dataset and uses 9 specified covariates; i.e. will give an error if applied to other datasets
findPrediction <- function(data, result) {
  
  options(na.action='na.pass')
  abd <- model.matrix(~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, data)
  abd_scaled <- abd
  abd_scaled[,c(-1,-11,-12)] <- apply(abd[,c(-1,-11,-12)], 2, scale) #scale except intercept and treatment indexes
  
  g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 2),as.data.frame(abd_scaled))[,-c(1:(9+2))]
  g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 3),as.data.frame(abd_scaled))[,-c(1:(9+2))]
  
  XX <- as.matrix(cbind(abd_scaled, g2, g3))
  X <- XX[complete.cases(XX),] # use only the complete cases for predictions
  
  coefs <- summary(result)[[1]][,"Mean"]
  
  pred <- X %*% coefs
  y <- data$DAS28[complete.cases(XX)]
  treat <- data$treat[complete.cases(XX)]
  
  return(list(y = y, pred = pred, treat = treat, X = X, coefs = coefs))
}

# find performance metric based on predictions
findPerformance <- function(prediction){
  
  with(prediction,{
    err_mse <- (pred - y)^2
    err_mse <- err_mse[!is.na(err_mse)]
    mse <- mean(err_mse, na.rm = TRUE)
    
    err_bias <- pred - y
    err_bias <- err_bias[!is.na(err_bias)]
    bias <- mean(err_bias, na.rm = TRUE)
    
    list(mse = mse, bias = bias)  
  })
}
