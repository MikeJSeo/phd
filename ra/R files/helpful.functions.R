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
firstStage <- function(study_data, jags_file, mm = 20, index = c("a", "b", "c", "d", "sigma"), scale = TRUE, no.interaction = FALSE){
  
  y <- study_data$DAS28
  treat <- factor(study_data$treat, level = c(1,2,3))
  X <- as.matrix(study_data[,c(-1,-2,-3)])
  if(scale == TRUE){
    X <- apply(X, 2, scale)  
  }
  ncov <- dim(X)[2]
  options(na.action='na.pass')
  X.ext <- model.matrix(y~ -1 + X + treat + X:treat)
  X.ext <- X.ext[,colnames(X.ext) != "treat1"]
  colnames(X.ext) <- gsub("X","",colnames(X.ext))
  X.ext <- cbind(y, X.ext)
  X.ext[,grepl(":", colnames(X.ext))] <- NA
  ini <- mice(X.ext, max = 0, print = FALSE)
  meth <- ini$meth
  
  namess <- colnames(X.ext)[grep(":", colnames(X.ext))]
  meth[grep(":", colnames(X.ext))] <- paste0("~I(", gsub(":", "*", namess),")")

  pred<- ini$pred
  XX <- mice(X.ext, meth = meth, pred = pred, maxit = 10, m = mm)
  
  if(no.interaction == TRUE){
    y <- study_data$DAS28
    treat <- study_data$treat
    X <- as.matrix(study_data[,c(-1,-2,-3)])
    if(scale == TRUE){
      X <- apply(X, 2, scale)  
    }
    ncov <- dim(X)[2]
    options(na.action='na.pass')
    X.ext <- model.matrix(y~ -1 + X + treat)
    X.ext <- X.ext[,colnames(X.ext) != "treat1"]
    colnames(X.ext) <- gsub("X","",colnames(X.ext))
    X.ext <- cbind(y, X.ext)
    XX <- mice(X.ext, maxit = 10, m = mm)
  }

  if(length(unique(study_data$treat)) == 2){
    study_data$treat[study_data$treat == "3"] <- "2"
  }
  
  jags_data <- list(
    N = length(y),
    treat = study_data$treat,
    Ncovariate = dim(X)[2],
    Ntreat = length(unique(study_data$treat)),
    y = y,
    #X = complete(XX, 1)[,-1]
    X = complete(XX, 1)[,2:(ncov+1)]
  )
  mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = index, n.iter = 10000)
  
  if(mm != 1){
  # add more samples
  for(i in 2:mm){
    
    jags_data <- list(
      N = length(y),
      treat = study_data$treat,
      Ncovariate = dim(X)[2],
      Ntreat = length(unique(study_data$treat)),
      y = y,
      #X = complete(XX, i)[,-1]
      X = complete(XX, i)[,2:(ncov+1)]
    )
    mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
    stats::update(mod, 1000)
    sample_more <- coda.samples(mod, variable.names = index, n.iter = 10000)
    samples <- add.mcmc(samples, sample_more)
  }
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

# revert first stage standardized coefficients to unstandardized coefficients

unstandardize_coefficients <- function(first_stage_result, study_data = NULL, X_mean = NULL, X_sd = NULL){

  if(!is.null(study_data)){
    X <- as.matrix(study_data[,c(-1,-2,-3)])
    X_mean <- apply(X, 2, mean, na.rm = TRUE)
    X_sd <- apply(X, 2, sd, na.rm = TRUE)  
  }
  
  vec_length <- length(first_stage_result$y)
  N_star <- matrix(0, nrow = vec_length, ncol = vec_length)
  
  if(is.null(study_data)){
    N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
    for(k in 1:length(X_mean)){
      N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
    }  
    for(k in 1:length(X_mean)){
      N_star[1+length(X_mean)+k,] <- c(rep(0, length(X_mean)+k), 1/X_sd[k], rep(0, length(X_mean) - k), 0)
    } 
    N_star[1+2*length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), -X_mean/X_sd, 1)
    
  } else {
    if(length(unique(study_data$treat)) == 3){
      N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
      for(k in 1:length(X_mean)){
        N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
      }  
      for(k in 1:length(X_mean)){
        N_star[1+length(X_mean)+k,] <- c(rep(0, length(X_mean)+k), 1/X_sd[k], rep(0, length(X_mean) - k), rep(0, vec_length - 1 - 2*length(X_mean)))
      }
      for(k in 1:length(X_mean)){
        N_star[1+2*length(X_mean)+k,] <- c(rep(0, 2* length(X_mean)+k), 1/X_sd[k], rep(0, length(X_mean) - k), rep(0, vec_length - 1 - 3*length(X_mean)))
      }
      N_star[1+3*length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), -X_mean/X_sd, rep(0, vec_length - 1 - 2*length(X_mean)-2), 1, 0)
      N_star[1+3*length(X_mean)+2,] <- c(rep(0, 2*length(X_mean)+1), -X_mean/X_sd, rep(0, vec_length - 1 - 3*length(X_mean)-2), 0, 1)
    } else if(length(unique(study_data$treat)) == 2){
      
      N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
      for(k in 1:length(X_mean)){
        N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
      }  
      for(k in 1:length(X_mean)){
        N_star[1+length(X_mean)+k,] <- c(rep(0, length(X_mean)+k), 1/X_sd[k], rep(0, length(X_mean) - k), 0)
      } 
      N_star[1+2*length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), -X_mean/X_sd, 1)
    }  
  }
  
  y <- N_star %*% first_stage_result$y
  Sigma <- N_star %*% solve(first_stage_result$Omega) %*% t(N_star)
  
  list(y = y, Sigma = Sigma)
}

# revert first stage standardized coefficients to unstandardized coefficients (used for model with no interaction)

unstandardize_coefficients2 <- function(first_stage_result, study_data = NULL, X_mean = NULL, X_sd = NULL){
  
  if(!is.null(study_data)){
    X <- as.matrix(study_data[,c(-1,-2,-3)])
    X_mean <- apply(X, 2, mean, na.rm = TRUE)
    X_sd <- apply(X, 2, sd, na.rm = TRUE)  
  }
  
  vec_length <- length(first_stage_result$y)
  N_star <- matrix(0, nrow = vec_length, ncol = vec_length)
  
  if(is.null(study_data)){
    N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
    for(k in 1:length(X_mean)){
      N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
    }  
    N_star[1+length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), 1)
    
  } else {
    if(length(unique(study_data$treat)) == 3){
      N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
      for(k in 1:length(X_mean)){
        N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
      }  
      N_star[1+length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), 1, 0)
      N_star[1+length(X_mean)+2,] <- c(rep(0, length(X_mean)+1), 0, 1)
    } else if(length(unique(study_data$treat)) == 2){
      
      N_star[1,] <- c(1, -X_mean/X_sd, rep(0, vec_length - 1 - length(X_mean)))
      for(k in 1:length(X_mean)){
        N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) -  k), rep(0, vec_length - 1 - length(X_mean)))
      }  
      N_star[1+length(X_mean)+1,] <- c(rep(0, length(X_mean)+1), 1)
    }  
  }
  y <- N_star %*% first_stage_result$y
  Sigma <- N_star %*% solve(first_stage_result$Omega) %*% t(N_star)
  
  list(y = y, Sigma = Sigma)
}



#second stage analysis using results from first stage analysis
secondStage <- function(y, Sigma, W = NULL, jags_file = NULL, n.iter = 200000, powerprior = FALSE, no.interaction = FALSE){
  
  data_jags <- c(y, Sigma)
  if(!is.null(W)){
    data_jags$W <- W  
  }
  if(powerprior == TRUE){
    data_jags$zero1 <- 0
    data_jags$zero2 <- 0
  }
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 20000)
  
  if(no.interaction == TRUE){
    var.names <- c("alpha", "beta", "delta")
  } else{
    var.names <- c("alpha", "beta", "gamma", "delta")    
  }

  
  if(powerprior == TRUE){
    var.names <- c(var.names, "a0")
  }
  
  samples <- coda.samples(mod, variable.names = var.names, n.iter = n.iter, n.chains = 3)
  
  return(samples) 
}
  
# function to find predictions for a specified dataset using the second stage result
# Note that this function is specific to the RA dataset and uses 9 specified covariates; i.e. will give an error if applied to other datasets
# prediction is divided by three groups according to treatments assigned
findPrediction <- function(data, result = NULL, coefs = NULL, calibration = NULL) {
  
  options(na.action='na.pass')
  abd <- model.matrix(~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, data)
  #abd_scaled <- abd
  #abd_scaled[,c(-1,-11,-12)] <- apply(abd[,c(-1,-11,-12)], 2, scale) #scale except intercept and treatment indexes
  
  g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 2),as.data.frame(abd))[,-c(1:(9+2))]
  g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 3),as.data.frame(abd))[,-c(1:(9+2))]
  
  XX <- as.matrix(cbind(abd, g2, g3))
  X <- XX[complete.cases(XX),] # use only the complete cases for predictions
  
  if(is.null(coefs)){
    coefs <- summary(result)[[1]][,"Mean"]
    coefs <- coefs[coefs != 0] #remove coefficients with zero values
  } else{
    coefs <- coefs$y
  }
  
  if(!is.null(calibration)){
    coefs[1:10] <- calibration$y[1:10]  
  }
  
  y <- data$DAS28[complete.cases(XX)]
  treat <- data$treat[complete.cases(XX)]
  pred <- X %*% coefs
  
  index1 <- X[,"treat2"] == 0 & X[,"treat3"] == 0
  index2 <- X[,"treat2"] == 1
  index3 <- X[,"treat3"] == 1
  
  y1 <- y[index1]
  y2 <- y[index2]
  y3 <- y[index3]
  
  pred1 <- pred[index1]
  pred2 <- pred[index2]
  pred3 <- pred[index3]
  
  coefs1 <- coefs * c(rep(1,10), rep(0, 2), rep(0, 9), rep(0, 9))
  pred_full_1 <- X %*% coefs1
  
  coefs2 <- coefs * c(rep(1,10), 1, 0, rep(1, 9), rep(0, 9))
  pred_full_2 <- X %*% coefs2
  
  coefs3 <- coefs * c(rep(1,10), 0, 1, rep(0, 9), rep(1, 9))
  pred_full_3 <- X %*% coefs3
  
  #calibration slope
  pred_full_11 <- pred_full_1
  
  pred_full_12 <- pred_full_2 - pred_full_1
  pred_full_12 <- pred_full_12 * (treat == "2")
  
  pred_full_13 <- pred_full_3 - pred_full_1
  pred_full_13 <- pred_full_13 * (treat == "3")
  
  pred_full_1 <- pred_full_1 * (treat == "1")
  pred_full_2 <- pred_full_2 * (treat == "2")
  pred_full_3 <- pred_full_3 * (treat == "3")
  
  return(list(y = y, y1 = y1, y2 = y2, y3 = y3, treat = treat, X = X, pred = pred,
              pred1 = pred1, pred2 = pred2, pred3 = pred3, coefs = coefs,
              pred_full_11 = pred_full_11, pred_full_12 = pred_full_12, pred_full_13 = pred_full_13, 
              pred_full_1 = pred_full_1, pred_full_2 = pred_full_2, pred_full_3 = pred_full_3))
}

# findPrediction for no interaction model
findPrediction2 <- function(data, result = NULL) {
  
  options(na.action='na.pass')
  abd <- model.matrix(~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, data)
  #abd_scaled <- abd
  #abd_scaled[,c(-1,-11,-12)] <- apply(abd[,c(-1,-11,-12)], 2, scale) #scale except intercept and treatment indexes
  
  #g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 2),as.data.frame(abd))[,-c(1:(9+2))]
  #g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 3),as.data.frame(abd))[,-c(1:(9+2))]
  
  XX <- abd
  #XX <- as.matrix(cbind(abd, g2, g3))
  X <- XX[complete.cases(XX),] # use only the complete cases for predictions
  
  coefs <- summary(result)[[1]][,"Mean"]
  coefs <- coefs[coefs != 0] #remove coefficients with zero values

  y <- data$DAS28[complete.cases(XX)]
  treat <- data$treat[complete.cases(XX)]
  pred <- X %*% coefs
  
  index1 <- X[,"treat2"] == 0 & X[,"treat3"] == 0
  index2 <- X[,"treat2"] == 1
  index3 <- X[,"treat3"] == 1
  
  y1 <- y[index1]
  y2 <- y[index2]
  y3 <- y[index3]
  
  pred1 <- pred[index1]
  pred2 <- pred[index2]
  pred3 <- pred[index3]
  
  coefs1 <- coefs * c(rep(1,10), rep(0, 2))
  pred_full_1 <- X %*% coefs1
  
  coefs2 <- coefs * c(rep(1,10), 1, 0)
  pred_full_2 <- X %*% coefs2
  
  coefs3 <- coefs * c(rep(1,10), 0, 1)
  pred_full_3 <- X %*% coefs3
    
  #calibration slope
  pred_full_11 <- pred_full_1
  
  pred_full_12 <- pred_full_2 - pred_full_1
  pred_full_12 <- pred_full_12 * (treat == "2")
  
  pred_full_13 <- pred_full_3 - pred_full_1
  pred_full_13 <- pred_full_13 * (treat == "3")
  
  pred_full_1 <- pred_full_1 * (treat == "1")
  pred_full_2 <- pred_full_2 * (treat == "2")
  pred_full_3 <- pred_full_3 * (treat == "3")
  
  return(list(y = y, y1 = y1, y2 = y2, y3 = y3, treat = treat, X = X, pred = pred,
              pred1 = pred1, pred2 = pred2, pred3 = pred3, coefs = coefs,
              pred_full_11 = pred_full_11, pred_full_12 = pred_full_12, pred_full_13 = pred_full_13, 
              pred_full_1 = pred_full_1, pred_full_2 = pred_full_2, pred_full_3 = pred_full_3))
}

findMSE <- function(y, pred){
  err_mse <- (pred-y)^2
  err_mse <- err_mse[!is.na(err_mse)]
  err_mse
}

findBias <- function(y, pred){
  err_bias <- pred-y
  err_bias <- err_bias[!is.na(err_bias)]
  err_bias
}

findRSquared <- function(y, pred){
  
  total <- (y - mean(y, na.rm = TRUE))^2 #total sum of squares
  tss <- sum(total[!is.na(total)])
  
  residual <- (y - pred)^2
  rss <- sum(residual[!is.na(residual)])#residual sum of squares
  rsquared <- 1 - rss/tss
  rsquared
}


# find performance metric based on predictions
findPerformance <- function(prediction){
  
  result <-
  with(prediction,{
    mse <- findMSE(y, pred)
    bias <- findBias(y, pred)
    
    mse1 <-  findMSE(y1, pred1)
    bias1 <- findBias(y1, pred1)
    
    mse2 <- findMSE(y2, pred2)
    bias2 <- findBias(y2, pred2)
    
    mse3 <- findMSE(y3, pred3)
    bias3 <- findBias(y3, pred3)
    
    rsquared <- findRSquared(y, pred)
    
    list(mse = mse, bias = bias, mse1 = mse1, bias1 = bias1, mse2 = mse2, bias2 = bias2, 
         mse3 = mse3, bias3 = bias3, rsquared = rsquared)
  })
  result
}

# Calibration plot
findPerformance2 <- function(prediction){
  
  result <- 
  with(prediction,{
    slope1 <- lm(y ~ pred_full_1 + pred_full_2 + pred_full_3, na.action=na.exclude)
    slope2 <- lm(y ~ pred_full_11 + pred_full_12 + pred_full_13, na.action=na.exclude)  
    list(slope1 = slope1, slope2 = slope2)
  })
  result
}
