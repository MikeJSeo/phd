#helpful functions

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
    sample_more <- coda.samples(mod, variable.names = index, n.iter = 10000)
    samples <- add.mcmc(samples, sample_more)
  }
  return(samples)
}


find_jags_data0 <- function(samples){
  
  samples_result <- as.matrix(samples)
  samples_result <- samples_result[, colSums(samples_result != 0) > 0]
  
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


secondStage <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL, samples4 = NULL,
                         y5 = NULL, Omega5 = NULL, jags_file = NULL, w = NULL, niter = 10000){
  
  r1 <- find_jags_data0(samples1)
  r2 <- find_jags_data0(samples2)
  r3 <- find_jags_data0(samples3)
  r4 <- find_jags_data0(samples4)
  
  if(!is.null(w)){
    if(w != 0){
      
      W <- matrix(sqrt(w), nrow = 30, ncol = 30)
      W[1:10,1:10] <- 1
      W[11:30,11:30] <- w

      data_jags <-
        list(y1 = r1[[1]],
             y2 = r2[[1]],
             y3 = r3[[1]][-(1:10)],
             y4 = r4[[1]][-(1:10)],
             y5 = y5[-(1:10)],
             Omega1 = r1[[2]],
             Omega2 = r2[[2]],
             Omega3 = r3[[2]][11:20, 11:20],
             Omega4 = r4[[2]][11:20, 11:20],
             Omega5 = Omega5[11:20, 11:20],
             W = W)  
    } else if (w == 0){
      data_jags <-
        list(y1 = r1[[1]][1:10],
             y2 = r2[[1]][1:10],
             y3 = r3[[1]][-(1:10)],
             y4 = r4[[1]][-(1:10)],
             y5 = y5[-(1:10)],
             Omega1 = r1[[2]][1:10,1:10],
             Omega2 = r2[[2]][1:10,1:10],
             Omega3 = r3[[2]][11:20, 11:20],
             Omega4 = r4[[2]][11:20, 11:20],
             Omega5 = Omega5[11:20, 11:20])  
    } 
  } else{ 
    data_jags <-
        list(y1 = r1[[1]],
             y2 = r2[[1]],
             y3 = r3[[1]],
             y4 = r4[[1]],
             y5 = y5,
             Omega1 = r1[[2]],
             Omega2 = r2[[2]],
             Omega3 = r3[[2]],
             Omega4 = r4[[2]],
             Omega5 = Omega5)
  }
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta"), n.iter = niter, n.chains = 3)
  
  return(samples)
}


#second stage analysis for external validation
secondStage2 <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL,
                        y4 = NULL, Omega4 = NULL, jags_file = NULL, univariate = FALSE, w = NULL, niter = 10000){
  
  r1 <- find_jags_data0(samples1)
  r2 <- find_jags_data0(samples2)
  r3 <- find_jags_data0(samples3)
  
  if(!is.null(w)){
    if(w != 0){
      
      W <- matrix(sqrt(w), nrow = 30, ncol = 30)
      W[1:10,1:10] <- 1
      W[11:30,11:30] <- w
      
      data_jags <-
        list(y1 = r1[[1]],
             y2 = r2[[1]][-(1:10)],
             y3 = r3[[1]][-(1:10)],
             y4 = y4[-(1:10)],
             Omega1 = r1[[2]],
             Omega2 = r2[[2]][11:20, 11:20],
             Omega3 = r3[[2]][11:20, 11:20],
             Omega4 = Omega4[11:20, 11:20],
             W = W)  
    } else if (w == 0){
      data_jags <-
        list(y1 = r1[[1]][1:10],
             y2 = r2[[1]][-(1:10)],
             y3 = r3[[1]][-(1:10)],
             y4 = y4[-(1:10)],
             Omega1 = r1[[2]][1:10,1:10],
             Omega2 = r2[[2]][11:20, 11:20],
             Omega3 = r3[[2]][11:20, 11:20],
             Omega4 = Omega4[11:20, 11:20])  
    } 
  } else{ 
    data_jags <-
      list(y1 = r1[[1]],
           y2 = r2[[1]],
           y3 = r3[[1]],
           y4 = y4,
           Omega1 = r1[[2]],
           Omega2 = r2[[2]],
           Omega3 = r3[[2]],
           Omega4 = Omega4)
  }
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta"), n.iter = niter, n.chains= 3)
  
  return(samples)
}



predictFn <- function(data, result, ncov = 9, measure = "mse") {
  
  options(na.action='na.pass')
  abd <- model.matrix(~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, data)
  abd_scaled <- abd
  abd_scaled[,c(-1,-11,-12)] <- apply(abd[,c(-1,-11,-12)], 2, scale)
  
  g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 2),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 3),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  
  XX <- as.matrix(cbind(abd_scaled, g2, g3))
  X <- XX[complete.cases(XX),]
  
  coefs <- summary(result)[[1]][,"Mean"]
  coefs <- coefs[coefs != 0]
  y <- data$DAS28[complete.cases(XX)]
  treat <- data$treat[complete.cases(XX)]
  
  index0 <- rep(TRUE, length(y))
  index1 <- X[,"treat2"] == 0 & X[,"treat3"] == 0
  index2 <- X[,"treat2"] == 1
  index3 <- X[,"treat3"] == 1
  
  X_full <- cbind(abd_scaled, abd_scaled[,2:10], abd_scaled[,2:10]) 
  X_full <- X_full[complete.cases(X_full),]
  
  if(measure %in% c("mse","bias")){
    full <- findPerformanceMetric(X, y, coefs, index0, measure)
    DMARDs <- findPerformanceMetric(X, y, coefs, index1, measure)
    RTX <- findPerformanceMetric(X, y, coefs, index2, measure)
    TCZ <- findPerformanceMetric(X, y, coefs, index3, measure)  
    return(list(full = full, DMARDs = DMARDs, RTX = RTX, TCZ = TCZ))
  } else if(measure %in% c("calibration")){
    full <- findPerformanceMetric2(X_full, y, coefs, treat, measure)
    return(full)
    
  }
}


findPerformanceMetric <- function(X, y, coefs, index, measure){
  
  pred <- X[index,] %*% coefs
  y <- y[index]
  
  if(measure == "mse"){
    err_mse <- (pred - y)^2
    err_mse <- err_mse[!is.na(err_mse)]
    mse <- mean(err_mse, na.rm = TRUE)
    return(mse)
  } else if(measure == "bias"){
    err_bias <- pred - y
    err_bias <- err_bias[!is.na(err_bias)]
    bias <- mean(err_bias, na.rm = TRUE)
    return(bias)
  }
}

findPerformanceMetric2 <- function(X_full, y, coefs, treat, measure){
  
  if(measure == "calibration"){
 
    coefs1 <- coefs * c(rep(1,10), rep(0, 2), rep(0, 9), rep(0, 9))
    pred1 <- X_full %*% coefs1
    
    coefs2 <- coefs * c(rep(1,10), 1, 0, rep(1, 9), rep(0, 9))
    pred2 <- X_full %*% coefs2
    
    coefs3 <- coefs * c(rep(1,10), 0, 1, rep(0, 9), rep(1, 9))
    pred3 <- X_full %*% coefs3
    
    pred <- cbind(pred1, pred2, pred3)
    
    best_treat <- apply(pred, 1, function(x) which.min(x))
    
    pred_matrix <- matrix(NA, nrow = 3, ncol = 3)
    pred_matrix[1,] <- c(mean(pred[best_treat == 1,1]), mean(pred[best_treat == 1,2]), mean(pred[best_treat == 1,3]))
    pred_matrix[2,] <- c(mean(pred[best_treat == 2,1]), mean(pred[best_treat == 2,2]), mean(pred[best_treat == 2,3]))
    pred_matrix[3,] <- c(mean(pred[best_treat == 3,1]), mean(pred[best_treat == 3,2]), mean(pred[best_treat == 3,3]))

    n <- table(best_treat,treat)
    
    obs_matrix <- matrix(NA, nrow = 3, ncol = 3)
    obs_matrix[1,] <- c(mean(y[best_treat ==1 & treat == "1"], na.rm =TRUE), mean(y[best_treat ==1 & treat == "2"], na.rm =TRUE), mean(y[best_treat ==1 & treat == "3"], na.rm =TRUE))
    obs_matrix[2,] <- c(mean(y[best_treat ==2 & treat == "1"], na.rm =TRUE), mean(y[best_treat ==2 & treat == "2"], na.rm =TRUE), mean(y[best_treat ==2 & treat == "3"], na.rm =TRUE))
    obs_matrix[3,] <- c(mean(y[best_treat ==3 & treat == "1"], na.rm =TRUE), mean(y[best_treat ==3 & treat == "2"], na.rm =TRUE), mean(y[best_treat ==3 & treat == "3"], na.rm =TRUE))

    #calibration slope
    pred12 <- pred[,2] - pred[,1]
    pred12 <- pred12 * (treat == "2")
    pred13 <- pred[,3] - pred[,1]
    pred13 <- pred13 * (treat == "3")
    slope1 <- lm(y ~ pred12 + pred13, na.action=na.exclude)
    
    pred1 <- pred[,1] * (treat == "1")
    pred2 <- pred[,2] * (treat == "2")
    pred3 <- pred[,3] * (treat == "3")
    slope2 <- lm(y ~ -1 + pred1 + pred2 + pred3, na.action=na.exclude)
    
    list(pred_matrix = pred_matrix, obs_matrix = obs_matrix, n = n, slope1 = slope1, slope2 = slope2)
    
  }
}

