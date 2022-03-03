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

  y <- N_star %*% first_stage_result$y
  Sigma <- N_star %*% solve(first_stage_result$Omega) %*% t(N_star)
  
  list(y = y, Sigma = Sigma)
}


######## first stage model
firstStage <- function(study_data, jags_file, mm = 20, index = c("a", "b", "c", "d", "sigma"), scale = TRUE){
  
  #imputation steps
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

  if(length(unique(study_data$treat)) == 2){
    study_data$treat[study_data$treat == "3"] <- "2"
  }
  
  # Model fitting step
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
  stats::update(mod, 1000)
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


#second stage analysis using results from first stage analysis
secondStage <- function(y, Sigma, W = NULL, jags_file = NULL, n.iter = 200000){
  
  data_jags <- c(y, Sigma)
  if(!is.null(W)){
    data_jags$W <- W  
  }
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 20000)
  
  var.names <- c("gamma", "delta")
  #var.names <- c("gamma", "d")    #for random effects
  
  samples <- coda.samples(mod, variable.names = var.names, n.iter = n.iter, n.chains = 3)
  
  return(samples) 
}


find.treatment.effect <- function(samples, newpatient, quantiles = c(0.025, 0.5, 0.975)){
  
  store_result <- list()
  for(ii in 1:2){
    
    index0 <- which(colnames(samples[[1]]) == paste0("delta[", ii, "]"))  
    index1 <- grep(paste0("gamma\\[", ii, ","), colnames(samples[[1]]))
    index <- c(index0, index1)
    samples2 <- samples[,index]
    
    merged <- samples2[[1]]
    for(i in 2:length(samples2)){
      merged <- rbind(merged, samples2[[i]])
    }
    pred <- merged %*% c(1, newpatient)
    
    CI <- quantile(pred, probs = quantiles)
    names(CI) <- quantiles
    store_result[[paste0("treatment ", ii)]] <- CI
  }
  return(store_result)
}