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

firstStage <- function(study_data, jags_file, mm = 20, index = c("alpha", "beta", "gamma", "delta", "sigma")){
  
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

  row_names <- rownames(summary(samples)[[1]])
  result <- summary(samples)[[1]][,c(1,2)]
  rounded_result <- apply(result, 2, round, digits = 2)
  raw <- as.data.frame(cbind(row_names, result))
  rounded <- as.data.frame(cbind(row_names, rounded_result))
  write_xlsx(raw, path =  paste0(jags_file,"-", study_data$study,"-raw.xlsx"))
  write_xlsx(rounded, path = paste0(jags_file,"-", study_data$study,"-rounded.xlsx"))

  return(samples)
}


find_jags_data0 <- function(samples){
  
  samples_result <- as.matrix(samples)
  samples_result <- samples_result[, colSums(samples_result != 0) > 0]
  
  Vars <- grep("^alpha", colnames(samples_result))
  Vars <- c(Vars, grep("^beta", colnames(samples_result)))
  Vars <- c(Vars, grep("^gamma", colnames(samples_result)))
  Vars <- c(Vars, grep("^delta", colnames(samples_result)))

  samples_result <- samples_result[,Vars]
  y <- apply(samples_result, 2, mean)
  Sigma <- cov(samples_result)
  Omega <- solve(Sigma)
  
  return(list(y = y, Omega = Omega))
}


secondStage <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL, samples4 = NULL,
                         y5 = NULL, Omega5 = NULL, jags_file = NULL, univariate = FALSE, w = NULL){
  
  r1 <- find_jags_data0(samples1)
  r2 <- find_jags_data0(samples2)
  r3 <- find_jags_data0(samples3)
  r4 <- find_jags_data0(samples4)
  
  if(univariate == TRUE){
    r1[[2]] <- diag(diag(r1[[2]]))
    r2[[2]] <- diag(diag(r2[[2]]))
    r3[[2]] <- diag(diag(r3[[2]]))
    r4[[2]] <- diag(diag(r4[[2]]))
    Omega5 <- diag(diag(Omega5))
  }
  
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
         Omega5 = Omega5,
         w = w)
  
  if(!is.null(w)){
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
           w = w)
  }
  
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "d"), n.iter = 10000)
  
  return(samples)
}

predictFn <- function(sample, result, ncov = 9) {
  
   # sample <- sample %>% filter(!is.na(DAS28))
  
  options(na.action='na.pass')
  abd <- model.matrix(~female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, sample)
  abd_scaled <- abd
  abd_scaled[,-1] <- apply(abd[,-1], 2, scale)
  
  g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(sample$treat == 2),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(sample$treat == 3),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  
  XX <- as.matrix(cbind(abd_scaled, g2, g3))
  XX2 <- XX[complete.cases(XX),]
  
  coefs <- summary(result)[[1]][,"Mean"]
  coefs <- coefs[coefs != 0]
  pred <- XX2 %*% coefs
  
  y <- sample$DAS28[complete.cases(XX)]
  err <- (y - pred)^2
  err <- err[!is.na(err)] 
  
  MSE <- mean(err, na.rm = TRUE)
  return(list(pred = pred, y = y , err = err, MSE = MSE))
}