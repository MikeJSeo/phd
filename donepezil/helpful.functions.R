
clean_data <- function(mydata, study_inclusion = NULL, Outcome = NULL,
                       covariates = NULL) {
  
  mydata <- mydata %>% filter(STUDY %in% study_inclusion) %>% 
    mutate(STUDY = as.numeric(as.factor(STUDY))) %>%
    filter(!is.na(WEEK)) 
  
  ### ADAS outcome
  if(Outcome == "ADAS_TRANSFORMED_OBS"){
    data_response <- mydata %>%  
      pivot_wider(id_cols = USUBJID, names_from = WEEK, values_from = ADAS_TRANSFORMED_OBS) %>%
      select(-"0") %>%
      rename(y4 = "4", y8 = "8", y12 = "12", y16 = "16", y20 = "20", y = "24", y6 = "6", y18 = "18")
  } else if(Outcome == "CIBIC_PLUS_TRANSFORMED"){
    data_response <- mydata %>%  
      pivot_wider(id_cols = USUBJID, names_from = WEEK, values_from = CIBIC_PLUS_TRANSFORMED) %>%
      select(-"0") %>%
      rename(y4 = "4", y8 = "8", y12 = "12", y16 = "16", y20 = "20", y = "24", y6 = "6", y18 = "18")
  } else if(Outcome == "Dropout"){
    data_response <- mydata %>% filter(WEEK == 0) %>% mutate(y = ifelse(DROPOUT == "Dropout", 1, 0)) %>% select(y) 
  }
  
  data0 <- mydata %>% filter(WEEK == 0) %>%
    select(STUDY, TRTORNOT, all_of(covariates)) %>%
    mutate(SEX = ifelse(SEX == "Female", 1, 0),
           AMNOTAP = ifelse(AMNOTAP == "Yes", 1, 0),
           treat = ifelse(TRTORNOT == "Placebo", 0, 1),
           AP = ifelse(AP == "Yes", 1, 0),
           studyid = STUDY
    ) %>%
    select(studyid, treat, all_of(covariates))
  
  
  #X$HEAD_IMAGE <- ifelse(X$HEAD_IMAGE == "Yes", 1, 0)
  #X$A_ONG <- ifelse(X$A_ONG == "Ongoing", 1, 0)
  
  data_full0 <- bind_cols(data0, data_response) #dataset with no standardization
  data_full <- data_full0
  
  mean_val <- apply(data_full[,covariates], 2, mean, na.rm = TRUE)
  sd_val <- apply(data_full[,covariates], 2, sd, na.rm = TRUE)
  data_full[,covariates] <- apply(data_full[,covariates], 2, scale)
  
  return(list(data_full0 = data_full0, data_full = data_full, mean_val = mean_val, sd_val = sd_val))
}

# Summarize each sample
summarize_each_sample <- function(samples){
  
  samples_result <- as.matrix(samples)
  samples_result <- samples_result[, colSums(samples_result != 0 ) > 0]
  
  Vars <- grep("^alpha", colnames(samples_result))
  Vars <- c(Vars, grep("^beta", colnames(samples_result)))
  
  if(!is.na(grep("^d", colnames(samples_result))[1])){
    Vars <- c(Vars,  grep("^d", colnames(samples_result))[1])
  }
  Vars <- c(Vars, grep("^gamma", colnames(samples_result)))
  
  samples_result <- samples_result[,Vars]
  y <- apply(samples_result, 2, mean)
  Sigma <- cov(samples_result)
  Omega <- solve(Sigma)
  
  return(list(y = y, Omega = Omega))
  
}


# revert standardized coefficients to unstandardized (prediction model)
unstandardize_cofficient <- function(result_dummy, data_cleaned, model = "prediction"){
  
  X_mean <- data_cleaned$mean_val
  X_sd <- data_cleaned$sd_val
  
  vec_length <- length(result_dummy$y)
  N_star <- matrix(0, nrow= vec_length, ncol = vec_length)
  
  if(model == "prediction"){
    N_star[1,] <- c(1, -X_mean/X_sd)
    for(k in 1:length(X_mean)){
      N_star[k+1,] <- c(rep(0,k), 1/X_sd[k], rep(0, length(X_mean) - k), rep(0, vec_length - 1 - length(X_mean)))
    }
  } else if(model == "IPDMA"){
    
    for(k in 1:length(study_inclusion)){
      N_star[k,k] <- 1
      N_star[k, (length(study_inclusion)+1): (length(study_inclusion) + length(X_mean)) ] <- -X_mean/X_sd
    }
    
    for(k in 1:length(X_mean)){
      N_star[length(study_inclusion)+k, length(study_inclusion)+k] <- 1/X_sd[k] 
    }
    N_star[length(study_inclusion) + length(X_mean) + 1, length(study_inclusion) + length(X_mean) + 1] <- 1
    N_star[length(study_inclusion) + length(X_mean) + 1, (length(study_inclusion) + length(X_mean) + 2): (length(study_inclusion) + 2 * length(X_mean) + 1) ] <- -X_mean/X_sd

    for(k in 1:length(X_mean)){
      N_star[length(study_inclusion) + 1 + length(X_mean) + k, length(study_inclusion) + 1 + length(X_mean) + k] <- 1/X_sd[k]
    }    
  }

  y <- N_star %*% result_dummy$y
  Sigma <- N_star %*% solve(result_dummy$Omega) %*% t(N_star)
  
  list(y = y, Sigma = Sigma)
}


crossvalidate <- function(crossdata, modelname){
  
  nstudy <- length(unique(crossdata$studyid))
  performances <- matrix(NA, nrow = 3, ncol = nstudy)
  
  for(i in 1:nstudy){
    training_set <- crossdata %>% filter(studyid != i)
    testing_set <- crossdata %>% filter(studyid == i)
    
    if(modelname == "lm"){
      lmfit <- lm(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = training_set)
      lmfit_coef <- summary(lmfit)$coef[,1]
      bb <- model.matrix(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = testing_set)
      prediction <- bb %*% lmfit_coef
    } else if(modelname == "lmer"){
      lmerfit <- lmer(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE + (1|studyid), data = training_set)
      bb <- model.matrix(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = testing_set)
      prediction <- bb %*% fixef(lmerfit)
    } else if(modelname == "ridge"){
      data_glmnet <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = training_set)
      data_glmnet <- cbind(y = training_set$y, data_glmnet = data_glmnet)

      lambdas <- 10^seq(3, -3, by = -.1)
      cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), family = "gaussian", type.measure ="mse", standardize = FALSE, alpha = 0, lambda = lambdas)
      #coef(cvfit, s = "lambda.min")
      bb <- model.matrix(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = testing_set)
      prediction <- bb %*% coef(cvfit, s = "lambda.min")
    } else if(modelname == "bayes"){

      bb <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = training_set) 
      
      data.JAGS <- 
        list(
          Nstudies = length(study_inclusion),
          Ncovariate = length(colnames(bb)),
          y = training_set$y,
          X = bb,
          Np = length(training_set$y),
          studyid = training_set$studyid)
      
      mod <- jags.model(file = "prediction model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
      stats::update(mod, 1000)
      samples <- coda.samples(mod, variable.names = c("alpha", "beta", "sigma"), n.iter = 10000)
      
      coefs <- summary(samples)$statistics[,1]
      bayeslasso_coefs <- coefs[c("alpha", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]", "beta[6]", "beta[7]")]
      
      bb <- model.matrix(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = testing_set)
      prediction <- bb %*% bayeslasso_coefs
    } else if(modelname == "bayes_binom"){
      
      bb <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = training_set) 
      
      data.JAGS <- 
        list(
          Nstudies = length(study_inclusion),
          Ncovariate = length(colnames(bb)),
          y = training_set$y,
          X = bb,
          Np = length(training_set$y),
          studyid = training_set$studyid)
      
      mod <- jags.model(file = "prediction model binary.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
      stats::update(mod, 1000)
      samples <- coda.samples(mod, variable.names = c("alpha", "beta"), n.iter = 10000)
      
      coefs <- summary(samples)$statistics[,1]
      bayeslasso_coefs <- coefs[c("alpha", "beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]", "beta[6]", "beta[7]")]
      
      bb <- model.matrix(y ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = testing_set)
      prediction0 <- bb %*% bayeslasso_coefs
      prediction <- exp(prediction0) / (1+exp(prediction0))
    }
    
    if(modelname != "bayes_binom"){
      performances[1,i] <- findMSE(testing_set$y, prediction) 
      performances[2,i] <- findMAE(testing_set$y, prediction)
      performances[3,i] <- findRsquared(testing_set$y, prediction)
    } else{
      performances[1,i] <- c(roc(testing_set$y, c(prediction))$auc)
    }
    
  }
  return(performances)
}



findRsquared <- function(y, pred){
  
  total <- (y - mean(y, na.rm = TRUE))^2
  tss <- sum(total[!is.na(total)])
  residual <- (y - pred)^2
  rss <- sum(residual[!is.na(residual)])
  rsquared <- 1 - rss/tss
  rsquared
}

findMAE <- function(y, pred){
  err_MAE <- abs(pred - y)
  err_MAE <- err_MAE[!is.na(err_MAE)]
  mean(err_MAE)
}

findMSE <- function(y, pred){
  err_mse <- (pred-y)^2
  err_mse <- err_mse[!is.na(err_mse)]
  mean(err_mse)
}

add.mcmc <- function(x, y){
  
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