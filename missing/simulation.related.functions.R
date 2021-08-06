
wrapper_function <- function(Nstudies = NULL, Ncov = NULL, sys_missing_prob = NULL, nonlinear = NULL, signal = NULL, interaction = NULL, heterogeneity = NULL, Nsim = 100){

  naive_store <- matrix(NA, nrow = Nsim, ncol = 3)
  imputation_store <- matrix(NA, nrow = Nsim, ncol = 3)
  separate_store <- matrix(NA, nrow = Nsim, ncol = 3)
  
  for(i in 1:Nsim){
    
    set.seed(i)
    simulated_data <- generate_simulation_data(Nstudies = Nstudies, Ncov = Ncov, sys_missing_prob = sys_missing_prob, nonlinear = nonlinear, signal = signal, interaction = interaction, heterogeneity = heterogeneity) 
    simulated_dataset <- simulated_data$dataset
    
    validation_data <- generate_simulation_data(Nstudies = 10, Ncov = Ncov, sys_missing_prob = sys_missing_prob, nonlinear = nonlinear, signal = signal, interaction = interaction, heterogeneity = heterogeneity)
    validation_dataset <- validation_data$dataset

    # naive method
    naivepred <- naive_prediction(simulated_dataset, validation_dataset)
    naiveperf <- findPerformance(validation_dataset$y, naivepred)
    naive_store[i,] <- naiveperf
      
    # imputation
    imputationpred <- try(imputation_prediction(simulated_dataset, validation_dataset, type_of_var = type_of_var))
    imputationperf <- try(findPerformance(validation_dataset$y, imputationpred))
    imputation_store[i,] <- imputationperf

    # separate predictions
    separatepred <- separate_prediction(simulated_dataset, validation_dataset)
    separateperf <- findPerformance(validation_dataset$y, separatepred)
    separate_store[i,] <- separateperf 

  }
  
  imputation_store <- apply(imputation_store, 2, as.numeric)
  failed_trials <- which(rowSums(is.na(imputation_store)) > 0)
  print(paste("failed # of trials: ", length(failed_trials)))
  
  if(length(failed_trials) != 0){
    imputation_store <- imputation_store[-failed_trials,]
    naive_store <- naive_store[-failed_trials,]
    separate_store <- separate_store[-failed_trials,]
  }

  print(round(apply(naive_store, 2, mean), digits = 3))
  print(round(apply(imputation_store, 2, mean), digits = 3))
  print(round(apply(separate_store, 2, mean), digits = 3))
  
  return_matrix <- matrix(NA, 3, 3)
  return_matrix[1,] <- round(apply(naive_store, 2, mean), digits = 3)
  return_matrix[2,] <- round(apply(imputation_store, 2, mean), digits = 3)
  return_matrix[3,] <- round(apply(separate_store, 2, mean), digits = 3)
  return(return_matrix)
  #return(list(naive_store = naive_store, imputation_store = imputation_store, separate_store = separate_store))
}


generate_simulation_data <- function(Nstudies = NULL, Ncov = NULL, sys_missing_prob = NULL, nonlinear = NULL, signal = NULL, interaction = NULL, heterogeneity = NULL){
  
  Npatients <- sample(150:500, Nstudies, replace = TRUE)
  Npatients.tot <- sum(Npatients)
  study <- rep(1:Nstudies, times = Npatients)
  
  a <- runif(Nstudies, 0.5, 1.5)
  a <- rep(a, times = Npatients)
  
  ## generate covariates
  rho <- 0.2
  Omega <- diag(1, Ncov)
  for(i in 1:Ncov){
    for(j in 1:Ncov){
      Omega[i,j] <- rho^abs(i - j) 
    }
  }
  sigma2 <- 1
  
  X <- NULL
  for(i in 1:Nstudies){
    mu <- runif(Ncov, -0.5, 0.5)
    X <- rbind(X, rmvnorm(Npatients[i], mu, Omega * sigma2))
  }
  
  if(nonlinear == "yes"){
    X[,4] <- X[,4] + X[,1]^2 - X[,2]^2
    X[,5] <- X[,1] / 2*X[,2]^2

    if(Ncov == 10){
      X[,8] <- 2*X[,8] + X[,1]^2 - X[,2]^2
      X[,9] <- X[,1] / X[,2]^2
    }
  }
  
  #categorize predictors
  if(Ncov == 5){
    X[,2] <- ifelse(X[,2] > 0, 1, 0)
    X[,3] <- ifelse(X[,3] > 1, 1, 0)
  } else if(Ncov == 10){
    X[,2] <- ifelse(X[,2] > 0, 1, 0)
    X[,3] <- ifelse(X[,3] > 0, 1, 0)
    X[,8] <- ifelse(X[,8] > 0, 1, 0)
    X[,9] <- ifelse(X[,9] > 0.5, 1, 0)
    X[,10] <- ifelse(X[,10] > 1, 1, 0)
  }

  if(signal == "small"){
    e_vec <- rnorm(Npatients.tot, 0, 1.0) # R squared around 0.1
  } else if(signal == "large"){
    e_vec <- rnorm(Npatients.tot, 0, 0.2) # R squared around 0.6
  }
  
  if(nonlinear == "yes"){
    X <- cbind(X, X[,1]^2, X[,1]^3)
  }
  
  b <- matrix(NA, Npatients.tot, Ncov)

  for(i in 1:Ncov){
    if(heterogeneity == "small"){
      b_dummy <- rnorm(Nstudies, 0.5, 0.1)  
    } else if(heterogeneity == "large"){
      b_dummy <- rnorm(Nstudies, 0.5, 0.5)
    }
    b_dummy <- rep(b_dummy, times = Npatients)
    b[,i] <- b_dummy
  }
  
  if(nonlinear == "yes"){

    b2 <- matrix(NA, Npatients.tot, 2)

    for(i in 1:2){
      if(heterogeneity == "small"){
        b_dummy2 <- rnorm(Nstudies, 0.5, 0.1)
      } else if(heterogeneity == "large"){
        b_dummy2 <- rnorm(Nstudies, 0.5, 0.5)
      }
      b_dummy2 <- rep(b_dummy2, times = Npatients)
      b2[,i] <- b_dummy2
    }
    b <- cbind(b, b2)
  }
  
  if(interaction == "yes"){
    treat <- rbinom(Npatients.tot, 1, 0.5)
    Xinteraction <- X[,1:Ncov] * treat
    cvec <- matrix(NA, Npatients.tot, Ncov)
    
    for(i in 1:Ncov){
      if(heterogeneity == "small"){
        cvec_dummy <- rnorm(Nstudies, 0.25, 0.1)  
      } else if(heterogeneity == "large"){
        cvec_dummy <- rnorm(Nstudies, 0.25, 0.5)
      }
      cvec_dummy <- rep(cvec_dummy, times = Npatients)
      cvec[,i] <- cvec_dummy
    }
    
    d <- rnorm(Nstudies, 1, 0.5)
    d <- rep(d, times = Npatients)
  }
  
  if(interaction == "no"){
    y <- a + apply(X * b, 1, sum) + e_vec    
  } else if(interaction == "yes"){
    y <- a + apply(X * b, 1, sum) + e_vec + d *treat + apply(Xinteraction * cvec, 1, sum)
  }
  

  # introduce systematically missing; first two predictors are always observed
  for(i in 3:Ncov){
    systematic_missing_study <- sample(Nstudies, size = sys_missing_prob*Nstudies)
    systematic_missing_study_dummy <- rep(0, Nstudies)
    systematic_missing_study_dummy[systematic_missing_study] <- 1
    
    study_missing <- as.logical(rep(systematic_missing_study_dummy, times = Npatients))  
    X[,i][study_missing] <- NA
  }    
    
  dataset <- data.frame(y = y, X = X[,1:Ncov], study = study)
  colnames(dataset) <- c("y", paste0("x", 1:Ncov), "study")
  dataset <- as_tibble(dataset)
  
  if(Ncov == 5){
    dataset <- dataset %>% mutate(x2 = as.factor(x2),
                                  x3 = as.factor(x3))
  } else if(Ncov == 10){
    dataset <- dataset %>% mutate(x2 = as.factor(x2),
                                  x3 = as.factor(x3),
                                  x8 = as.factor(x8),
                                  x9 = as.factor(x9),
                                  x10 = as.factor(x10)
    )
  }  
  
  if(interaction == "yes"){
    dataset <- cbind(dataset, treat = treat)
    dataset <- as_tibble(dataset)
    return(list(y = y , X = X, study = study, treat = treat, dataset = dataset))
  } else{
    dataset <- as_tibble(dataset)
    return(list(y = y , X = X, study = study, dataset = dataset))
  }
}


naive_prediction <- function(traindata, testdata){
  
  nstudy <- length(unique(testdata$study))
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  #Find missing data pattern such as systematically missing predictors, sporadically missing predictors
  missingPattern <- findMissingPattern(traindata, covariates)
  missingPatternTest <- findMissingPattern(testdata, covariates)
  without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)

  if(interaction == FALSE){
    form <- as.formula(paste0("y ~ ", paste(without_sys_cov, collapse= "+"), " + (1|study) " ))
  } else {
    form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
  }
  trained_model <- lmer(form, data = traindata)
  
  if(interaction == FALSE){
    form2 <- as.formula(paste0("y ~ ", paste(without_sys_cov, collapse= "+")))
  } else {
    form2 <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
  }
  
  for(studyid in 1:nstudy){
  
    testdata_dummy <- testdata %>% filter(study == studyid)
  
    bb <- model.matrix(form2, data = testdata_dummy)
    predictions[[studyid]] <- bb %*% fixef(trained_model)
  }
  return(unlist(predictions))
}


imputation_prediction <- function(traindata, testdata, type_of_var){
  
  nstudy <- length(unique(testdata$study))
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  #Find missing data pattern such as systematically missing predictors, sporadically missing predictors
  missingPattern <- findMissingPattern(traindata, covariates)

  if(interaction == TRUE){
    traindata <- createinteractions(traindata, covariates)
  }
  
  meth <- getCorrectMeth(traindata, missingPattern, method = "imputation", type_of_var)
  pred <- getCorrectPred(traindata, missingPattern, method = "imputation")

  imp <- mice(traindata, pred = pred, meth = meth)
  impc <- complete(imp, "long", include = "TRUE")
  
  imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations

  for(studyid in 1:nstudy){
    
    testdata_dummy <- testdata %>% filter(study == studyid)
    prediction.dummy <- matrix(NA, nrow = dim(testdata_dummy)[1], ncol = length(imp.list))
    
    missingPatternTest <- findMissingPattern(testdata_dummy, covariates)  
    
    if(interaction == FALSE){
      form <- as.formula(paste0("y ~ ", paste(missingPatternTest$without_sys_covariates, collapse= "+"), " + (1|study) "))
    } else {
      form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
    }
    
    if(interaction == FALSE){
      form2 <- as.formula(paste0("y ~ ", paste(missingPatternTest$without_sys_covariates, collapse= "+")))
    } else {
      form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
    }
    
    for(ii in 1:length(imp.list)){
      imp.dummy <- imp.list[[ii]]
      imp.model <- lmer(form, data = imp.dummy)
      
      bb <- model.matrix(form2, data = testdata_dummy)
      prediction.dummy[,ii] <- bb %*% fixef(imp.model)
    }
    predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
  }
  return(unlist(predictions))
}


separate_prediction <- function(traindata, testdata){
  
  nstudy <- length(unique(testdata$study))
  nstudy2 <- length(unique(traindata$study))
  
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  #Find missing data pattern such as systematically missing predictors, sporadically missing predictors
  missingPattern <- findMissingPattern(traindata, covariates)
  
  for(studyid in 1:nstudy){
    
    testdata_dummy <- testdata %>% filter(study == studyid)
    missingPatternTest <- findMissingPattern(testdata_dummy, covariates)
    prediction_store <- matrix(NA, dim(testdata_dummy)[1], nstudy)
    precision_store <- matrix(NA, dim(testdata_dummy)[1], nstudy)
    
    for(studyid2 in 1:nstudy2){
      
      traindata_dummy <- traindata %>% filter(study == studyid2)
      missingPattern <- findMissingPattern(traindata_dummy, covariates)
      
      #use covariates that are not systematically missing in both training and testing dataset
      without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
      
      if(interaction == FALSE){
        form <- as.formula(paste0("y ~ ", paste(without_sys_cov, collapse= "+")))
      } else {
        form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat"))
      }
      trained_model <- lm(form, data = traindata_dummy)
      
      bb <- model.matrix(form, data = testdata_dummy)
      prediction_store[,studyid2] <- bb %*% coef(trained_model)
      precision_store[,studyid2] <- 1/diag(bb %*% vcov(trained_model) %*% t(bb))
    }
    product_store <- prediction_store * precision_store
    precision_vec <- apply(precision_store, 1, sum)
    
    final_store <- sweep(product_store, 1, precision_vec, `/`)
    
    #prediction[[studyid]] <- apply(prediction_store, 1, mean)
    predictions[[studyid]] <- apply(final_store, 1, sum)
    
  }
  return(unlist(predictions))
}


