

# Given simulated training data, estimate a model using naive method and calculates predictions using the testing data.
naive_prediction <- function(traindata, testdata){
  
  nstudy <- length(unique(testdata$study))
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  if(length(covariates) == 5){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous")
    names(typeofvar) <- paste0("x", 1:5)
  } else if(length(covariates) == 10){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
    names(typeofvar) <- paste0("x", 1:10)
  }
  
  #Find missing data pattern such as systematically missing predictors, sporadically missing predictors
  missingPattern <- findMissingPattern(traindata, covariates, typeofvar, 
                                       studyname = "study", treatmentname = "treat", outcomename = "y")
  missingPatternTest <- findMissingPattern(testdata, covariates, typeofvar, 
                                           studyname = "study", treatmentname = "treat", outcomename = "y")
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
    predictions[[studyid]] <- c(bb %*% fixef(trained_model))
  }
  return(predictions)
}


# Given simulated training data, estimate a model using imputation method and calculates predictions using the testing data.
imputation_prediction <- function(traindata, testdata){
  
  nstudy <- length(unique(testdata$study))
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  if(length(covariates) == 5){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous")
    names(typeofvar) <- paste0("x", 1:5)
  } else if(length(covariates) == 10){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
    names(typeofvar) <- paste0("x", 1:10)
  }
  
  imputationapproach <- ipdma.impute(traindata, covariates = covariates, typeofvar = typeofvar, interaction = interaction,
                                     studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
  imp.list <- imputationapproach$imp.list

  for(studyid in 1:nstudy){
    
    testdata_dummy <- testdata %>% filter(study == studyid)
    prediction.dummy <- matrix(NA, nrow = dim(testdata_dummy)[1], ncol = length(imp.list))
    
    missingPatternTest <- findMissingPattern(testdata_dummy, covariates, typeofvar, 
                                         studyname = "study", treatmentname = "treat", outcomename = "y")
    
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
  return(predictions)
}


# Given simulated training data, estimate a model using imputation method and calculates predictions using the testing data.
separate_prediction <- function(traindata, testdata){
  
  nstudy <- length(unique(testdata$study))
  nstudy2 <- length(unique(traindata$study))
  
  predictions <- list()
  
  #If the dataset contains treatment, it is assumed to have interaction terms as well
  interaction <- "treat" %in% colnames(traindata)
  
  covariates <- names(traindata)
  covariates <- covariates[grepl("x", covariates)]
  
  if(length(covariates) == 5){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous")
    names(typeofvar) <- paste0("x", 1:5)
  } else if(length(covariates) == 10){
    typeofvar <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
    names(typeofvar) <- paste0("x", 1:10)
  }
  
  for(studyid in 1:nstudy){
    
    testdata_dummy <- testdata %>% filter(study == studyid)
    
    missingPatternTest <- findMissingPattern(testdata, covariates, typeofvar, 
                                         studyname = "study", treatmentname = "treat", outcomename = "y")
    prediction_store <- matrix(NA, dim(testdata_dummy)[1], nstudy2)
    precision_store <- matrix(NA, dim(testdata_dummy)[1], nstudy2)
    
    for(studyid2 in 1:nstudy2){
      
      traindata_dummy <- traindata %>% filter(study == studyid2)
      missingPattern <- findMissingPattern(traindata_dummy, covariates, typeofvar, 
                                               studyname = "study", treatmentname = "treat", outcomename = "y")
      
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
    
    predictions[[studyid]] <- apply(final_store, 1, sum)
    #prediction[[studyid]] <- apply(prediction_store, 1, mean)
  }
  return(predictions)
}



#wrapper function to calculate performance metrics for multiple simulations (i.e. Nsim = 100)
wrapper_function <- function(Nstudies = NULL, Ncov = NULL, sys_missing_prob = NULL, signal = NULL, sign = NULL, interaction = NULL, Nsim = 100){

  naive_store <- matrix(NA, nrow = Nsim, ncol = 3)
  imputation_store <- matrix(NA, nrow = Nsim, ncol = 3)
  separate_store <- matrix(NA, nrow = Nsim, ncol = 3)
  
  for(i in 1:Nsim){
    
    # NEED TO FIX###############################
    simulated_data <- generate_sysmiss_ipdma_example(Nstudies = Nstudies, Ncov = Ncov, sys_missing_prob = sys_missing_prob,
                                                      signal = signal, sign = sign, interaction = interaction)
    simulated_dataset <- simulated_data$dataset
    
    validation_data <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, signal = "small", sign = "same", interaction = FALSE)
    validation_dataset <- validation_data$dataset
    
    naivepred <- naive_prediction(simulated_dataset, validation_dataset)
    imputationpred <- imputation_prediction(simulated_dataset, validation_dataset)
    separatepred <- separate_prediction(simulated_dataset, validation_dataset)
    
    
    set.seed(i)
    simulated_data <- generate_simulation_data(Nstudies = Nstudies, Ncov = Ncov, sys_missing_prob = sys_missing_prob, nonlinear = nonlinear, signal = signal, interaction = interaction, heterogeneity = heterogeneity) 
    simulated_dataset <- simulated_data$dataset
    
    validation_data <- generate_simulation_data(Nstudies = Nstudies, Ncov = Ncov, sys_missing_prob = sys_missing_prob, nonlinear = nonlinear, signal = signal, interaction = interaction, heterogeneity = heterogeneity)
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








