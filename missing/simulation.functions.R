
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


imputation_prediction <- function(traindata, testdata, method = "imputation"){
  
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
  
  if(method == "imputation"){
    imputationapproach <- ipdma.impute(traindata, covariates = covariates, typeofvar = typeofvar, interaction = interaction,
                                       studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)    
  } else if(method == "imputation_nocluster"){
    imputationapproach <- ipdma.impute(traindata, covariates = covariates, typeofvar = typeofvar, sys_impute_method = "pmm", interaction = interaction,
                                       studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)   
  }

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
      precision_store[,studyid2] <- 1/(diag(bb %*% vcov(trained_model) %*% t(bb))+ sigma(trained_model)^2)
    }
    product_store <- prediction_store * precision_store
    precision_vec <- apply(precision_store, 1, sum)
    
    final_store <- sweep(product_store, 1, precision_vec, `/`)
    
    predictions[[studyid]] <- apply(final_store, 1, sum)
    #prediction[[studyid]] <- apply(prediction_store, 1, mean)
  }
  return(predictions)
}


wrapper_function <- function(Nstudies = NULL, Ncov = NULL, sys_missing_prob = NULL, magnitude = NULL, heterogeneity = NULL, interaction = NULL, aggregation_bias = NULL, Nsim = 100){

  naive_store <- matrix(NA, nrow = Nsim, ncol = 3)
  imputation_noclusterstore <- matrix(NA, nrow = Nsim, ncol = 3)
  imputation_store <- matrix(NA, nrow = Nsim, ncol = 3)
  separate_store <- matrix(NA, nrow = Nsim, ncol = 3)
  
  for(i in 1:Nsim){
    
    set.seed(i)
    simulated_data <- generate_sysmiss_ipdma_example(Nstudies = Nstudies, Ncov = Ncov, sys_missing_prob = sys_missing_prob, magnitude = magnitude, 
                                                     heterogeneity = heterogeneity, interaction = interaction, aggregation_bias = aggregation_bias)
    simulated_dataset <- simulated_data$dataset
    
    validation_data <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = Ncov, sys_missing_prob = sys_missing_prob, magnitude = magnitude, 
                                                      heterogeneity = heterogeneity, interaction = interaction, aggregation_bias = aggregation_bias)
    validation_dataset <- validation_data$dataset
    
    testdata <- findTestingOutcome(validation_dataset)
    
    # naive method
    naivepred <- NA
    naivepred <- try(naive_prediction(simulated_dataset, validation_dataset))
    naiveperf <- try(findPerformance(testdata, naivepred))
    naive_store[i,] <- naiveperf
    
    # imputation method - ignoring study level
    imputation_noclusterpred <- NA
    imputation_noclusterpred <- try(imputation_prediction(simulated_dataset, validation_dataset, method = "imputation_nocluster"))
    imputation_noclusterperf <- try(findPerformance(testdata, imputation_noclusterpred))
    imputation_noclusterstore[i,] <- imputation_noclusterperf
    
    # imputation method - accounting for study level
    imputationpred <- NA
    imputationpred <- try(imputation_prediction(simulated_dataset, validation_dataset))
    imputationperf <- try(findPerformance(testdata, imputationpred))
    imputation_store[i,] <- imputationperf
    
    # separate method
    separatepred <- NA
    separatepred <- try(separate_prediction(simulated_dataset, validation_dataset))
    separateperf <- try(findPerformance(testdata, separatepred))
    separate_store[i,] <- separateperf
  }
  
  naive_store_revised <- apply(naive_store, 2, as.numeric)
  imputation_noclusterstore_revised <- apply(imputation_noclusterstore, 2, as.numeric)
  imputation_store_revised <- apply(imputation_store, 2, as.numeric)
  separate_store_revised <- apply(separate_store, 2, as.numeric)
  
  naive_failed <- which(rowSums(is.na(naive_store_revised)) > 0)
  imputation_noclusterfailed <- which(rowSums(is.na(imputation_noclusterstore_revised)) > 0)
  imputation_failed <- which(rowSums(is.na(imputation_store_revised)) > 0)
  separate_failed <- which(rowSums(is.na(separate_store_revised)) > 0)
  
  number_failed_simulations <- c(length(naive_failed), length(imputation_noclusterfailed),
                                 length(imputation_failed), length(separate_failed))

  failed_set <- unique(c(naive_failed, imputation_noclusterfailed, imputation_failed, separate_failed))

  if(length(failed_set) != 0){
    naive_store_revised <- naive_store_revised[-failed_set,]
    imputation_noclusterstore_revised <- imputation_noclusterstore_revised[-failed_set,]
    imputation_store_revisedd <- imputation_store_revised[-failed_set,]
    separate_store_revised <- separate_store_revised[-failed_set,]
  }
  
  return_matrix <- matrix(NA, 4, 3)
  return_matrix[1,] <- round(apply(naive_store_revised, 2, mean, na.rm = TRUE), digits = 5)
  return_matrix[2,] <- round(apply(imputation_noclusterstore_revised, 2, mean, na.rm = TRUE), digits = 5)
  return_matrix[3,] <- round(apply(imputation_store_revised, 2, mean, na.rm = TRUE), digits = 5)
  return_matrix[4,] <- round(apply(separate_store_revised, 2, mean, na.rm = TRUE), digits = 5)
  
  #only report MSE and R-squared
  return_matrix <- return_matrix[,c(1,3)]

  rownames(return_matrix) <- c("Naive", "Imputation ignoring heterogeneity", "Imputation accounting heterogeneity", "Separate prediction")
  colnames(return_matrix) <- c("MSE", "R-squared")
  
  return(list(naive_store = naive_store, imputation_noclusterstore = imputation_noclusterstore,
              imputation_store = imputation_store, separate_store = separate_store,
              number_failed_simulations = number_failed_simulations, return_matrix = return_matrix))
}








