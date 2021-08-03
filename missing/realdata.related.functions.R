findTestData <- function(crossdata){
  
  nstudy <- length(unique(crossdata$study))
  
  testingdata <- list()
  
  for(studyid in 1:nstudy){
    
    testing_set <- crossdata[crossdata$study == studyid,]
    
    covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
    missingPatternTest <- findMissingPattern(testing_set, covariates_all)  
    
    testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    testingdata[[studyid]] <- testing_set$y
  }
  return(unlist(testingdata))
}

  

crossvalidation_realdata <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()

  covariates_naive <- c("baseline", "gender")
  covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
  
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
    
    if(method == "naive"){
      
      training_set <- training_set %>% select(study, y, treat, all_of(covariates_naive))
      training_set <- createinteractions(training_set, covariates_naive)
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all) 
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      missingPattern <- findMissingPattern(training_set, covariates_naive)  

      meth <- getCorrectMeth(training_set, missingPattern, method)
      pred <- getCorrectPred(training_set, missingPattern, method)

      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.model <- lmer(y ~ (baseline + gender) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
        bb <- model.matrix(y ~ (baseline + gender) * treat, data = testing_set)
        
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)

    } else if(method == "imputation"){
      
      training_set <- training_set %>% select(study, y, treat, all_of(covariates_all))
      training_set <- createinteractions(training_set, covariates_all)
    
      missingPatternTest <- findMissingPattern(testing_set, covariates_all)  
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      missingPattern <- findMissingPattern(training_set, covariates_all)  
      
      meth <- getCorrectMeth(training_set, missingPattern, method, type_of_var = type_of_var)
      pred <- getCorrectPred(training_set, missingPattern, method)

      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
        imp.model <- lmer(form, data = imp.dummy)
        
        form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
        bb <- model.matrix(form2, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)

    } else if(method == "separate"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all)  
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      precision_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        
        training_set_dummy <- training_set %>% filter(study == studyname2[i])
        missingPattern <- findMissingPattern(training_set_dummy, covariates_all)  
        
        training_set_dummy <- training_set_dummy %>% select(study, y, treat, all_of(missingPattern$without_sys_covariates))
        training_set_dummy <- createinteractions(training_set_dummy, missingPattern$without_sys_covariates)
        
        meth <- getCorrectMeth(training_set_dummy, missingPattern, "average_prediction")
        pred <- getCorrectPred(training_set_dummy, missingPattern, "average_prediction")
        
        imp <- mice(training_set_dummy, pred = pred, meth = meth)
        impc <- complete(imp, "long", include = "TRUE")
        imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
        
        prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
        variance.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
        
        #use covariates that are not systematically missing in both training and testing dataset
        without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
          imp.model <- lm(form, data = imp.dummy)
          
          bb <- model.matrix(form, data = testing_set)
          prediction.dummy[,ii] <- bb %*% coef(imp.model)
          variance.dummy[,ii] <- diag(bb %*% vcov(imp.model) %*% t(bb))
        }
        prediction_store[,i] <- apply(prediction.dummy, 1, mean)
        precision_store[,i] <- 1/findVarianceUsingRubinsRule(prediction.dummy, variance.dummy)
        
        print(paste0("2nd one; ", studyname2[i]))
      }
      product_store <- prediction_store * precision_store
      precision_vec <- apply(precision_store, 1, sum)
      
      final_store <- sweep(product_store, 1, precision_vec, `/`)
      
      predictions[[studyid]] <- apply(final_store, 1, sum)
      #predictions[[studyid]] <- apply(prediction_store, 1, mean)
    }
  }
  
  return(unlist(predictions))
  
}


findVarianceUsingRubinsRule <- function(prediction.dummy, variance.dummy){
  
  avg.prediction <- apply(prediction.dummy, 1, mean)
  summ <- 0
  for(iii in 1:5){
    summ <- summ + (prediction.dummy[,iii] - avg.prediction)^2
  }
  return(apply(variance.dummy, 1, mean) + (5 + 1)/ (5^2 - 5) * summ)
}

