
crossvalidation_realdata <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  testingoutcome <- list()
  
  covariates_naive <- c("baseline", "gender")
  typeofvar_naive <- c("continuous", "binary")
  
  covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
  typeofvar_all <- c("continuous", "binary", "continuous", "binary", "binary", "binary", "binary", "binary")
  
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
    
    if(method == "naive"){
      
      naiveapproach <- ipdma.impute(training_set, covariates = covariates_naive, typeofvar = typeofvar_naive, interaction = TRUE,
                                    studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
      imp.list <- naiveapproach$imp.list

      testing_set <- testing_set %>% select(study, y, treat, all_of(covariates_naive)) %>% filter(complete.cases(.))
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.model <- lmer(y ~ (baseline + gender) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
        bb <- model.matrix(y ~ (baseline + gender) * treat, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      
      testingoutcome[[studyid]] <- testing_set$y
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      
    } else if(method == "imputation"){
      
      imputationapproach <- ipdma.impute(mydata, covariates = covariates_all, typeofvar = typeofvar_all, interaction = TRUE,
                                         studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
      imp.list <- imputationapproach$imp.list
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                               studyname = "study", treatmentname = "treat", outcomename = "y")
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
        imp.model <- lmer(form, data = imp.dummy)
        
        form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
        bb <- model.matrix(form2, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      
      testingoutcome[[studyid]] <- testing_set$y
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      
    } else if(method == "separate"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                               studyname = "study", treatmentname = "treat", outcomename = "y")
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      precision_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        
        training_set_dummy <- training_set %>% filter(study == studyname2[i])
        
        # Need to remove systematically missing covariates for each study
        missingPattern <- findMissingPattern(training_set_dummy, covariates_all, typeofvar_all, 
                                                 studyname = "study", treatmentname = "treat", outcomename = "y")
        training_set_dummy <- training_set_dummy %>% select(-all_of(missingPattern$sys_covariates))
        imputationapproach <- ipdma.impute(training_set_dummy, covariates = missingPattern$without_sys_covariates, typeofvar = typeofvar[missingPattern$without_sys_covariates], interaction = TRUE,
                                           studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
        imp.list <- imputationapproach$imp.list
        
        prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
        variance.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
        
        #use covariates that are not systematically missing in both training and testing dataset
        without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.dummy <- imp.dummy %>% select(-".imp", -".id", -"study")
          form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
          imp.model <- lm(form, data = imp.dummy)
          
          bb <- model.matrix(form, data = testing_set)
          prediction.dummy[,ii] <- bb %*% coef(imp.model)
          variance.dummy[,ii] <- diag(bb %*% vcov(imp.model) %*% t(bb))
        }
        prediction_store[,i] <- apply(prediction.dummy, 1, mean)
        precision_store[,i] <- 1/findVarianceUsingRubinsRule(prediction.dummy, variance.dummy)
        
      }
      product_store <- prediction_store * precision_store
      precision_vec <- apply(precision_store, 1, sum)
      
      final_store <- sweep(product_store, 1, precision_vec, `/`)
      
      predictions[[studyid]] <- apply(final_store, 1, sum)
      #predictions[[studyid]] <- apply(prediction_store, 1, mean)
      
      testingoutcome[[studyid]] <- testing_set$y
    }
  }
  return(list(predictions = predictions, testingoutcome = testingoutcome))
}


