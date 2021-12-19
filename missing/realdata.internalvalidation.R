#devtools::install_github("MikeJSeo/bipd") # personal github with some imputation related functions
library(bipd)
library(dplyr)

#load imputation methods
library(mice) #pmm
library(miceadds)#2l.pmm
library(micemd) #2l.2stage.norm

library(lme4) #running lmer
library(broom.mixed) #for summarizing lmer results from multiply imputed dataset
library(mitools) #for function imputationList

setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")

setwd("C:/Users/mike/Desktop/Github/phd/missing")
source("helpful.functions.R")

############################################################################
################ Data cleaning

# pick which treatment to compare
mydata <- data %>% filter(treat.m2 %in% c("TAU", "Guided", "Unguided"))

# merge Guided and Unguided as treatment
mydata <- mydata %>% mutate(treat.m2 = ifelse(treat.m2 %in% c("Guided", "Unguided"), 1, 0)) %>%
  rename(treat = treat.m2)

# Delete 2 studies that have same gender(female) for all participants
mydata <- mydata %>% filter(!study %in% c("Forsell, 2017", "Milgrom, 2016")) %>%
  mutate(gender = na_if(gender, "")) %>%
  mutate(gender = ifelse(gender == "Female", 1, 0))

# Delete a study where ComorbidAnxiety is only observed for treated patients
mydata <- mydata %>% filter(!study %in% c("Christensen, 2004"))

# Delete a study where Alcoholic patient is only observed for very few patients
# mydata <- mydata %>% filter(!study %in% c("De Graaf, 2009", "Kivi, 2014"))

# leave studies two treatment (delete the ones that have only 1)
mydata2 <- mydata %>% group_by(study) %>% filter(length(unique(treat)) == 2)
mydata <- mydata %>% filter(study %in% unique(mydata2$study))
mydata$study <- as.numeric(as.factor(mydata$study))

# visualizing variables
# library(summarytools)
# view(dfSummary(mydata), method = "browser")

# Specify the covariates of interest and type of variable
covariates <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
typeofvar <- c("continuous", "binary", "continuous", "binary", "binary", "binary", "binary", "binary")
mydata <- mydata %>% select(all_of(c("study", "treat", "y", covariates)))

covariates_naive <- c("baseline", "gender")
typeofvar_naive <- c("continuous", "binary")

covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
typeofvar_all <- c("continuous", "binary", "continuous", "binary", "binary", "binary", "binary", "binary")


########################################
############ Internal validation
nstudy <- length(unique(mydata$study))
predictions <- list()
testingoutcome <- list()

# Naive approach
set.seed(1)
naiveapproach <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                              studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
imp.list <- naiveapproach$imp.list

for(studyid in 1:nstudy){
  testing_set <- mydata[mydata$study == studyid,]

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
}
apparent_naiveperf <- findPerformance(testingoutcome, predictions, aggregation = "ignore")

  
# Imputation approach ignoring clustering
set.seed(1)
imputationapproach <- ipdma.impute(mydata, covariates = covariates_all, typeofvar = typeofvar_all, sys_impute_method = "pmm",
                                   interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)  
imp.list <- imputationapproach$imp.list

for(studyid in 1:nstudy){
  testing_set <- mydata[mydata$study == studyid,]
  
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
}
apparent_imputation_noclusterperf <- findPerformance(testingoutcome, predictions, aggregation = "ignore")


#imputation approach accounting for clustering
set.seed(1)
imputationapproach <- ipdma.impute(mydata, covariates = covariates_all, typeofvar = typeofvar_all, interaction = TRUE,
                                   studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
imp.list <- imputationapproach$imp.list

for(studyid in 1:nstudy){
  testing_set <- mydata[mydata$study == studyid,]
  
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
}
apparent_imputationperf <- findPerformance(testingoutcome, predictions, aggregation = "ignore")

#separate prediction
set.seed(1)

for(studyid in 1:nstudy){
  testing_set <- mydata[mydata$study == studyid,]

  studyname2 <- unique(mydata$study)
  nstudy2 <- length(studyname2)

  missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                         studyname = "study", treatmentname = "treat", outcomename = "y")
  testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))

  prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2) # to store prediction of the fit estimates (X*beta)
  precision_store <- matrix(NA, dim(testing_set)[1], nstudy2) # to store precision of the prediction (standard error of fit + residual standard deviation) 

  for(i in 1:nstudy2){
  
    training_set_dummy <- mydata %>% filter(study == studyname2[i])
  
    # Need to remove systematically missing covariates for each study
    missingPattern <- findMissingPattern(training_set_dummy, covariates_all, typeofvar_all, 
                                       studyname = "study", treatmentname = "treat", outcomename = "y")
    training_set_dummy <- training_set_dummy %>% select(-all_of(missingPattern$sys_covariates))
    imputationapproach <- ipdma.impute(training_set_dummy, covariates = missingPattern$without_sys_covariates, typeofvar = typeofvar[which(covariates_all %in% missingPattern$without_sys_covariates)], interaction = TRUE,
                                     studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
    imp.list <- imputationapproach$imp.list
  
    prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
    variance.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
  
    # Use covariates that are not systematically missing in both training and testing dataset
    without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
  
    for(ii in 1:length(imp.list)){
      imp.dummy <- imp.list[[ii]]
      imp.dummy <- imp.dummy %>% select(-".imp", -".id", -"study")
      form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
      imp.model <- lm(form, data = imp.dummy)
    
      bb <- model.matrix(form, data = testing_set)
      prediction.dummy[,ii] <- bb %*% coef(imp.model)
      variance.dummy[,ii] <- diag(bb %*% vcov(imp.model) %*% t(bb)) + sigma(imp.model)^2
    }
    prediction_store[,i] <- apply(prediction.dummy, 1, mean)
    precision_store[,i] <- 1/(findVarianceUsingRubinsRule(prediction.dummy, variance.dummy))
  }
  product_store <- prediction_store * precision_store
  precision_vec <- apply(precision_store, 1, sum)

  final_store <- sweep(product_store, 1, precision_vec, `/`)

  predictions[[studyid]] <- apply(final_store, 1, sum)
  testingoutcome[[studyid]] <- testing_set$y
}
apparent_separateperf <- findPerformance(testingoutcome, predictions, aggregation = "ignore")


rbind(apparent_naiveperf, apparent_imputation_noclusterperf, apparent_imputationperf, apparent_separateperf)


############## Optimism-corrected performance

#Naive approach
set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 3)
colnames(optimism) <- c("mse", "mae", "rsquared")

for(jjj in 1:200){
  
  nstudy <- length(unique(mydata$study))
  predictions <- list()
  predictions_bootstrap <- list()
  testingoutcome <- list()
  testingoutcome_bootstrap <- list()

  mydata_bootstrap <- mydata[findBootstrapSample(mydata),]
  rownames(mydata_bootstrap) <- 1:dim(mydata_bootstrap)[1]
  naiveapproach <- ipdma.impute(mydata_bootstrap, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                                studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)
  imp.list <- naiveapproach$imp.list
  
  for(studyid in 1:nstudy){
    
    testing_set <- mydata[mydata$study == studyid,]
    testing_set <- testing_set %>% select(study, y, treat, all_of(covariates_naive)) %>% filter(complete.cases(.))
    
    testing_set_bootstrap <- mydata_bootstrap[mydata_bootstrap$study == studyid,]
    testing_set_bootstrap <- testing_set_bootstrap %>% select(study, y, treat, all_of(covariates_naive)) %>% filter(complete.cases(.))

    prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
    prediction.dummy.bootstrap <- matrix(NA, nrow = dim(testing_set_bootstrap)[1], ncol = length(imp.list))
    
    for(ii in 1:length(imp.list)){
      imp.dummy <- imp.list[[ii]]
      imp.model <- lmer(y ~ (baseline + gender) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
      bb <- model.matrix(y ~ (baseline + gender) * treat, data = testing_set)
      prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      
      bb2 <- model.matrix(y ~ (baseline + gender) * treat, data = testing_set_bootstrap)
      prediction.dummy.bootstrap[,ii] <- bb2 %*% fixef(imp.model)
    }
    
    testingoutcome[[studyid]] <- testing_set$y
    predictions[[studyid]] <- apply(prediction.dummy, 1, mean)

    testingoutcome_bootstrap[[studyid]] <- testing_set_bootstrap$y
    predictions_bootstrap[[studyid]] <- apply(prediction.dummy.bootstrap, 1, mean)
  }
  naiveperf_bootstrap <- findPerformance(testingoutcome_bootstrap, predictions_bootstrap, aggregation = "ignore")
  naiveperf_test <- findPerformance(testingoutcome, predictions, aggregation = "ignore")
  
  optimism[jjj,] <- naiveperf_bootstrap - naiveperf_test
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_naiveperf <- apparent_naiveperf - optimism_averaged


# Imputation approach ignoring clustering
set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 3)
colnames(optimism) <- c("mse", "mae", "rsquared")

for(jjj in 1:200){

  mydata_bootstrap <- mydata[findBootstrapSample(mydata),]
  rownames(mydata_bootstrap) <- 1:dim(mydata_bootstrap)[1]
  imputationapproach <- ipdma.impute(mydata_bootstrap, covariates = covariates_all, typeofvar = typeofvar_all, sys_impute_method = "pmm",
                                     interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)  
  imp.list <- imputationapproach$imp.list
  
  for(studyid in 1:nstudy){
    
    testing_set <- mydata[mydata$study == studyid,]
    testing_set_bootstrap <- mydata_bootstrap[mydata_bootstrap$study == studyid,]
    
    missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                             studyname = "study", treatmentname = "treat", outcomename = "y")
    
    testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    testing_set_bootstrap <- testing_set_bootstrap %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    
    prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
    prediction.dummy.bootstrap <- matrix(NA, nrow = dim(testing_set_bootstrap)[1], ncol = length(imp.list))
    
    for(ii in 1:length(imp.list)){
      imp.dummy <- imp.list[[ii]]
      form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
      imp.model <- lmer(form, data = imp.dummy)
    
      form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
      bb <- model.matrix(form2, data = testing_set)
      prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      
      bb2 <- model.matrix(form2, data = testing_set_bootstrap)
      prediction.dummy.bootstrap[,ii] <- bb2 %*% fixef(imp.model)
    }
  
    testingoutcome[[studyid]] <- testing_set$y
    predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
    
    testingoutcome_bootstrap[[studyid]] <- testing_set_bootstrap$y
    predictions_bootstrap[[studyid]] <- apply(prediction.dummy.bootstrap, 1, mean)
  }
  
  imputation_noclusterperf_bootstrap <- findPerformance(testingoutcome_bootstrap, predictions_bootstrap, aggregation = "ignore")
  imputation_noclusterperf_test <- findPerformance(testingoutcome, predictions, aggregation = "ignore")
  
  optimism[jjj,] <- imputation_noclusterperf_bootstrap - imputation_noclusterperf_test
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_imputation_noclusterperf <- apparent_imputation_noclusterperf - optimism_averaged



# Imputation approach accounting for clustering
set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 3)
colnames(optimism) <- c("mse", "mae", "rsquared")

for(jjj in 1:200){
  
  nstudy <- length(unique(mydata$study))
  predictions <- list()
  predictions_bootstrap <- list()
  testingoutcome <- list()
  testingoutcome_bootstrap <- list()
  
  mydata_bootstrap <- mydata[findBootstrapSample(mydata),]
  rownames(mydata_bootstrap) <- 1:dim(mydata_bootstrap)[1]
  imputationapproach <- ipdma.impute(mydata_bootstrap, covariates = covariates_all, typeofvar = typeofvar_all,
                                     interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)  
  imp.list <- imputationapproach$imp.list
  
  for(studyid in 1:nstudy){
    
    testing_set <- mydata[mydata$study == studyid,]
    testing_set_bootstrap <- mydata_bootstrap[mydata_bootstrap$study == studyid,]
    
    missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                             studyname = "study", treatmentname = "treat", outcomename = "y")
    
    testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    testing_set_bootstrap <- testing_set_bootstrap %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
     
    prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
    prediction.dummy.bootstrap <- matrix(NA, nrow = dim(testing_set_bootstrap)[1], ncol = length(imp.list))
    
    for(ii in 1:length(imp.list)){
      imp.dummy <- imp.list[[ii]]
      form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
      imp.model <- lmer(form, data = imp.dummy)
      
      form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
      bb <- model.matrix(form2, data = testing_set)
      prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      
      bb2 <- model.matrix(form2, data = testing_set_bootstrap)
      prediction.dummy.bootstrap[,ii] <- bb2 %*% fixef(imp.model)
    }
    
    testingoutcome[[studyid]] <- testing_set$y
    predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
    
    testingoutcome_bootstrap[[studyid]] <- testing_set_bootstrap$y
    predictions_bootstrap[[studyid]] <- apply(prediction.dummy.bootstrap, 1, mean)
  }
  
  imputationperf_bootstrap <- findPerformance(testingoutcome_bootstrap, predictions_bootstrap, aggregation = "ignore")
  imputationperf_test <- findPerformance(testingoutcome, predictions, aggregation = "ignore")
  
  optimism[jjj,] <- imputationperf_bootstrap - imputationperf_test
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_imputationperf <- apparent_imputationperf - optimism_averaged


#separate prediction
set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 3)
colnames(optimism) <- c("mse", "mae", "rsquared")

for(jjj in 1:200){
  
  mydata_bootstrap <- mydata[findBootstrapSample(mydata),]

  for(studyid in 1:nstudy){
    testing_set <- mydata[mydata$study == studyid,]
    testing_set_bootstrap <- mydata_bootstrap[mydata_bootstrap$study == studyid,]
  
    studyname2 <- unique(mydata$study)
    nstudy2 <- length(studyname2)
  
    missingPatternTest <- findMissingPattern(testing_set, covariates_all, typeofvar_all, 
                                           studyname = "study", treatmentname = "treat", outcomename = "y")
    testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    testing_set_bootstrap <- testing_set_bootstrap %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
    
    prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2) # to store prediction of the fit estimates (X*beta)
    precision_store <- matrix(NA, dim(testing_set)[1], nstudy2) # to store precision of the prediction (standard error of fit + residual standard deviation) 
  
    prediction_store_bootstrap <- matrix(NA, dim(testing_set_bootstrap)[1], nstudy2)
    precision_store_bootstrap <-  matrix(NA, dim(testing_set_bootstrap)[1], nstudy2)
    
    for(i in 1:nstudy2){
    
      training_set_dummy <- mydata_bootstrap %>% filter(study == studyname2[i])

      # Need to remove systematically missing covariates for each study
      missingPattern <- findMissingPattern(training_set_dummy, covariates_all, typeofvar_all, 
                                         studyname = "study", treatmentname = "treat", outcomename = "y")
      training_set_dummy <- training_set_dummy %>% select(-all_of(missingPattern$sys_covariates))

      imputationapproach <- ipdma.impute(training_set_dummy, covariates = missingPattern$without_sys_covariates, typeofvar = typeofvar[which(covariates_all %in% missingPattern$without_sys_covariates)], interaction = TRUE,
                                       studyname = "study", treatmentname = "treat", outcomename = "y", m = 10)
      imp.list <- imputationapproach$imp.list
    
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      variance.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
    
      prediction.dummy.bootstrap <- matrix(NA, nrow = dim(testing_set_bootstrap)[1], ncol = length(imp.list))
      variance.dummy.bootstrap <- matrix(NA, nrow = dim(testing_set_bootstrap)[1], ncol = length(imp.list))
    
      # Use covariates that are not systematically missing in both training and testing dataset
      without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
    
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.dummy <- imp.dummy %>% select(-".imp", -".id", -"study")
        form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
        imp.model <- lm(form, data = imp.dummy)
      
        bb <- model.matrix(form, data = testing_set)
        prediction.dummy[,ii] <- bb %*% coef(imp.model)
        variance.dummy[,ii] <- diag(bb %*% vcov(imp.model) %*% t(bb)) + sigma(imp.model)^2
      
        bb2 <- model.matrix(form, data = testing_set_bootstrap)
        prediction.dummy.bootstrap[,ii] <- bb2 %*% coef(imp.model)
        variance.dummy.bootstrap[,ii] <- diag(bb2 %*% vcov(imp.model) %*% t(bb2)) + sigma(imp.model)^2
      }
      prediction_store[,i] <- apply(prediction.dummy, 1, mean)
      prediction_store_bootstrap[,i] <- apply(prediction.dummy.bootstrap, 1, mean)
    
      precision_store[,i] <- 1/(findVarianceUsingRubinsRule(prediction.dummy, variance.dummy))
      precision_store_bootstrap[,i] <- 1/(findVarianceUsingRubinsRule(prediction.dummy.bootstrap, variance.dummy.bootstrap))
    }
    
    product_store <- prediction_store * precision_store
    precision_vec <- apply(precision_store, 1, sum)
    
    product_store_bootstrap <- prediction_store_bootstrap * precision_store_bootstrap
    precision_vec_bootstrap <- apply(precision_store_bootstrap, 1, sum)
    
    final_store <- sweep(product_store, 1, precision_vec, `/`)
    final_store_bootstrap <- sweep(product_store_bootstrap, 1, precision_vec_bootstrap, `/`)
    
    predictions[[studyid]] <- apply(final_store, 1, sum)
    testingoutcome[[studyid]] <- testing_set$y
    
    predictions_bootstrap[[studyid]] <- apply(final_store_bootstrap, 1, sum)
    testingoutcome_bootstrap[[studyid]] <- testing_set_bootstrap$y
  }
  separateperf_bootstrap <- findPerformance(testingoutcome_bootstrap, predictions_bootstrap, aggregation = "ignore")
  separateperf_test <- findPerformance(testingoutcome, predictions, aggregation = "ignore")
  
  optimism[jjj,] <- separateperf_bootstrap - separateperf_test
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_separateperf <- apparent_separateperf - optimism_averaged


rbind(optimism_corrected_naiveperf, optimism_corrected_imputation_noclusterperf, optimism_corrected_imputationperf, optimism_corrected_separateperf)
