library(jomo) #for imputation
library(mitools) #convenient function to combine imputations
library(dplyr)
library(tidyr)
library(lme4) #linear mixed effects model
library(glmnet)
library(rjags) #for running JAGS

library(caret) #cross validation
library(ranger) #random forest
library(gbm) #gradient boosting model
library(e1071) #svm
library(pROC) #finding ROC
library(ggplot2)
library(coda)

# Load in data
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Study3_prediction_model")
data <- readRDS('dt_pm.rds')
data <- as_tibble(data)
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Seo") #change back the directory
source("helpful.functions.R")


##### Primary outcome: ADAS_TRANSFORMED_OBS
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "ADAS_TRANSFORMED_OBS", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE) %>% filter(treat == 0)

#find which model fitted best
data_full2 <- data_full %>% drop_na(y)

############crossvalidation
testingoutcome <- findTestingOutcome(data_full2)

prediction.lm <- crossvalidate(data_full2, "lm")
performance.lm <- findPerformance(testingoutcome, prediction.lm, aggregation = "weighted")

prediction.lmer <- crossvalidate(data_full2, "lmer")
performance.lmer <- findPerformance(testingoutcome, prediction.lmer, aggregation = "weighted")

prediction.ridge <- crossvalidate(data_full2, "ridge")
performance.ridge <- findPerformance(testingoutcome, prediction.ridge, aggregation = "weighted")

prediction.bayes <- crossvalidate(data_full2, "bayes")
performance.bayes <- findPerformance(testingoutcome, prediction.bayes, aggregation = "weighted")

performance.randomforest <- crossvalidate_usingcaret(data_full2, "randomforest")
performance.gbm <- crossvalidate_usingcaret(data_full2, "gbm")
performance.svm <- crossvalidate_usingcaret(data_full2, "svm")

rbind(performance.lm, performance.lmer, performance.ridge, performance.bayes,
      performance.randomforest, performance.gbm, performance.svm)


#### Fitting the prediction model using Bayesian linear mixed effects model
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Seo/")
load("prediction-ADAS-imputations.RData")
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[1]]) 

data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(imputed_matrix)),
    y = imp.list$imputation[[1]]$y,
    X = imputed_matrix,
    Np = length(data_full$studyid),
    studyid = data_full$studyid)

mod <- jags.model(file = "prediction model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
stats::update(mod, 1000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta", "sigma"), n.iter = 10000)

for(mm in 2:5){
  
  imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[mm]]) 
  
  data.JAGS <- 
    list(
      Nstudies = length(study_inclusion),
      Ncovariate = length(colnames(imputed_matrix)),
      y = imp.list$imputation[[1]]$y,
      X = imputed_matrix,
      Np = length(data_full$studyid),
      studyid = data_full$studyid)
  
  mod <- jags.model(file = "prediction model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 1000)
  sample_more <- coda.samples(mod, variable.names = c("alpha", "beta", "sigma"), n.iter = 10000)
  samples <- add.mcmc(samples, sample_more)
}
summary(samples)
#save(samples, file = "prediction-ADAS-result.RData")

##### Secondary outcome: CIBIC_PLUS_TRANSFORMED
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_PLUS_TRANSFORMED", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE) %>% filter(treat == 0)
data_full2 <- data_full %>% drop_na(y)
testingoutcome <- findTestingOutcome(data_full2)

### cross validation
prediction.lm <- crossvalidate(data_full2, "lm")
performance.lm <- findPerformance(testingoutcome, prediction.lm, aggregation = "weighted")

prediction.lmer <- crossvalidate(data_full2, "lmer")
performance.lmer <- findPerformance(testingoutcome, prediction.lmer, aggregation = "weighted")

prediction.ridge <- crossvalidate(data_full2, "ridge")
performance.ridge <- findPerformance(testingoutcome, prediction.ridge, aggregation = "weighted")

prediction.bayes <- crossvalidate(data_full2, "bayes")
performance.bayes <- findPerformance(testingoutcome, prediction.bayes, aggregation = "weighted")

performance.randomforest <- crossvalidate_usingcaret(data_full2, "randomforest")
performance.gbm <- crossvalidate_usingcaret(data_full2, "gbm")
performance.svm <- crossvalidate_usingcaret(data_full2, "svm")

rbind(performance.lm, performance.lmer, performance.ridge, performance.bayes,
      performance.randomforest, performance.gbm, performance.svm)

#### Fitting the prediction model using Bayesian linear mixed effects model
load("prediction-CIBIC-imputations.RData")
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[1]]) 

data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(imputed_matrix)),
    y = imp.list$imputation[[1]]$y,
    X = imputed_matrix,
    Np = length(clus),
    studyid = clus)

mod <- jags.model(file = "prediction model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
stats::update(mod, 1000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta", "sigma"), n.iter = 10000)

for(mm in 2:5){
  
  imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[mm]]) 
  
  data.JAGS <- 
    list(
      Nstudies = length(study_inclusion),
      Ncovariate = length(colnames(imputed_matrix)),
      y = imp.list$imputation[[1]]$y,
      X = imputed_matrix,
      Np = length(clus),
      studyid = clus)
  
  mod <- jags.model(file = "prediction model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 1000)
  sample_more <- coda.samples(mod, variable.names = c("alpha", "beta", "sigma"), n.iter = 10000)
  samples <- add.mcmc(samples, sample_more)
}
summary(samples)
#save(samples, file = "prediction-CIBIC-result.RData")

#######Analyzing Dropout
study_inclusion <- c(161, 231, 302, 304, 311, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "Dropout", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:y) %>% filter(treat == 0)
data_full2 <- data_full %>% drop_na(y)
testingoutcome <- findTestingOutcome(data_full2)

### cross validation
prediction.glm <- crossvalidate(data_full2, "glm")
performance.glm <- findPerformance(testingoutcome, prediction.glm, aggregation = "weighted", outcome = "binary")

prediction.glmer <- crossvalidate(data_full2, "glmer")
performance.glmer <- findPerformance(testingoutcome, prediction.glmer, aggregation = "weighted", outcome = "binary")

prediction.ridge_binary <- crossvalidate(data_full2, "ridge_binary")
performance.ridge_binary <- findPerformance(testingoutcome, prediction.ridge_binary, aggregation = "weighted", outcome = "binary")

prediction.bayes_binary <- crossvalidate(data_full2, "bayes_binary")
performance.bayes_binary <- findPerformance(testingoutcome, prediction.bayes_binary, aggregation = "weighted", outcome = "binary")

performance.randomforest_binary <- crossvalidate_usingcaret(data_full2, "randomforest", outcome = "binary")
performance.gbm_binary <- crossvalidate_usingcaret(data_full2, "gbm", outcome = "binary")
performance.svm_binary <- crossvalidate_usingcaret(data_full2, "svm", outcome = "binary")

rbind(performance.glm, performance.glmer, performance.ridge_binary, performance.bayes_binary,
      performance.randomforest_binary, performance.gbm_binary, performance.svm_binary)

#### Fitting the prediction model using Bayesian linear mixed effects model
bb <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full) 
data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(bb)),
    y = data_full$y,
    X = bb,
    Np = length(data_full$y),
    studyid = data_full$studyid)

mod <- jags.model(file = "prediction model binary.txt", data = data.JAGS, n.chains = 3, n.adapt = 5000)
stats::update(mod, 5000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta"), n.iter = 50000)
summary(samples)
#save(samples, file = "prediction-Dropout-result.RData")

