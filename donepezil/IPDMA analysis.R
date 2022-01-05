### This code fits IPDMA model
library(mitools) #convenient function to combine imputations
library(dplyr)
library(tidyr)
library(rjags) #for running JAGS
library(ggplot2)
library(coda)

# First load the data. Do this before doing any analysis.
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Study3_prediction_model")
data <- readRDS('dt_pm.rds')
data <- as_tibble(data)
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Seo") #change back the directory
source("helpful.functions.R")

# Do preprocessing of the data
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "ADAS_TRANSFORMED_OBS", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE)

##### Primary outcome: ADAS_TRANSFORMED_OBS
set.seed(1)
load("IPDMA-ADAS-imputations.RData")
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[1]]) 

data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(imputed_matrix)),
    y = imp.list$imputation[[1]]$y,
    X = imputed_matrix,
    Np = length(data_full$studyid),
    studyid = clus <- data_full$studyid,
    treat = data_full$treat + 1)

mod <- jags.model(file = "random effects IPDMA model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
stats::update(mod, 1000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta", "d", "sd.delta", "sigma"), n.iter = 10000)

for(mm in 2:5){
  
  imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[mm]]) 
  
  data.JAGS <- 
    list(
      Nstudies = length(study_inclusion),
      Ncovariate = length(colnames(imputed_matrix)),
      y = imp.list$imputation[[1]]$y,
      X = imputed_matrix,
      Np = length(data_full$studyid),
      studyid = data_full$studyid,
      treat = data_full$treat + 1)
  
  mod <- jags.model(file = "random effects IPDMA model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 1000)
  sample_more <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta", "d", "sd.delta", "sigma"), n.iter = 10000)
  samples <- add.mcmc(samples, sample_more)
}
summary(samples)
#save(samples, file = "IPDMA-ADAS-result.RData")


###### Secondary outcome: CIBIC_PLUS_TRANSFORMED
set.seed(1)
load("IPDMA-CIBIC-imputations.RData")
imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[1]]) 

data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(imputed_matrix)),
    y = imp.list$imputation[[1]]$y,
    X = imputed_matrix,
    Np = length(clus),
    studyid = clus,
    treat = data_full$treat + 1)

mod <- jags.model(file = "random effects IPDMA model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
stats::update(mod, 1000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta", "d", "sd.delta", "sigma"), n.iter = 10000)

for(mm in 2:5){
  
  imputed_matrix <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = imp.list$imputations[[mm]]) 
  
  data.JAGS <- 
    list(
      Nstudies = length(study_inclusion),
      Ncovariate = length(colnames(imputed_matrix)),
      y = imp.list$imputation[[1]]$y,
      X = imputed_matrix,
      Np = length(clus),
      studyid = clus,
      treat = data_full$treat + 1)
  
  mod <- jags.model(file = "random effects IPDMA model.txt", data = data.JAGS, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 1000)
  sample_more <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta", "d", "sd.delta", "sigma"), n.iter = 10000)
  samples <- add.mcmc(samples, sample_more)
}
summary(samples)
#save(samples, file = "IPDMA-CIBIC-result.RData")

####### Analyzing Dropout
study_inclusion <- c(161, 231, 302, 304, 311, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "Dropout", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:y)

bb <- model.matrix(y ~ -1 + AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full) 
data.JAGS <- 
  list(
    Nstudies = length(study_inclusion),
    Ncovariate = length(colnames(bb)),
    y = data_full$y,
    X = bb,
    Np = length(data_full$y),
    studyid = data_full$studyid,
    treat = data_full$treat + 1
    )

mod <- jags.model(file = "random effects IPDMA model binary.txt", data = data.JAGS, n.chains = 3, n.adapt = 5000)
stats::update(mod, 5000)
samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta", "d", "sd.delta"), n.iter = 50000)
summary(samples)
#save(samples, file = "IPDMA-Dropout-result.RData")
