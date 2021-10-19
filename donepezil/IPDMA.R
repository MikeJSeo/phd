library(jomo) #for imputation
library(mitools) #convenient function to combine imputations
library(dplyr)
library(tidyr)
library(lme4) #linear mixed effects model
library(rjags) #for running JAGS
library(pROC) #finding ROC
library(ggplot2)
library(coda)

# Load in data
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Study3_prediction_model")
data <- readRDS('dt_pm.rds')
data <- as_tibble(data)
setwd("E:/EST-RA-9140/EQUIPERCENTILE1/Seo") #change back the directory
source("helpful.functions.R")

#####################################ADAS_TRANSFORMED_OBS
#####predicting placebo group
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
crossvalidation <- crossvalidate(data_full2, "bayes")
apply(crossvalidation, 1, mean)

###imputation using jomo
bb <- data_full %>% select(all_of(covariates), y, y4, y6, y8, y12, y16, y18, y20)
bb <- bb %>% mutate(Intercept = 1)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates)])
clus <- data_full$studyid

###Need to compute imputation
#imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "prediction-ADAS-imputations.RData")

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
#save(samples, file = "prediction-ADAS-result.RData")

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
load("prediction-ADAS-result.RData")
arbitrary_patient <- c(70, 1, 60, 0, 1, 30, 8)
arbitrary_patient_standardized <- (arbitrary_patient - data_cleaned$mean_val) / data_cleaned$sd_val

matrix_coefs<- as.matrix(samples[, c("alpha", paste0("beta[", 1:7,"]"))])
matrix_mean <- matrix_coefs %*% c(1,arbitrary_patient_standardized)
matrix_sigma <- as.matrix(samples[,c("sigma")])

ypred <- rnorm(length(matrix_mean), matrix_mean, matrix_sigma)
mean(ypred)
set.seed(1)
quantile(ypred, probs = c(0.025, 0.975))

####
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned)


################ IPD-MA
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "ADAS_TRANSFORMED_OBS", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE)

###imputation using jomo
options(na.action = 'na.pass')
bb <- model.matrix(y ~ (AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE)*treat, data = data_full)
interaction_covariates <- c("agetreat", "sextreat", "weighttreat", "aptreat", "amnotaptreat", "adasbasetreat", "cdrbasetreat")
colnames(bb)[c(1,10:16)] <- c("Intercept", interaction_covariates)
data_response <- data_full %>% select(y, y4, y6, y8, y12, y16, y18, y20)
bb <- cbind(bb, data_response)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates, "treat", interaction_covariates)])
clus <- data_full$studyid
Z <- data_full[,"treat", drop = FALSE] #covariates associated to random effects

###Need to compute imputation
#imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "IPDMA-ADAS-imputations.RData")

load("IPDMA-ADAS-imputations.RData")
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
#save(samples, file = "IPDMA-ADAS-result.RData")

##### Finding patient specific treatment effect
load("IPDMA-ADAS-result.RData")

#samples_test <-samples[,c(grep(c("beta"), colnames(samples[[1]])), grep("gamma", colnames(samples[[1]])) )]
#coda::gelman.plot(samples_test) #check gelman diagnostic plot
coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, USUBJID, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- bb2 %*% IPDMA_coefs

#hist(patient_specific_effect_IPDMA, xlab = "patient specific treatment effect with ADAS outcome", main = "")

df <- data.frame(patient_specific_effect_IPDMA = patient_specific_effect_IPDMA)
ggplot(df, aes(x = patient_specific_effect_IPDMA)) +
  geom_histogram(colour = "#1F3552", fill = "#4271AE") + 
  labs(x = "patient specific treatment effect for ADAS outcome") +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank())

##################### unstandardize result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, model = "IPDMA")

c(result2$y)
sqrt(diag(result2$Sigma))




#####################################CIBIC_PLUS_TRANSFORMED
#####predicting placebo group
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_PLUS_TRANSFORMED", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE) %>% filter(treat == 0)

#find which model fitted best
data_full2 <- data_full %>% drop_na(y)
crossvalidation <- crossvalidate(data_full2, "bayes")
apply(crossvalidation, 1, mean)

###imputation using jomo
bb <- data_full %>% select(all_of(covariates), y, y4, y6, y8, y12, y16, y18, y20)
bb <- bb %>% mutate(Intercept = 1)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates)])
clus <- data_full$studyid

###Need to compute imputation
#imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "prediction-CIBIC-imputations.RData")

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

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
load("prediction-CIBIC-result.RData")
arbitrary_patient <- c(70, 1, 60, 0, 1, 30, 8)
arbitrary_patient_standardized <- (arbitrary_patient - data_cleaned$mean_val) / data_cleaned$sd_val

matrix_coefs<- as.matrix(samples[, c("alpha", paste0("beta[", 1:7,"]"))])
matrix_mean <- matrix_coefs %*% c(1,arbitrary_patient_standardized)
matrix_sigma <- as.matrix(samples[,c("sigma")])

set.seed(1)
ypred <- rnorm(length(matrix_mean), matrix_mean, matrix_sigma)
mean(ypred)
quantile(ypred, probs = c(0.025, 0.975))

result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned)

c(result2$y)
sqrt(diag(result2$Sigma))

################ IPD-MA
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_PLUS_TRANSFORMED", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE)

###imputation using jomo
options(na.action = 'na.pass')
bb <- model.matrix(y ~ (AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE)*treat, data = data_full)
interaction_covariates <- c("agetreat", "sextreat", "weighttreat", "aptreat", "amnotaptreat", "adasbasetreat", "cdrbasetreat")
colnames(bb)[c(1,10:16)] <- c("Intercept", interaction_covariates)
data_response <- data_full %>% select(y, y4, y6, y8, y12, y16, y18, y20)
bb <- cbind(bb, data_response)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates, "treat", interaction_covariates)])
clus <- data_full$studyid
Z <- data_full[,"treat", drop = FALSE] #covariates associated to random effects

###Need to compute imputation
#imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "IPDMA-CIBIC-imputations.RData")

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
save(samples, file = "IPDMA-CIBIC-result.RData")



##### Finding patient specific treatment effect
load("IPDMA-CIBIC-result.RData")

coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, USUBJID, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- bb2 %*% IPDMA_coefs

hist(patient_specific_effect_IPDMA, xlab = "patient specific treatment effect with CIBIC outcome", main = "")

result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, "IPDMA")

c(result2$y)
sqrt(diag(result2$Sigma))


#####################################Dropout
#####predicting placebo group
study_inclusion <- c(161, 231, 302, 304, 311, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "Dropout", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:y) %>% filter(treat == 0)

#find which model fitted best
data_full2 <- data_full %>% drop_na(y)
crossvalidation <- crossvalidate(data_full2, "bayes_binom")
apply(crossvalidation, 1, mean)


########################

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

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
load("prediction-Dropout-result.RData")
arbitrary_patient <- c(70, 1, 60, 0, 1, 30, 8)
arbitrary_patient_standardized <- (arbitrary_patient - data_cleaned$mean_val) / data_cleaned$sd_val

matrix_coefs<- as.matrix(samples[, c("alpha", paste0("beta[", 1:7,"]"))])
matrix_mean <- matrix_coefs %*% c(1,arbitrary_patient_standardized)

inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

mean(inv_logit(matrix_mean))
quantile(inv_logit(matrix_mean), probs = c(0.025, 0.975))

#set.seed(1)
#ypred <- rbinom(length(matrix_mean), 1, inv_logit(matrix_mean) )
#mean(ypred)
#quantile(ypred, probs = c(0.025, 0.975))

result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, "prediction")

c(result2$y)
sqrt(diag(result2$Sigma))


################ IPD-MA
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


load("IPDMA-Dropout-result.RData")
coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- exp(bb2 %*% IPDMA_coefs)

hist(patient_specific_effect_IPDMA, xlab = "patient specific treatment effect with Dropout outcome in odds ratio", main = "")


result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, "IPDMA")

c(result2$y)
sqrt(diag(result2$Sigma))
