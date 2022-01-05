# This code implements multiple imputation for IPDMA using jomo package
library(jomo) #for imputation
library(dplyr)
library(tidyr)

# First load the data. Do this before doing any analysis.
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
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE)

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

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "IPDMA-ADAS-imputations.RData")

###### Secondary outcome: CIBIC_PLUS_TRANSFORMED
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_TRANSFORMED_OBS", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE)

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

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "IPDMA-CIBIC-imputations.RData")
