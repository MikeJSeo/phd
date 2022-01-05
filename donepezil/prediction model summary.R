library(jomo) #for imputation
library(mitools) #convenient function to combine imputations
library(dplyr)
library(tidyr)
library(lme4) #linear mixed effects model
library(glmnet)
library(rjags) #for running JAGS
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

load("prediction-ADAS-result.RData")
summary(samples) # standardized model result

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
arbitrary_patient <- c(70, 1, 60, 0, 1, 30, 8)
arbitrary_patient_standardized <- (arbitrary_patient - data_cleaned$mean_val) / data_cleaned$sd_val

matrix_coefs<- as.matrix(samples[, c("alpha", paste0("beta[", 1:7,"]"))])
matrix_mean <- matrix_coefs %*% c(1,arbitrary_patient_standardized)
matrix_sigma <- as.matrix(samples[,c("sigma")])

ypred <- rnorm(length(matrix_mean), matrix_mean, matrix_sigma)
mean(ypred)
set.seed(1)
quantile(ypred, probs = c(0.025, 0.975))

### unstandardize result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned)

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3

##### Secondary outcome: CIBIC_PLUS_TRANSFORMED
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_PLUS_TRANSFORMED", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE) %>% filter(treat == 0)

load("prediction-CIBIC-result.RData")
summary(samples) # standardized model result

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
arbitrary_patient <- c(70, 1, 60, 0, 1, 30, 8)
arbitrary_patient_standardized <- (arbitrary_patient - data_cleaned$mean_val) / data_cleaned$sd_val

matrix_coefs<- as.matrix(samples[, c("alpha", paste0("beta[", 1:7,"]"))])
matrix_mean <- matrix_coefs %*% c(1,arbitrary_patient_standardized)
matrix_sigma <- as.matrix(samples[,c("sigma")])

set.seed(1)
ypred <- rnorm(length(matrix_mean), matrix_mean, matrix_sigma)
mean(ypred)
quantile(ypred, probs = c(0.025, 0.975))

### Unstandardize result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned)

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3

#######Analyzing Dropout
study_inclusion <- c(161, 231, 302, 304, 311, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "Dropout", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:y) %>% filter(treat == 0)

load("prediction-Dropout-result.RData")
summary(samples) # standardized model result

## for arbitrary patient
## Age 70, Female (SEX = 1), WEIGHT = 60, AP = 0, AMNOTAP = 1, ADAS_TRANSFORMED_BASE = 30, CDR_TRANSFORMED_BASE = 8
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

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3

