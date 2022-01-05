library(jomo) #for imputation
library(dplyr)
library(tidyr)

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

bb <- data_full %>% select(all_of(covariates), y, y4, y6, y8, y12, y16, y18, y20)
bb <- bb %>% mutate(Intercept = 1)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates)])
clus <- data_full$studyid

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "prediction-ADAS-imputations.RData")


##### Secondary outcome: CIBIC_PLUS_TRANSFORMED
study_inclusion <- c(161, 231, 302, 304, 311, 312, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "CIBIC_PLUS_TRANSFORMED", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:CDR_TRANSFORMED_BASE) %>% filter(treat == 0)

bb <- data_full %>% select(all_of(covariates), y, y4, y6, y8, y12, y16, y18, y20)
bb <- bb %>% mutate(Intercept = 1)

y_imputation <- as.data.frame(bb[,c("y", "y4", "y6", "y8", "y12", "y16", "y18", "y20")])
X_imputation <- as.data.frame(bb[,c("Intercept", covariates)])
clus <- data_full$studyid

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "prediction-CIBIC-imputations.RData")