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
load("IPDMA-ADAS-result.RData")
summary(samples) # standardized model result

### Finding patient specific treatment effect
#samples_test <-samples[,c(grep(c("beta"), colnames(samples[[1]])), grep("gamma", colnames(samples[[1]])) )]
#coda::gelman.plot(samples_test) #check gelman diagnostic plot
coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, USUBJID, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- bb2 %*% IPDMA_coefs

df <- data.frame(patient_specific_effect_IPDMA = patient_specific_effect_IPDMA)
ggplot(df, aes(x = patient_specific_effect_IPDMA)) +
  geom_histogram(colour = "#1F3552", fill = "#4271AE") + 
  labs(x = "patient specific treatment effect for ADAS outcome") +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank())

### unstandardize result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, model = "IPDMA")

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3                       


###### Secondary outcome: CIBIC_PLUS_TRANSFORMED

##### Finding patient specific treatment effect
load("IPDMA-CIBIC-result.RData")
summary(samples) # standardized model result

coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, USUBJID, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- bb2 %*% IPDMA_coefs

#hist(patient_specific_effect_IPDMA, xlab = "patient specific treatment effect with CIBIC outcome", main = "")
df <- data.frame(patient_specific_effect_IPDMA = patient_specific_effect_IPDMA)
ggplot(df, aes(x = patient_specific_effect_IPDMA)) +
  geom_histogram(colour = "#1F3552", fill = "#4271AE") + 
  labs(x = "patient specific treatment effect for CIBIC outcome") +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())


### Unstandardize result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, "IPDMA")

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3

####### Analyzing Dropout
study_inclusion <- c(161, 231, 302, 304, 311, 315, 339)
covariates <- c("AGE", "SEX", "WEIGHT", "AP", "AMNOTAP", "ADAS_TRANSFORMED_BASE", "CDR_TRANSFORMED_BASE")
data_cleaned <- clean_data(data, study_inclusion = study_inclusion, Outcome = "Dropout", covariates = covariates)
data_full <- data_cleaned$data_full
#check how many NAs are in each column:
#checkNA <- data_full %>% summarize_all(~sum(is.na(.)))
#View(checkNA)
data_full <- data_full %>% drop_na(studyid:y)

load("IPDMA-Dropout-result.RData")
summary(result) # standardized model result

coefs <- summary(samples)$statistics[,1]
IPDMA_coefs <- coefs[c("d", "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", "gamma[6]", "gamma[7]")]

data_full1 <- as_tibble(data_full)
data_full1 <- data_full1 %>% select(studyid, AGE, SEX, WEIGHT, AP, AMNOTAP, ADAS_TRANSFORMED_BASE, CDR_TRANSFORMED_BASE) %>%
  na.omit()
bb2 <- model.matrix( ~ AGE + SEX + WEIGHT + AP + AMNOTAP + ADAS_TRANSFORMED_BASE + CDR_TRANSFORMED_BASE, data = data_full1)
patient_specific_effect_IPDMA <- exp(bb2 %*% IPDMA_coefs)

hist(patient_specific_effect_IPDMA, xlab = "patient specific treatment effect with Dropout outcome in odds ratio", main = "")

df <- data.frame(patient_specific_effect_IPDMA = patient_specific_effect_IPDMA)
ggplot(df, aes(x = patient_specific_effect_IPDMA)) +
  geom_histogram(colour = "#1F3552", fill = "#4271AE") + 
  labs(x = "patient specific treatment effect with dropout outcome in odds ratio") +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank())

# Unstandardizing result
result <- summarize_each_sample(samples)
result2 <- unstandardize_cofficient(result, data_cleaned, "IPDMA")

result2_mean <- matrix(apply(result2, 2, mean), nrow = 1)
result2_lower <- matrix(apply(result2, 2, quantile, prob = 0.025), nrow = 1)
result2_upper <- matrix(apply(result2, 2, quantile, prob = 0.975), nrow = 1)
result3 <- rbind(result2_mean, result2_lower, result2_upper)
colnames(result3) <- colnames(result$samples_result)
rownames(result3) <- c("mean", "0.025 quantile", "0.975 quantile")
result3
