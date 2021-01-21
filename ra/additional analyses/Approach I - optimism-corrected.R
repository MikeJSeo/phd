setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")

library(readxl)
library(writexl)
library(tidyverse)
library(mice)
library(rjags)

setwd("C:/Users/ms19g661/Desktop")

# xlsx files
mydata <- read_excel("ra_dataset.xlsx")
BSRBR <- mydata %>% filter(study == "BSRBR")
SCQM <- mydata %>% filter(study == "SCQM")

# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")


#############apparent performance
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)
r1$y <- r1$y[c(1:10,29,30,11:28)]
r1$Omega <- r1$Omega[c(1:10,29,30,11:28), c(1:10,29,30,11:28)]

#internal validation
prediction_BSRBR <- findPrediction(BSRBR, coefs = r1)
performance_BSRBR <- findPerformance(prediction_BSRBR)
apparent_performance_BSRBR <- unlist(lapply(performance_BSRBR, mean))

###################
r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)
r2$y <- r2$y[c(1:10,29,30,11:28)]
r2$Omega <- r2$Omega[c(1:10,29,30,11:28), c(1:10,29,30,11:28)]

#internal validation
prediction_SCQM <- findPrediction(SCQM, coefs = r2)
performance_SCQM <- findPerformance(prediction_SCQM)
apparent_performance_SCQM <- unlist(lapply(performance_SCQM, mean))

setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files") #set the location to where JAGS file exists

###########Calculating optimism: SCQM

set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 8)
colnames(optimism) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3")
for(ii in 1:200){
 
  SCQM_bootstrap <- SCQM[sample(1:dim(SCQM)[1], replace = TRUE),]
  samples_SCQM_bootstrap <- firstStage(SCQM_bootstrap, "first stage-bayesLASSO.txt", mm = 1)
  
  r2 <- summarize_each_study(samples_SCQM_bootstrap)
  r2 <- unstandardize_coefficients(r2, SCQM_bootstrap)
  r2$y <- r2$y[c(1:10,29,30,11:28)]
  r2$Omega <- r2$Omega[c(1:10,29,30,11:28), c(1:10,29,30,11:28)]
  
  #internal validation
  prediction_SCQM_bootstrap <- findPrediction(SCQM_bootstrap, coefs = r2)
  performance_SCQM_bootstrap <- findPerformance(prediction_SCQM_bootstrap)
  
  prediction_SCQM_test <- findPrediction(SCQM, coefs = r2)
  performance_SCQM_test <- findPerformance(prediction_SCQM_test)
  
  optimism[ii,] <- mapply('-',lapply(performance_SCQM_bootstrap, mean),lapply(performance_SCQM_test, mean),SIMPLIFY=TRUE)
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_performance_SCQM <- apparent_performance_SCQM - optimism_averaged


###########Calculating optimism: BSRBR

set.seed(1)
optimism2 <- matrix(NA, nrow = 200, ncol = 9)
colnames(optimism2) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3", "rsquared")
for(ii in 1:200){
  
  BSRBR_bootstrap <- BSRBR[sample(1:dim(BSRBR)[1], replace = TRUE),]
  samples_BSRBR_bootstrap <- firstStage(BSRBR_bootstrap, "first stage-bayesLASSO.txt", mm = 1)
  
  r1 <- summarize_each_study(samples_BSRBR_bootstrap)
  r1 <- unstandardize_coefficients(r1, BSRBR_bootstrap)
  r1$y <- r1$y[c(1:10,29,30,11:28)]
  r1$Omega <- r1$Omega[c(1:10,29,30,11:28), c(1:10,29,30,11:28)]
  
  #internal validation
  prediction_BSRBR_bootstrap <- findPrediction(BSRBR_bootstrap, coefs = r1)
  performance_BSRBR_bootstrap <- findPerformance(prediction_BSRBR_bootstrap)
  
  prediction_BSRBR_test <- findPrediction(BSRBR, coefs = r1)
  performance_BSRBR_test <- findPerformance(prediction_BSRBR_test)
  
  optimism2[ii,] <- mapply('-',lapply(performance_BSRBR_bootstrap, mean),lapply(performance_BSRBR_test, mean),SIMPLIFY=TRUE)
}
optimism2_averaged <- apply(optimism2, 2, mean, na.rm = TRUE)
optimism2_corrected_performance_BSRBR <- apparent_performance_BSRBR - optimism2_averaged


