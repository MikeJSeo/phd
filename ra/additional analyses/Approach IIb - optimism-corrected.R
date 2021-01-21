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
TOWARD <- mydata %>% filter(study == "TOWARD")
REFLEX <- mydata %>% filter(study == "REFLEX")


#################
# first stage analysis

# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI-bayesLASSO.RData")
load("TOWARD-ApproachI-bayesLASSO.RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")

y_TOWARD2 <- c(4.9625, 0.1060, -0.0278, 0.0143, -0.0066, 0.1309, -0.0560, 0.0974, 0.1670, 0.4551,
               -0.0196, 0.0416, -0.0129, -0.0222, -0.0291, -0.0275, -0.0015, -0.0100, -0.0043, -1.6943)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO.xlsx", col_names = FALSE))
X_mean <- c(0.83, 52.33, 9.10, 27.74, 0.82, 1.59, 1.53, 46.39, 6.54)
X_sd <- c(0.38, 12.11, 8.18, 6.44, 0.38, 1.46, 0.61, 24.74, 0.96)
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files") #set the location to where JAGS file exists


###############################################################################

### apparent performance
#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)

r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)

r3 <- summarize_each_study(samples_REFLEX)
r3 <- unstandardize_coefficients(r3, REFLEX)

r4 <- summarize_each_study(samples_TOWARD)
r4 <- unstandardize_coefficients(r4, TOWARD)

r5 <- list(y = y_TOWARD2, Omega = Omega_TOWARD2)
r5 <- unstandardize_coefficients(r5, X_mean = X_mean, X_sd = X_sd)

y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = r5[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]], Sigma5 = r5[[2]])

#internal validation
result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII.txt")
prediction_SCQM <- findPrediction(SCQM, result)
prediction_BSRBR <- findPrediction(BSRBR, result)
performance_SCQM <- findPerformance(prediction_SCQM)
performance_BSRBR <- findPerformance(prediction_BSRBR)
apparent_performance_SCQM <- unlist(lapply(performance_SCQM, mean))
apparent_performance_BSRBR <- unlist(lapply(performance_BSRBR, mean))




##### Finding optimism: SCQM
set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 9)
colnames(optimism) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3", "rsquared")
for(ii in 1:200){
  
  SCQM_bootstrap <- SCQM[sample(1:dim(SCQM)[1], replace = TRUE),]
  samples_SCQM_bootstrap <- firstStage(SCQM_bootstrap, "first stage-bayesLASSO.txt", mm = 1)
  r2 <- summarize_each_study(samples_SCQM_bootstrap)
  r2 <- unstandardize_coefficients(r2, SCQM_bootstrap)
  
  y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = r5[[1]])
  Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]], Sigma5 = r5[[2]])
  result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII.txt", n.iter = 10000)
  
  prediction_SCQM_bootstrap <- findPrediction(SCQM_bootstrap, result)
  performance_SCQM_bootstrap <- findPerformance(prediction_SCQM_bootstrap)
  
  prediction_SCQM_test <- findPrediction(SCQM, result)
  performance_SCQM_test <- findPerformance(prediction_SCQM_test)
  
  optimism[ii,] <- mapply('-',lapply(performance_SCQM_bootstrap, mean),lapply(performance_SCQM_test, mean),SIMPLIFY=TRUE)
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_performance_SCQM <- apparent_performance_SCQM - optimism_averaged

##### Finding optimism: BSRBR
set.seed(1)
optimism2 <- matrix(NA, nrow = 200, ncol = 9)
colnames(optimism2) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3", "rsquared")
for(ii in 1:200){
  
  BSRBR_bootstrap <- BSRBR[sample(1:dim(BSRBR)[1], replace = TRUE),]
  samples_BSRBR_bootstrap <- firstStage(BSRBR_bootstrap, "first stage-bayesLASSO.txt", mm = 1)
  r1 <- summarize_each_study(samples_BSRBR_bootstrap)
  r1 <- unstandardize_coefficients(r1, BSRBR_bootstrap)
  
  y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = r5[[1]])
  Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]], Sigma5 = r5[[2]])
  result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII.txt", n.iter = 10000)
  
  prediction_BSRBR_bootstrap <- findPrediction(BSRBR_bootstrap, result)
  performance_BSRBR_bootstrap <- findPerformance(prediction_BSRBR_bootstrap)
  
  prediction_BSRBR_test <- findPrediction(BSRBR, result)
  performance_BSRBR_test <- findPerformance(prediction_BSRBR_test)
  
  optimism2[ii,] <- mapply('-',lapply(performance_BSRBR_bootstrap, mean),lapply(performance_BSRBR_test, mean),SIMPLIFY=TRUE)
}
optimism2_averaged <- apply(optimism2, 2, mean, na.rm = TRUE)
optimism_corrected_performance_BSRBR <- apparent_performance_BSRBR - optimism2_averaged

