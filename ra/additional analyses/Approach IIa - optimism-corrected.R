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

## REFLEX
# setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")
# samples_REFLEX <- firstStage(REFLEX, "first stage.txt")
# samples_TOWARD <- firstStage(TOWARD, "first stage.txt")
# samples_BSRBR <- firstStage(BSRBR, "first stage.txt")
# samples_SCQM <- firstStage(SCQM, "first stage.txt")
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI.RData")
# save(samples_SCQM, file = "SCQM-ApproachI.RData")


#################
# first stage analysis

# second stage analysis - load datasets
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI.RData")
load("TOWARD-ApproachI.RData")
load("BSRBR-ApproachI.RData")
load("SCQM-ApproachI.Rdata")

y_TOWARD2 <- c(4.9562, 0.1472, -0.1151, 0.0202, 0.0608, 0.1905, 0.0166,
               0.0774, 0.1813, 0.4509, -0.0800, 0.1921, -0.0379, -0.1439,
               -0.1313, -0.1356, 0.0382, -0.0359, -0.0117, -1.6994)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2.xlsx", col_names = FALSE))
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
optimism <- matrix(NA, nrow = 200, ncol = 8)
colnames(optimism) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3")
for(ii in 1:200){
  
  SCQM_bootstrap <- SCQM[sample(1:dim(SCQM)[1], replace = TRUE),]
  samples_SCQM_bootstrap <- firstStage(SCQM_bootstrap, "first stage.txt", mm = 1)
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
optimism2 <- matrix(NA, nrow = 200, ncol = 8)
colnames(optimism2) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3")
for(ii in 1:200){
  
  BSRBR_bootstrap <- BSRBR[sample(1:dim(BSRBR)[1], replace = TRUE),]
  samples_BSRBR_bootstrap <- firstStage(BSRBR_bootstrap, "first stage.txt", mm = 1)
  r1 <- summarize_each_study(samples_BSRBR_bootstrap)
  
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

