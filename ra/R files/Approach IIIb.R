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

setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

###########################
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


W50 <- matrix(sqrt(0.5), nrow = 30, ncol = 30)
W50[1:10,1:10] <- 1
W50[11:30,11:30] <- 0.5

W25 <- matrix(sqrt(0.25), nrow = 30, ncol = 30)
W25[1:10,1:10] <- 1
W25[11:30,11:30] <- 0.25

###internal validation
y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]][-(1:10)], y4 = r4[[1]][-(1:10)], y5 = r5[[1]][-(1:10)])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]][11:20, 11:20], Sigma4 = r4[[2]][11:20, 11:20], Sigma5 = r5[[2]][11:20, 11:20])
result <- secondStage(y = y, Sigma = Sigma, W = W25, jags_file = "second stage-ApproachIII.txt")

prediction_SCQM_internal_25 <- findPrediction(SCQM, result, calibration = r2)
prediction_BSRBR_internal_25 <- findPrediction(BSRBR, result, calibration = r1)
performance_SCQM_internal_25 <- findPerformance(prediction_SCQM_internal_25)
performance_BSRBR_internal_25 <- findPerformance(prediction_BSRBR_internal_25)
lapply(performance_SCQM_internal_25, mean)
lapply(performance_BSRBR_internal_25, mean)
calibration_SCQM_internal_25 <- findPerformance2(prediction_SCQM_internal_25)
calibration_BSRBR_internal_25 <- findPerformance2(prediction_BSRBR_internal_25)


result <- secondStage(y = y, Sigma = Sigma, W = W50, jags_file = "second stage-ApproachIII.txt")

prediction_SCQM_internal_50 <- findPrediction(SCQM, result, calibration = r2)
prediction_BSRBR_internal_50 <- findPrediction(BSRBR, result, calibration = r1)
performance_SCQM_internal_50 <- findPerformance(prediction_SCQM_internal_50)
performance_BSRBR_internal_50 <- findPerformance(prediction_BSRBR_internal_50)
lapply(performance_SCQM_internal_50, mean)
lapply(performance_BSRBR_internal_50, mean)
calibration_SCQM_internal_50 <- findPerformance2(prediction_SCQM_internal_50)
calibration_BSRBR_internal_50 <- findPerformance2(prediction_BSRBR_internal_50)

#####################################################


###internal-external validation
#all datasets except SCQM
y <- list(y1 = r1[[1]], y2 = r3[[1]][-(1:10)], y3 = r4[[1]][-(1:10)], y4 = r5[[1]][-(1:10)])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r3[[2]][11:20, 11:20], Sigma3 = r4[[2]][11:20, 11:20], Sigma4 = r5[[2]][11:20, 11:20])

result <- secondStage(y = y, Sigma = Sigma, W = W25, jags_file = "second stage-ApproachIII-external.txt")

prediction_SCQM_external_25 <- findPrediction(SCQM, result, calibration = r1)
performance_SCQM_external_25 <- findPerformance(prediction_SCQM_external_25)
lapply(performance_SCQM_external_25, mean)
calibration_SCQM_external_25 <- findPerformance2(prediction_SCQM_external_25)

result <- secondStage(y = y, Sigma = Sigma, W = W50, jags_file = "second stage-ApproachIII-external.txt")

prediction_SCQM_external_50 <- findPrediction(SCQM, result, calibration = r1)
performance_SCQM_external_50 <- findPerformance(prediction_SCQM_external_50)
lapply(performance_SCQM_external_50, mean)
calibration_SCQM_external_50 <- findPerformance2(prediction_SCQM_external_50)

#all datasets except BSRBR
y <- list(y1 = r2[[1]], y2 = r3[[1]][-(1:10)], y3 = r4[[1]][-(1:10)], y4 = r5[[1]][-(1:10)])
Sigma <- list(Sigma1 = r2[[2]], Sigma2 = r3[[2]][11:20, 11:20], Sigma3 = r4[[2]][11:20, 11:20], Sigma4 = r5[[2]][11:20, 11:20])

result <- secondStage(y = y, Sigma = Sigma, W = W25, jags_file = "second stage-ApproachIII-external.txt")

prediction_BSRBR_external_25 <- findPrediction(BSRBR, result, calibration = r2)
performance_BSRBR_external_25 <- findPerformance(prediction_BSRBR_external_25)
lapply(performance_BSRBR_external_25, mean)
calibration_BSRBR_external_25 <- findPerformance2(prediction_BSRBR_external_25)

result <- secondStage(y = y, Sigma = Sigma, W = W50, jags_file = "second stage-ApproachIII-external.txt")

prediction_BSRBR_external_50 <- findPrediction(BSRBR, result, calibration = r2)
performance_BSRBR_external_50 <- findPerformance(prediction_BSRBR_external_50)
lapply(performance_BSRBR_external_50, mean)
calibration_BSRBR_external_50 <- findPerformance2(prediction_BSRBR_external_50)

#weighted performance measure
performance_weighted25 <- mapply(c, performance_SCQM_external_25, performance_BSRBR_external_25)
lapply(performance_weighted25, mean, na.rm = TRUE)

performance_weighted50 <- mapply(c, performance_SCQM_external_50, performance_BSRBR_external_50)
lapply(performance_weighted50, mean, na.rm = TRUE)


Approach3b25.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal_25, prediction_BSRBR_external = prediction_BSRBR_external_25,
                            prediction_SCQM_internal = prediction_SCQM_internal_25, prediction_SCQM_external = prediction_SCQM_external_25,
                            calibration_BSRBR_internal = calibration_BSRBR_internal_25, calibration_BSRBR_external = calibration_BSRBR_external_25,
                            calibration_SCQM_internal = calibration_SCQM_internal_25, calibration_SCQM_external = calibration_SCQM_external_25) 

Approach3b50.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal_50, prediction_BSRBR_external = prediction_BSRBR_external_50,
                            prediction_SCQM_internal = prediction_SCQM_internal_50, prediction_SCQM_external = prediction_SCQM_external_50,
                            calibration_BSRBR_internal = calibration_BSRBR_internal_50, calibration_BSRBR_external = calibration_BSRBR_external_50,
                            calibration_SCQM_internal = calibration_SCQM_internal_50, calibration_SCQM_external = calibration_SCQM_external_50)  

setwd("~/GitHub/phd/ra/Result")
save(Approach3b25.result, file = "Approach3b25.result.RData")
save(Approach3b50.result, file = "Approach3b50.result.RData")
