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

dataset <- SCQM
apply(dataset, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(dataset, 2, function(x) sd(as.numeric(x), na.rm = TRUE))
dataset %>% summarise_if(is.numeric, mean, na.rm = TRUE, digits = 5)
dataset %>% summarise_if(is.numeric, sd, na.rm = TRUE)
sapply(dataset, function(x) sum(length(which(is.na(x)))))


#################
# first stage analysis

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
# S <- solve(r1[[2]])
# R <- cov2cor(S)
# #Check if Omega_TOWARD2 typed correctly
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#   for(j in 1:20){
#     aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#   }
# }
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files") #set the location to where JAGS file exists

################################################################################

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
prediction_SCQM_internal <- findPrediction(SCQM, result)
prediction_BSRBR_internal <- findPrediction(BSRBR, result)
performance_SCQM_internal <- findPerformance(prediction_SCQM_internal)
performance_BSRBR_internal <- findPerformance(prediction_BSRBR_internal)
lapply(performance_SCQM_internal, mean)
lapply(performance_BSRBR_internal, mean)
calibration_SCQM_internal <- findPerformance2(prediction_SCQM_internal)
calibration_BSRBR_internal <- findPerformance2(prediction_BSRBR_internal)

#internal-external validation
#all datasets except SCQM
y <- list(y1 = r1[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = r5[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r3[[2]], Sigma3 = r4[[2]], Sigma4 = r5[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII-external.txt")
prediction_SCQM_external <- findPrediction(SCQM, result)
performance_SCQM_external <- findPerformance(prediction_SCQM_external)
lapply(performance_SCQM_external, mean)
calibration_SCQM_external <- findPerformance2(prediction_SCQM_external)

#all datasets except BSRBR
y <- list(y1 = r2[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = r5[[1]])
Sigma <- list(Sigma1 = r2[[2]], Sigma2 = r3[[2]], Sigma3 = r4[[2]], Sigma4 = r5[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII-external.txt")
prediction_BSRBR_external <- findPrediction(BSRBR, result)
performance_BSRBR_external <- findPerformance(prediction_BSRBR_external)
lapply(performance_BSRBR_external, mean)
calibration_BSRBR_external <- findPerformance2(prediction_BSRBR_external)

#weighted performance measure
performance_weighted <- mapply(c, performance_SCQM_external, performance_BSRBR_external)
lapply(performance_weighted, mean, na.rm = TRUE)

Approach2a.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal, prediction_BSRBR_external = prediction_BSRBR_external,
                         prediction_SCQM_internal = prediction_SCQM_internal, prediction_SCQM_external = prediction_SCQM_external,
                         calibration_BSRBR_internal = calibration_BSRBR_internal, calibration_BSRBR_external = calibration_BSRBR_external,
                         calibration_SCQM_internal = calibration_SCQM_internal, calibration_SCQM_external = calibration_SCQM_external) 

setwd("~/GitHub/phd/ra/Result")
save(Approach2a.result, file = "Approach2a.result.RData")

