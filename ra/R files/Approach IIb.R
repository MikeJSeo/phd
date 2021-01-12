setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")

library(readxl)
library(writexl)
library(tidyverse)
library(mice)
library(rjags)


###############################################
setwd("C:/Users/ms19g661/Desktop")

# xlsx files
mydata <- read_excel("ra_dataset.xlsx")
BSRBR <- mydata %>% filter(study == "BSRBR")
SCQM <- mydata %>% filter(study == "SCQM")
TOWARD <- mydata %>% filter(study == "TOWARD")
REFLEX <- mydata %>% filter(study == "REFLEX")


# first stage analysis

## REFLEX
# setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")
# samples_REFLEX <- firstStage(REFLEX, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_TOWARD <- firstStage(TOWARD, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_BSRBR <- firstStage(BSRBR, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_SCQM <- firstStage(SCQM, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI-bayesLASSO.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI-bayesLASSO.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI-bayesLASSO.RData")
# save(samples_SCQM, file = "SCQM-ApproachI-bayesLASSO.RData")

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
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#  for(j in 1:20){
#    aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#  }
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

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII.txt")
prediction_SCQM_internal <- findPrediction(SCQM, result)
prediction_BSRBR_internal <- findPrediction(BSRBR, result)
performance_SCQM_internal <- findPerformance(prediction_SCQM_internal)
performance_BSRBR_internal <- findPerformance(prediction_BSRBR_internal)
lapply(performance_SCQM_internal, mean)
lapply(performance_BSRBR_internal, mean)
calibration_SCQM_internal <- findPerformance2(prediction_SCQM_internal)
calibration_BSRBR_internal <- findPerformance2(prediction_BSRBR_internal)

####internal-external validation
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

Approach2b.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal, prediction_BSRBR_external = prediction_BSRBR_external,
                          prediction_SCQM_internal = prediction_SCQM_internal, prediction_SCQM_external = prediction_SCQM_external,
                          calibration_BSRBR_internal = calibration_BSRBR_internal, calibration_BSRBR_external = calibration_BSRBR_external,
                          calibration_SCQM_internal = calibration_SCQM_internal, calibration_SCQM_external = calibration_SCQM_external) 

setwd("~/GitHub/phd/ra/Result")
save(Approach2b.result, file = "Approach2b.result.RData")



