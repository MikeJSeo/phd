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
y_TOWARD2 <- c(4.961614, 0.116018, -0.043371, 0.018451, 0.007829, 0.148655,
               -0.043279, 0.085000, 0.173144, 0.448602, -0.03,  0.072104, 
               -0.021726, -0.049325, -0.054995, -0.049588, 0.008290, -0.021396,
               -0.002223, -1.695539)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO.xlsx", col_names = FALSE))
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#   for(j in 1:20){
#     aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#   }
# }

setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

# result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
#                       y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-fixed.txt", univariate = TRUE)
# predictFn(SCQM, result)$MSE
# predictFn(BSRBR, result)$MSE

result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachII.txt")
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

predictFn(SCQM, result, measure= "calibration")
predictFn(BSRBR, result, measure= "calibration")


#external
result <- secondStage2(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachII-external.txt")
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(SCQM, result, measure= "calibration")

result <- secondStage2(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachII-external.txt")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")
predictFn(BSRBR, result, measure= "calibration")