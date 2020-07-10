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

#SCQM2 <- as.matrix(SCQM)
#apply(SCQM2, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
#apply(SCQM2, 2, function(x) sd(as.numeric(x), na.rm = TRUE))
#SCQM %>% summarise_if(is.numeric, mean, na.rm = TRUE, digits = 5)
#SCQM %>% summarise_if(is.numeric, sd, na.rm = TRUE)
#sapply(SCQM, function(x) sum(length(which(is.na(x)))))  


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


# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI.RData")
load("TOWARD-ApproachI.RData")
load("BSRBR-ApproachI.RData")
load("SCQM-ApproachI.Rdata")
y_TOWARD2 <- c(4.96455891, 0.15079734, -0.11131923, 0.0217670, 0.07085947,
               0.20328172, 0.02549393, 0.02943369, 0.18806348, 0.45842890,
               -0.08158954, 0.18847061, -0.04329039, -0.17427219, -0.14564021,
               -0.13871368, 0.08763985, -0.03790719, -0.03013991, -1.71212275)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2.xlsx", col_names = FALSE))
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

#S <- solve(r1[[2]])
#R <- cov2cor(S)
# Check if Omega_TOWARD2 typed correctly
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#   for(j in 1:20){
#     aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#   }
# }



#multivariate
result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachII.txt")
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

predictFn(SCQM, result, measure= "calibration")
predictFn(BSRBR, result, measure= "calibration")

#univariate
# result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
#                       y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-fixed.txt", univariate = TRUE)
# predictFn(SCQM, result)$MSE
# predictFn(BSRBR, result)$MSE


## external validation
#multivariate 
result <- secondStage2(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                      y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachII-external.txt")
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(SCQM, result, measure = "calibration")

result <- secondStage2(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachII-external.txt")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")
predictFn(BSRBR, result, measure = "calibration")

#univariate
# result <- secondStage2(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
#                        y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-fixed-external.txt", univariate = TRUE)
# 
# result <- secondStage2(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
#                        y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-fixed-external.txt", univariate = TRUE)