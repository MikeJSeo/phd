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
y_TOWARD2 <- c(4.961614, 0.116018, -0.043371, 0.018451, 0.007829, 0.148655,
               -0.043279, 0.085000, 0.173144, 0.448602, -0.03,  0.072104, 
               -0.021726, -0.049325, -0.054995, -0.049588, 0.008290, -0.021396,
               -0.002223, -1.695539)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO.xlsx", col_names = FALSE))

setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

######### w = 0.25

W0 <- matrix(1, nrow = 10, ncol = 10)
W1 <- matrix(0.5, nrow = 10, ncol = 20)
W2 <- matrix(0.5, nrow = 20, ncol = 10)
W3 <- matrix(0.25, nrow = 20, ncol = 20)
W <- rbind(cbind(W0, W1), cbind(W2, W3))

result <- secondStage3(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

## External
result <- secondStage4(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")

result <- secondStage4(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")



######### w = 0.5

W0 <- matrix(1, nrow = 10, ncol = 10)
W1 <- matrix(sqrt(0.5), nrow = 10, ncol = 20)
W2 <- matrix(sqrt(0.5), nrow = 20, ncol = 10)
W3 <- matrix(0.5, nrow = 20, ncol = 20)
W <- rbind(cbind(W0, W1), cbind(W2, W3))

result <- secondStage3(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                       y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

## External
result <- secondStage4(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")

result <- secondStage4(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")


#### w = 0
W <- matrix(0, nrow = 30, ncol = 30)

result <- secondStage3(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                       y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-0.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

## External
result <- secondStage4(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external-0.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")


result <- secondStage4(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external-0.txt", W = W)
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")




#### w = 1
W <- matrix(1, nrow = 30, ncol = 30)

result <- secondStage3(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                       y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

## External
result <- secondStage4(samples1 = samples_BSRBR, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")

result <- secondStage4(samples1 = samples_SCQM, samples2 = samples_REFLEX, samples3 = samples_TOWARD, 
                       y4 = y_TOWARD2, Omega4 = Omega_TOWARD2, jags_file = "second stage-ApproachIIb-external.txt", W = W)
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")