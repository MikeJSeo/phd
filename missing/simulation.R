library(dplyr)
library(mvtnorm)
library(mice) #pmm
library(miceadds) #2l.pmm
library(micemd) #2l.2stage.norm
library(mitools)
library(lme4)
library(tidyr)

setwd("~/GitHub/phd/missing")
source("helpful.functions.R")

####################################
simulated_data <- generate_data()
test_data <- findTestData(simulated_data)

# Some notes:
# Missing data imputation (partially) following Chapter 7 in the book https://stefvanbuuren.name/fimd/sec-mlguidelines.html 
#2l.lmer Jolani (2018) works, but is too slow
#2l.pmm uses homoscedastic assumption
#2l.2stage.pmm does not work; gives error
#2l.2stage.norm implements the two-stage method by Resche-Rigon and White (2018)

# naive approach
naivepred <- findPrediction(simulated_data, method = "naive")
save(naivepred, file = "naivepred.RData")
load("naivepred.RData")
naiveperf <- findPerformance(test_data, naivepred)
apply(naiveperf, 1, mean, na.rm = TRUE)

# imputation
imputationpred <- findPrediction(simulated_data, method = "imputation")
save(imputationpred, file = "imputationpred.RData")
load("imputationpred.RData")
imputationperf <- findPerformance(test_data, imputationpred)
apply(imputationperf, 1, mean, na.rm = TRUE)

# average predictions
averagepred <- findPrediction(simulated_data, method = "average_predictions")
#save(averagepred, file = "averagepred.RData")
load("averagepred.RData")
averageperf <- findPerformance(test_data, averagepred)
apply(averageperf, 1, mean, na.rm = TRUE)


