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
simulated_data1 <- generate_data1()
test_data <- findTestData(simulated_data1) 

# naive method
naivepred <- findPrediction1(simulated_data1, method = "naive")
naiveperf <- findPerformance(test_data, naivepred)
apply(naiveperf, 1, mean, na.rm = TRUE)

# imputation
imputationpred <- findPrediction1(simulated_data1, method = "imputation")
imputationperf <- findPerformance(test_data, imputationpred)
apply(imputationperf, 1, mean, na.rm = TRUE)

# separate predictions
separatepred <- findPrediction1(simulated_data1, method = "separate")
separateperf <- findPerformance(test_data, separatepred)
apply(separateperf, 1, mean, na.rm = TRUE)




####################################
simulated_data2 <- generate_data2()
test_data <- findTestData(simulated_data2)

# naive method
naivepred <- findPrediction2(simulated_data2, method = "naive")
naiveperf <- findPerformance(test_data, naivepred)
apply(naiveperf, 1, mean, na.rm = TRUE)

# imputation
imputationpred <- findPrediction2(simulated_data2, method = "imputation")
imputationperf <- findPerformance(test_data, imputationpred)
apply(imputationperf, 1, mean, na.rm = TRUE)

# separate predictions
separatepred <- findPrediction2(simulated_data2, method = "separate")
separateperf <- findPerformance(test_data, separatepred)
apply(separateperf, 1, mean, na.rm = TRUE)



# Some notes:
# Missing data imputation (partially) following Chapter 7 in the book https://stefvanbuuren.name/fimd/sec-mlguidelines.html 
#2l.lmer Jolani (2018) works, but is too slow
#2l.pmm uses homoscedastic assumption
#2l.2stage.pmm does not work; gives error
#2l.2stage.norm implements the two-stage method by Resche-Rigon and White (2018)


