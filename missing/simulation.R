library(dplyr)
library(mvtnorm)
library(micemd)
library(lme4)
library(mitools)

#library(tidyr)

setwd("~/GitHub/phd/missing")
source("helpful.functions.R")
source("simulation.related.functions.R")

####################################
# type of variable
type_of_var <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
names(type_of_var) <- paste0("x", 1:10)


#################################### simulation1
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation2
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation3
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation4
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation5
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation6
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation7
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation8
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation9
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation10
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation11
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation12
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation13
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation14
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation15
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation16
result <- wrapper_function(Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation17
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation18
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation19
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation20
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation21
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation22
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation23
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation24
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation25
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation26
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation27
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation28
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation29
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation30
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation31
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation32
result <- wrapper_function(Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation33
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation34
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation35
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation36
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation37
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation38
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation39
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation40
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation41
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation42
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation43
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation44
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation45
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation46
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation47
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation48
result <- wrapper_function(Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation49
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation50
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation51
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation52
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation53
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation54
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation55
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation56
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")

#################################### simulation57
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")

#################################### simulation58
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")

#################################### simulation59
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")

#################################### simulation60
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")

#################################### simulation61
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")

#################################### simulation62
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")

#################################### simulation63
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")

#################################### simulation64
result <- wrapper_function(Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")




# set.seed(i)
# simulated_data <- generate_simulation_data(Ncov = 10, sys_missing_prob = 0.6, signal = "large", nonlinear = "yes", interaction = "no", heterogeneity = "large")
# simulated_dataset <- simulated_data$dataset
# 
# validation_data <- generate_simulation_data(Ncov = 10, sys_missing_prob = 0.6, signal = "large", nonlinear = "no", interaction = "no", heterogeneity = "large")
# validation_dataset <- validation_data$dataset

# naivepred <- naive_prediction(simulated_dataset, validation_dataset)
# naiveperf <- findPerformance(validation_dataset$y, naivepred)
# naiveperf
# 
# separatepred <- separate_prediction(simulated_dataset, validation_dataset)
# separateperf <- findPerformance(validation_dataset$y, separatepred)
# separateperf
# 
# imputationpred <- imputation_prediction(simulated_dataset, validation_dataset, type_of_var = type_of_var)
# imputationperf <- findPerformance(validation_dataset$y, imputationpred)
# imputationperf
#
# apply(result$naive_store, 2, mean)
# apply(result$imputation_store, 2, mean)
# apply(result$separate_store, 2, mean)


