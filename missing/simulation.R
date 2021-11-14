#devtools::install_github("MikeJSeo/bipd") 
library(bipd)
library(dplyr)
library(mvtnorm)
library(lme4)
library(micemd)

setwd("C:/Users/mike/Desktop/Github/phd/missing")
source("helpful.functions.R")
source("simulation.functions.R")


####################################
# type of variable
type_of_var <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
names(type_of_var) <- paste0("x", 1:10)

store_result <- list()

#################################### simulation1
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation2
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation3
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation4
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation5
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation6
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation7
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation8
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation9
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation10
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation11
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation12
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation13
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation14
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation15
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation16
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation17
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation18
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation19
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation20
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation21
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation22
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation23
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation24
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation25
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation26
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation27
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation28
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation29
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation30
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation31
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation32
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
store_result[[length(store_result)+1]] <- result

#################################### simulation33
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation34
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation35
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation36
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation37
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation38
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation39
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation40
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation41
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation42
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation43
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation44
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation45
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation46
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation47
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation48
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation49
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation50
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation51
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation52
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation53
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation54
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation55
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation56
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation57
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation58
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation59
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation60
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation61
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation62
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation63
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result

#################################### simulation64
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
store_result[[length(store_result)+1]] <- result



# Example
# simulated_data <- generate_sysmiss_ipdma_example(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1, interaction = FALSE, aggregation_bias = FALSE)
# simulated_dataset <- simulated_data$dataset
# 
# validation_data <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1, interaction = FALSE, aggregation_bias = FALSE)
# validation_dataset <- validation_data$dataset
# 
# naivepred <- naive_prediction(simulated_dataset, validation_dataset)
# imputationpred <- imputation_prediction(simulated_dataset, validation_dataset)
# imputation_noclusterpred <- imputation_prediction(simulated_dataset, validation_dataset, method = "imputation_nocluster")
# separatepred <- separate_prediction(simulated_dataset, validation_dataset)
# 
# testdata <- findTestingOutcome(validation_dataset)
# 
# naiveperf <- findPerformance(testdata, naivepred)
# imputationperf <- findPerformance(testdata, imputationpred)
# imputation_noclusterpref <- findPerformance(testdata, imputation_noclusterpred)
# separateperf <- findPerformance(testdata, separatepred)
# 
# naiveperf
# imputationperf
# imputation_noclusterpref
# separateperf


