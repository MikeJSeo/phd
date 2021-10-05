
###Test trial

simulated_data <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, signal = "small", sign = "different", interaction = FALSE)
simulated_dataset <- simulated_data$dataset

validation_data <- generate_sysmiss_ipdma_example(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, signal = "small", sign = "different", interaction = FALSE)
validation_dataset <- validation_data$dataset

naivepred <- naive_prediction(simulated_dataset, validation_dataset)
imputationpred <- imputation_prediction(simulated_dataset, validation_dataset)
separatepred <- separate_prediction(simulated_dataset, validation_dataset)


testdata <- findTestingOutcome(validation_dataset)

naiveperf <- findPerformance(testdata, naivepred)
imputationperf <- findPerformance(testdata, imputationpred)
separateperf <- findPerformance(testdata, separatepred)

naiveperf
imputationperf
separateperf




#####################################
#devtools::install_github("MikeJSeo/bipd") 
library(bipd)
library(dplyr)
library(mvtnorm)
library(lme4)
library(micemd)

#library(mitools)

setwd("~/GitHub/phd/missing")
source("helpful.functions.R")
source("simulation.related.functions.R")

####################################
# type of variable
type_of_var <- c("continuous", "binary", "binary", "continuous", "continuous", "continuous", "continuous", "binary", "binary", "binary")
names(type_of_var) <- paste0("x", 1:10)

store_result <- list()

#################################### simulation1
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation2
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation3
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation4
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation5
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation6
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation7
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation8
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation9
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation10
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation11
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation12
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation13
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation14
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation15
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation16
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation17
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation18
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation19
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation20
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation21
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation22
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation23
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation24
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation25
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation26
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation27
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation28
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation29
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation30
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation31
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation32
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation33
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation34
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation35
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation36
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation37
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation38
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation39
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation40
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation41
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation42
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation43
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation44
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation45
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation46
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation47
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation48
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation49
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation50
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation51
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation52
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation53
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation54
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation55
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation56
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation57
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation58
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation59
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation60
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation61
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation62
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation63
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation64
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.6, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation65
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation66
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation67
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation68
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation69
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation70
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation71
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation72
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation73
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation74
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation75
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation76
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation77
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation78
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation79
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation80
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation81
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation82
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation83
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation84
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation85
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation86
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation87
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation88
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation89
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation90
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation91
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation92
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation93
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation94
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation95
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation96
result <- wrapper_function(Nstudies = 15, Ncov = 5, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation97
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation98
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation99
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation100
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation101
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation102
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation103
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation104
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation105
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation106
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation107
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation108
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation109
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation110
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation111
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation112
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 4/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation113
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation114
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation115
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation116
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation117
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation118
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation119
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation120
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "no", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation121
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation122
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation123
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation124
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "small", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation125
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation126
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "no", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result

#################################### simulation127
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "small")
store_result[[length(store_result)+1]] <- result

#################################### simulation128
result <- wrapper_function(Nstudies = 15, Ncov = 10, sys_missing_prob = 8/15, nonlinear = "yes", signal = "large", interaction = "yes", heterogeneity = "large")
store_result[[length(store_result)+1]] <- result




# set.seed(2)
# simulated_data <- generate_simulation_data(Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
# simulated_dataset <- simulated_data$dataset
# 
# validation_data <- generate_simulation_data(Ncov = 10, sys_missing_prob = 0.6, nonlinear = "no", signal = "large", interaction = "no", heterogeneity = "large")
# validation_dataset <- validation_data$dataset
# 
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


