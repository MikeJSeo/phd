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

setwd("C:/Users/mike/Desktop/Github/phd/missing/simulation_results")
options(warn=-1) #options(warn=0)

#################################### simulation1
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation1.RData")

load("simulation1.RData")
wrapper_function2(result)

####################################  simulation2
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation2.RData")

load("simulation2.RData")
wrapper_function2(result)

#################################### simulation3
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation3.RData")

load("simulation3.RData")
wrapper_function2(result)

#################################### simulation4
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation4.RData")

load("simulation4.RData")
wrapper_function2(result)

#################################### simulation5
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation5.RData")

load("simulation5.RData")
wrapper_function2(result)

#################################### simulation6
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation6.RData")

load("simulation6.RData")
wrapper_function2(result)

#################################### simulation7
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation7.RData")

load("simulation7.RData")
wrapper_function2(result)

#################################### simulation8
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation8.RData")

load("simulation8.RData")
wrapper_function2(result)

#################################### simulation9
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation9.RData")

load("simulation9.RData")
wrapper_function2(result)

#################################### simulation10
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation10.RData")

load("simulation10.RData")
wrapper_function2(result)

#################################### simulation11
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation11.RData")

load("simulation11.RData")
wrapper_function2(result)

#################################### simulation12
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation12.RData")

load("simulation12.RData")
wrapper_function2(result)

#################################### simulation13
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation13.RData")

load("simulation13.RData")
wrapper_function2(result)

#################################### simulation14
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation14.RData")

load("simulation14.RData")
wrapper_function2(result)

#################################### simulation15
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation15.RData")

load("simulation15.RData")
wrapper_function2(result)

#################################### simulation16
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.1)
#save(result, file = "simulation16.RData")

load("simulation16.RData")
wrapper_function2(result)

#################################### simulation17
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation17.RData")

load("simulation17.RData")
wrapper_function2(result)

#################################### simulation18
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation18.RData")

load("simulation18.RData")
wrapper_function2(result)

#################################### simulation19
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation19.RData")

load("simulation19.RData")
wrapper_function2(result)

#################################### simulation20
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation20.RData")

load("simulation20.RData")
wrapper_function2(result)

#################################### simulation21
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation21.RData")

load("simulation21.RData")
wrapper_function2(result)

#################################### simulation22
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation22.RData")

load("simulation22.RData")
wrapper_function2(result)

#################################### simulation23
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation23.RData")

load("simulation23.RData")
wrapper_function2(result)

#################################### simulation24
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation24.RData")

load("simulation24.RData")
wrapper_function2(result)

#################################### simulation25
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation25.RData")

load("simulation25.RData")
wrapper_function2(result)

#################################### simulation26
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation26.RData")

load("simulation26.RData")
wrapper_function2(result)

#################################### simulation27
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation27.RData")

load("simulation27.RData")
wrapper_function2(result)

#################################### simulation28
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation28.RData")

load("simulation28.RData")
wrapper_function2(result)

#################################### simulation29
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation29.RData")

load("simulation29.RData")
wrapper_function2(result)

#################################### simulation30
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation30.RData")

load("simulation30.RData")
wrapper_function2(result)

#################################### simulation31
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation31.RData")

load("simulation31.RData")
wrapper_function2(result)

#################################### simulation32
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.1)
#save(result, file = "simulation32.RData")

load("simulation32.RData")
wrapper_function2(result)

#################################### simulation33
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation33.RData")

load("simulation33.RData")
wrapper_function2(result)

#################################### simulation34
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation34.RData")

load("simulation34.RData")
wrapper_function2(result)

#################################### simulation35
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation35.RData")

load("simulation35.RData")
wrapper_function2(result)

#################################### simulation36
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation36.RData")

load("simulation36.RData")
wrapper_function2(result)

#################################### simulation37
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation37.RData")

load("simulation37.RData")
wrapper_function2(result)

#################################### simulation38
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation38.RData")

load("simulation38.RData")
wrapper_function2(result)

#################################### simulation39
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation39.RData")

load("simulation39.RData")
wrapper_function2(result)

#################################### simulation40
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation40.RData")

load("simulation40.RData")
wrapper_function2(result)

#################################### simulation41
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation41.RData")

load("simulation41.RData")
wrapper_function2(result)

#################################### simulation42
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation42.RData")

load("simulation42.RData")
wrapper_function2(result)

#################################### simulation43
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation43.RData")

load("simulation43.RData")
wrapper_function2(result)

#################################### simulation44
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation44.RData")

load("simulation44.RData")
wrapper_function2(result)

#################################### simulation45
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation45.RData")

load("simulation45.RData")
wrapper_function2(result)

#################################### simulation46
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation46.RData")

load("simulation46.RData")
wrapper_function2(result)

#################################### simulation47
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation47.RData")

load("simulation47.RData")
wrapper_function2(result)

#################################### simulation48
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.2, heterogeneity = 0.3)
#save(result, file = "simulation48.RData")

load("simulation48.RData")
wrapper_function2(result)

#################################### simulation49
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation49.RData")

load("simulation49.RData")
wrapper_function2(result)

#################################### simulation50
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation50.RData")

load("simulation50.RData")
wrapper_function2(result)

#################################### simulation51
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation51.RData")

load("simulation51.RData")
wrapper_function2(result)

#################################### simulation52
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation52.RData")

load("simulation52.RData")
wrapper_function2(result)

#################################### simulation53
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation53.RData")

load("simulation53.RData")
wrapper_function2(result)

#################################### simulation54
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation54.RData")

load("simulation54.RData")
wrapper_function2(result)

#################################### simulation55
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation55.RData")

load("simulation55.RData")
wrapper_function2(result)

#################################### simulation56
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.1, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation56.RData")

load("simulation56.RData")
wrapper_function2(result)

#################################### simulation57
result <- wrapper_function(Nstudies = 2, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.3, heterogeneity = 0.3)
#save(result, file = "simulation57.RData")

load("simulation57.RData")
wrapper_function2(result)

#################################### simulation58
result <- wrapper_function(Nstudies = 3, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.3, heterogeneity = 0.3)
#save(result, file = "simulation58.RData")

load("simulation58.RData")
wrapper_function2(result)

#################################### simulation59
result <- wrapper_function(Nstudies = 5, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation59.RData")

load("simulation59.RData")
wrapper_function2(result)

#################################### simulation60
result <- wrapper_function(Nstudies = 10, Ncov = 5, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation60.RData")

load("simulation60.RData")
wrapper_function2(result)

#################################### simulation61
result <- wrapper_function(Nstudies = 2, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation61.RData")

load("simulation61.RData")
wrapper_function2(result)

#################################### simulation62
result <- wrapper_function(Nstudies = 3, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation62.RData")

load("simulation62.RData")
wrapper_function2(result)

#################################### simulation63
result <- wrapper_function(Nstudies = 5, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation63.RData")

load("simulation63.RData")
wrapper_function2(result)

#################################### simulation64
result <- wrapper_function(Nstudies = 10, Ncov = 10, sys_missing_prob = 0.3, magnitude = 0.5, heterogeneity = 0.3)
#save(result, file = "simulation64.RData")

load("simulation64.RData")
wrapper_function2(result)



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


