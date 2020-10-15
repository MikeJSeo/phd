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

# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")


#############ApproachI
result <- samples_BSRBR[,c(1:10,39,40,20:37)]

#internal validation
prediction_BSRBR_internal <- findPrediction(BSRBR, result)
performance_BSRBR_internal <- findPerformance(prediction_BSRBR_internal)
lapply(performance_BSRBR_internal, mean)
calibration_BSRBR_internal <- findPerformance2(prediction_BSRBR_internal)

#internal-external validation
prediction_SCQM_external <- findPrediction(SCQM, result)
performance_SCQM_external <- findPerformance(prediction_SCQM_external)
lapply(performance_SCQM_external, mean)
calibration_SCQM_external <- findPerformance2(prediction_SCQM_external)

###################
result <- samples_SCQM[,c(1:10,39,40,20:37)]

#internal validation
prediction_SCQM_internal <- findPrediction(SCQM, result)
performance_SCQM_internal <- findPerformance(prediction_SCQM_internal)
lapply(performance_SCQM_internal, mean)
calibration_SCQM_internal <- findPerformance2(prediction_SCQM_internal)


#internal-external validation
prediction_BSRBR_external <- findPrediction(BSRBR, result)
performance_BSRBR_external <- findPerformance(prediction_BSRBR_external)
lapply(performance_BSRBR_external, mean)
calibration_BSRBR_external <- findPerformance2(prediction_BSRBR_external)

#weighted performance measure
performance_weighted <- mapply(c, performance_SCQM_external, performance_BSRBR_external)
lapply(performance_weighted, mean, na.rm = TRUE)

Approach1.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal, prediction_BSRBR_external = prediction_BSRBR_external,
                         prediction_SCQM_internal = prediction_SCQM_internal, prediction_SCQM_external = prediction_SCQM_external,
                         calibration_BSRBR_internal = calibration_BSRBR_internal, calibration_BSRBR_external = calibration_BSRBR_external,
                         calibration_SCQM_internal = calibration_SCQM_internal, calibration_SCQM_external = calibration_SCQM_external) 
                
setwd("~/GitHub/phd/ra/Result")         
save(Approach1.result, file = "Approach1.result.RData")