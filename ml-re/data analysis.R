# load up data
library(dplyr)

#setwd("C:/Users/ms19g661/Desktop")
setwd("C:/Users/mike/Desktop")
data <- read.csv("dataCBT.csv")

data <- as_tibble(data)
cols <- c("study", "gender", "relstat")
data <- data %>% mutate_at(cols, as.factor)

# use fully observed data
data <- data %>% na.omit() %>%
  mutate(across(c("baseline", "age"), scale))


###############################
#####machine learning methods

#packages
library(ggplot2)
library(ranger)
library(h2o)
library(ranger)
library(lme4)
library(gbm)
library(keras)

#setwd("~/GitHub/phd/ml-re")
setwd("C:/Users/mike/Desktop/Github/phd/ml-re")
source("helpful.functions.R")

#lm
lmpred1 <- findPredictionCBT(data, "lm")
lmperf1 <- findPerformanceCBT(data, lmpred1)
apply(lmperf1, 1, mean)

#lmer
lmerpred1 <- findPredictionCBT(data, "lmer")
lmerperf1 <- findPerformanceCBT(data, lmerpred1)
apply(lmerperf1, 1, mean)

#random forest nostudy
rfpred1 <- findPredictionCBT(data, "randomforest_nostudy")
rfperf1 <- findPerformanceCBT(data, rfpred1)
apply(rfperf1, 1, mean)

#random forest withstudy
rfpred2 <- findPredictionCBT(data, "randomforest_withstudy")
rfperf2 <- findPerformanceCBT(data, rfpred2)
apply(rfperf2, 1, mean)

#gbm nostudy
gbmpred1 <- findPredictionCBT(data, "gbm_nostudy")
gbmperf1 <- findPerformanceCBT(data, gbmpred1)
apply(gbmperf1, 1, mean)

#gbm with study
gbmpred2 <- findPredictionCBT(data, "gbm_withstudy")
gbmperf2 <- findPerformanceCBT(data, gbmpred2)
apply(gbmperf2, 1, mean)

#https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/
#keras
keraspred1 <- findPredictionCBT(data, "keras")
kerasperf1 <- findPerformanceCBT(data, keraspred1)
apply(kerasperf1, 1, mean)