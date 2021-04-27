
# load up data
library(dplyr)

setwd("C:/Users/ms19g661/Desktop")
#setwd("C:/Users/mike/Desktop")
data <- read.csv("dataCBT.csv")
#data <- read.csv("data_ICBT.csv")

#quantifying heterogeneity
library(summarytools)
#view(dfSummary(data), method = "browser")

data %>% group_by(study) %>% summarize_all(~mean(., na.rm = TRUE))
data %>% group_by(study) %>% summarize_all(~sd(., na.rm = TRUE))


# change to factor
data <- as_tibble(data)
cols <- c("study", "gender", "relstat")
data <- data %>% mutate_at(cols, as.factor)

# count number of NAs
data %>% summarize_all(~sum(is.na(.)))

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
library(mvmeta)
library(bipd)

setwd("~/GitHub/phd/ml-re")
#setwd("C:/Users/mike/Desktop/Github/phd/ml-re")
source("helpful.functions.R")


#you could fit the same model per study, and then meta-analyse coefficients
#two stage
twostagepred1 <- findPredictionCBT(data, "two-stage")
twostageperf1 <- findPerformanceCBT(data, twostagepred1$predictions)
apply(twostageperf1, 1, mean)

#lasso
lassopred1 <- findPredictionCBT(data, "lasso")
lassoperf1 <- findPerformanceCBT(data, lassopred1$predictions)
apply(lassoperf1, 1, mean)

#ridge
ridgepred1 <- findPredictionCBT(data, "ridge")
ridgeperf1 <- findPerformanceCBT(data, ridgepred1$predictions)
apply(ridgeperf1, 1, mean)


#average_prediction
avgpred1 <- findPredictionCBT(data, "average_prediction")
avgperf1 <- findPerformanceCBT(data, avgpred1$predictions)
apply(avgperf1, 1, mean)

#lm
lm0pred1 <- findPredictionCBT(data, "lm0")
lm0perf1 <- findPerformanceCBT(data, lm0pred1$predictions)
apply(lm0perf1, 1, mean)

#lm
lmpred1 <- findPredictionCBT(data, "lm")
lmperf1 <- findPerformanceCBT(data, lmpred1$predictions)
apply(lmperf1, 1, mean)

#lmer
lmerpred1 <- findPredictionCBT(data, "lmer")
lmerperf1 <- findPerformanceCBT(data, lmerpred1$predictions)
apply(lmerperf1, 1, mean)

#random forest h2o
rfh2opred1 <- findPredictionCBT(data, "randomforest_h2o")
rfh2operf1 <- findPerformanceCBT(data, rfh2opred1$predictions)
apply(rfh2operf1, 1, mean)

#random forest nostudy
rfpred1 <- findPredictionCBT(data, "randomforest_nostudy")
rfperf1 <- findPerformanceCBT(data, rfpred1$predictions)
apply(rfperf1, 1, mean)

#random forest withstudy
rfpred2 <- findPredictionCBT(data, "randomforest_withstudy")
rfperf2 <- findPerformanceCBT(data, rfpred2$predictions)
apply(rfperf2, 1, mean)

#gbm nostudy
gbmpred1 <- findPredictionCBT(data, "gbm_nostudy")
gbmperf1 <- findPerformanceCBT(data, gbmpred1$predictions)
apply(gbmperf1, 1, mean)

#gbm with study
gbmpred2 <- findPredictionCBT(data, "gbm_withstudy")
gbmperf2 <- findPerformanceCBT(data, gbmpred2$predictions)
apply(gbmperf2, 1, mean)

#https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_regression/
#keras
keraspred1 <- findPredictionCBT(data, "keras")
kerasperf1 <- findPerformanceCBT(data, keraspred1$predictions)
apply(kerasperf1, 1, mean)

#stacking
pred0 <- list(lmpred1 = lmpred1$predictions0, lmerpred1 = lmerpred1$predictions0, rfpred1 = rfpred1$predictions0)
Z_train <- stacked_model(pred0)
y_train <- stacked_model_y(data)
y_test <- stacked_model_y_test(data)

pred <- list(lmpred1 = lmpred1$predictions, lmerpred1 = lmerpred1$predictions, rfpred1 = rfpred1$predictions)
Z_test <- stacked_model(pred)

stackingpred1 <- find_stacked_result(data, Z_train, y_train, Z_test)
stackingperf1 <- findPerformanceCBT(data, stackingpred1)
apply(stackingperf1, 1, mean)

#stacking
stackpred1 <- findPredictionCBT(data, "stacking")
stackperf1 <- findPerformanceCBT(data, stackpred1$predictions)
apply(stackperf1, 1, mean)

