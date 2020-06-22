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

####################### train test split test
SCQM2 <- SCQM %>% mutate(id = row_number())

#Create training set
train <- SCQM2 %>% sample_frac(.70)
#Create test set
test  <- anti_join(SCQM2, train, by = 'id')

train$id <- NULL
test$id <- NULL

result_SCQM <- firstStage(train, "first stage.txt",mm =2)
result <- result_SCQM[,c(1:10,39,40,20:37)]
predictFn(test, result, measure = "mse")
predictFn(test, result, measure = "bias")

BSRBR2 <- BSRBR %>% mutate(id = row_number())

#Create training set
train <- BSRBR2 %>% sample_frac(.70)
#Create test set
test  <- anti_join(BSRBR2, train, by = 'id')

train$id <- NULL
test$id <- NULL

result_BSRBR <- firstStage(train, "first stage.txt",mm =2)
result <- result_BSRBR[,c(1:10,39,40,20:37)]
predictFn(test, result, measure = "mse")
predictFn(test, result, measure = "bias")


#############ApproachI
result <- samples_BSRBR[,c(1:10,39,40,20:37)]
#internal
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")

#internal-external
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")

result <- samples_SCQM[,c(1:10,39,40,20:37)]

#internal
predictFn(SCQM, result, measure = "mse")
predictFn(SCQM, result, measure = "bias")
predictFn(SCQM, result, measure = "benefit")

#internal-external
predictFn(BSRBR, result, measure = "mse")
predictFn(BSRBR, result, measure = "bias")
