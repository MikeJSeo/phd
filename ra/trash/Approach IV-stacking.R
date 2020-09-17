setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")
source("helpful.functions.revised.R")

library(readxl)
library(writexl)
library(tidyverse)
library(mice)
library(rjags)


###############################################
setwd("C:/Users/ms19g661/Desktop")

# xlsx files
mydata <- read_excel("ra_dataset.xlsx")
BSRBR <- mydata %>% filter(study == "BSRBR")
SCQM <- mydata %>% filter(study == "SCQM")
TOWARD <- mydata %>% filter(study == "TOWARD")
REFLEX <- mydata %>% filter(study == "REFLEX")



######### internal validation

### approach I
# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")

result <- samples_BSRBR[,c(1:10,39,40,20:37)]

predictionI <- findPrediction(BSRBR, result)

### approach IIa
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI.RData")
load("TOWARD-ApproachI.RData")
load("BSRBR-ApproachI.RData")
load("SCQM-ApproachI.Rdata")
y_TOWARD2 <- c(4.96455891, 0.15079734, -0.11131923, 0.0217670, 0.07085947,
               0.20328172, 0.02549393, 0.02943369, 0.18806348, 0.45842890,
               -0.08158954, 0.18847061, -0.04329039, -0.17427219, -0.14564021,
               -0.13871368, 0.08763985, -0.03790719, -0.03013991, -1.71212275)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2.xlsx", col_names = FALSE))
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachII.txt")
predictionIIa <- findPrediction(BSRBR, result)

### approach IIb
# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI-bayesLASSO.RData")
load("TOWARD-ApproachI-bayesLASSO.RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")
y_TOWARD2 <- c(4.961614, 0.116018, -0.043371, 0.018451, 0.007829, 0.148655,
               -0.043279, 0.085000, 0.173144, 0.448602, -0.03,  0.072104, 
               -0.021726, -0.049325, -0.054995, -0.049588, 0.008290, -0.021396,
               -0.002223, -1.695539)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO.xlsx", col_names = FALSE))
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachII.txt")
predictionIIb <- findPrediction(BSRBR, result)


### approach IIc
result <- secondStage(samples1 = samples_BSRBR, samples2 = samples_SCQM, samples3 = samples_REFLEX, samples4 = samples_TOWARD, 
                      y5 = y_TOWARD2, Omega5 = Omega_TOWARD2, jags_file = "second stage-ApproachII.txt", niter = 200000)
result_BSRBR <- result
for(i in 1:10){
  result_BSRBR[[1]][,i] <- samples_BSRBR[[1]][,i]
  result_BSRBR[[2]][,i] <- samples_BSRBR[[2]][,i]
  result_BSRBR[[3]][,i] <- samples_BSRBR[[3]][,i]
}
predictionIIc <- findPrediction(BSRBR, result_BSRBR)


#fitting random forest using caret package

library(caret)

predI <- predictionI$pred
predIIa <- predictionIIa$pred
predIIb <- predictionIIb$pred
predIIc <- predictionIIc$pred

predDF <- data.frame(predI, predIIa, predIIb, predIIc, y = predictionI$y)
predDF <- predDF[complete.cases(predDF),]

modelStack <- train(y ~ ., data = predDF, method = "rf")
combPred <- predict(modelStack, predDF)

findPerformance(combPred, predDF$y, measure = "bias")


#####Internal external

### approach I
setwd("C:/Users/ms19g661/Desktop/RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")

result <- samples_SCQM[,c(1:10,39,40,20:37)]
predictionI <- findPrediction(BSRBR, result)

### approach IIa

