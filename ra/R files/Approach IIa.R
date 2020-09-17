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
TOWARD <- mydata %>% filter(study == "TOWARD")
REFLEX <- mydata %>% filter(study == "REFLEX")

dataset <- SCQM
apply(dataset, 2, function(x) mean(as.numeric(x), na.rm = TRUE))
apply(dataset, 2, function(x) sd(as.numeric(x), na.rm = TRUE))
dataset %>% summarise_if(is.numeric, mean, na.rm = TRUE, digits = 5)
dataset %>% summarise_if(is.numeric, sd, na.rm = TRUE)
sapply(dataset, function(x) sum(length(which(is.na(x)))))


#################
# first stage analysis

## REFLEX
# setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")
# samples_REFLEX <- firstStage(REFLEX, "first stage.txt")
# samples_TOWARD <- firstStage(TOWARD, "first stage.txt")
# samples_BSRBR <- firstStage(BSRBR, "first stage.txt")
# samples_SCQM <- firstStage(SCQM, "first stage.txt")
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI.RData")
# save(samples_SCQM, file = "SCQM-ApproachI.RData")


# second stage analysis - load datasets
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
#S <- solve(r1[[2]])
#R <- cov2cor(S)
# Check if Omega_TOWARD2 typed correctly
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#   for(j in 1:20){
#     aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#   }
# }
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files") #set the location to where JAGS file exists

################################################################################

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)


y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)

#internal validation
result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII.txt")
prediction_SCQM <- findPrediction(SCQM, result)
prediction_BSRBR <- findPrediction(BSRBR, result)
findPerformance(prediction_SCQM)
findPerformance(prediction_BSRBR)


#internal-external validation
#all datasets except SCQM
y <- list(y1 = r1[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = y_TOWARD2)
Omega <- list(Omega1 = r1[[2]], Omega2 = r3[[2]], Omega3 = r4[[2]], Omega4 = Omega_TOWARD2)

result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII-external.txt")
prediction_SCQM <- findPrediction(SCQM, result)
findPerformance(prediction_SCQM)

#all datasets except BSRBR
y <- list(y1 = r2[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = y_TOWARD2)
Omega <- list(Omega1 = r2[[2]], Omega2 = r3[[2]], Omega3 = r4[[2]], Omega4 = Omega_TOWARD2)

result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII-external.txt")
prediction_BSRBR <- findPrediction(BSRBR, result)
findPerformance(prediction_BSRBR)

