### this approach is for testing purposes; penalizing all parameters including main effects

setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")

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


#first stage analysis
# setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")
# samples_REFLEX <- firstStage(REFLEX, "first stage-bayesLASSO-full.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_TOWARD <- firstStage(TOWARD, "first stage-bayesLASSO-full.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_BSRBR <- firstStage(BSRBR, "first stage-bayesLASSO-full.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_SCQM <- firstStage(SCQM, "first stage-bayesLASSO-full.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI-bayesLASSO-full.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI-bayesLASSO-full.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI-bayesLASSO-full.RData")
# save(samples_SCQM, file = "SCQM-ApproachI-bayesLASSO-full.RData")

# second stage analysis
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI-bayesLASSO-full.RData")
load("TOWARD-ApproachI-bayesLASSO-full.RData")
load("BSRBR-ApproachI-bayesLASSO-full.RData")
load("SCQM-ApproachI-bayesLASSO-full.Rdata")

#estimates when penalization is applied for all
y_TOWARD2 <- c(4.9621, 0.0854, -0.0269, 0.0122, 0.0065, 0.1077, -0.0176, 0.0724, 0.1414, 0.4243, -0.0129, 0.0645, -0.0193, -0.0459, -0.0326, -0.0644, 0.0171, 0.0010, 0.0154, -1.6969)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO_full.xlsx", col_names = FALSE))
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#  for(j in 1:20){
#    aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#  }
# }
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")


################################################################################

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)


####second stage analysis

####internal validation
y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)

result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII.txt")
prediction_SCQM <- findPrediction(SCQM, result)
prediction_BSRBR <- findPrediction(BSRBR, result)
findPerformance(prediction_SCQM)
findPerformance(prediction_BSRBR)


####internal-external validation
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

