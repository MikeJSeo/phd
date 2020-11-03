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

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)


###internal validation
y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)
result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachIV-powerprior.txt", n.iter = 10000, powerprior = TRUE)
summary(result)