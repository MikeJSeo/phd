setwd("C:/Users/mike/Desktop/Github/phd/book chapter/book chapter part3")
source("helpful.functions.R")

library(readxl)
library(writexl)
library(tidyverse)
library(mice)
library(rjags)

setwd("C:/Users/mike/Desktop")

# xlsx files
mydata <- read_excel("ra_dataset.xlsx")
BSRBR <- mydata %>% filter(study == "BSRBR")
SCQM <- mydata %>% filter(study == "SCQM")
TOWARD <- mydata %>% filter(study == "TOWARD")
REFLEX <- mydata %>% filter(study == "REFLEX")

# no shrinkage two-stage analysis - load datasets
# samples_REFLEX <- firstStage(REFLEX, "first stage.txt")
# samples_TOWARD <- firstStage(TOWARD, "first stage.txt")
# samples_BSRBR <- firstStage(BSRBR, "first stage.txt")
# samples_SCQM <- firstStage(SCQM, "first stage.txt")
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI.RData")
# save(samples_SCQM, file = "SCQM-ApproachI.RData")

setwd("C:/Users/mike/Desktop/RData")
load("REFLEX-ApproachI.RData")
load("TOWARD-ApproachI.RData")
load("BSRBR-ApproachI.RData")
load("SCQM-ApproachI.Rdata")

setwd("C:/Users/mike/Desktop/Github/phd/book chapter/book chapter part3")

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)
r1$y <- r1$y[11:30] #take out intercept and main effects
r1$Sigma <- r1$Sigma[11:30, 11:30]

r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)
r2$y <- r2$y[11:30] 
r2$Sigma <- r2$Sigma[11:30, 11:30]

r3 <- summarize_each_study(samples_REFLEX)
r3 <- unstandardize_coefficients(r3, REFLEX)
r3$y <- r3$y[11:20] 
r3$Sigma <- r3$Sigma[11:20, 11:20]

r4 <- summarize_each_study(samples_TOWARD)
r4 <- unstandardize_coefficients(r4, TOWARD)
r4$y <- r4$y[11:20] 
r4$Sigma <- r4$Sigma[11:20, 11:20]

y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage.txt")
summary(result)


# with shrinkage two-stage analysis - load datasets
# samples_REFLEX <- firstStage(REFLEX, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_TOWARD <- firstStage(TOWARD, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_BSRBR <- firstStage(BSRBR, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_SCQM <- firstStage(SCQM, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI-bayesLASSO.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI-bayesLASSO.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI-bayesLASSO.RData")
# save(samples_SCQM, file = "SCQM-ApproachI-bayesLASSO.RData")

# second stage analysis
setwd("C:/Users/mike/Desktop/RData")
load("REFLEX-ApproachI-bayesLASSO.RData")
load("TOWARD-ApproachI-bayesLASSO.RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")

setwd("C:/Users/mike/Desktop/Github/phd/book chapter/book chapter part3")

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)
r1$y <- r1$y[11:30] #take out intercept and main effects
r1$Sigma <- r1$Sigma[11:30, 11:30]

r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)
r2$y <- r2$y[11:30] 
r2$Sigma <- r2$Sigma[11:30, 11:30]

r3 <- summarize_each_study(samples_REFLEX)
r3 <- unstandardize_coefficients(r3, REFLEX)
r3$y <- r3$y[11:20] 
r3$Sigma <- r3$Sigma[11:20, 11:20]

r4 <- summarize_each_study(samples_TOWARD)
r4 <- unstandardize_coefficients(r4, TOWARD)
r4$y <- r4$y[11:20] 
r4$Sigma <- r4$Sigma[11:20, 11:20]

y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage.txt")
summary(result)


# with shrinkage and NRS down-weighting two-stage analysis - load datasets
# samples_REFLEX <- firstStage(REFLEX, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_TOWARD <- firstStage(TOWARD, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_BSRBR <- firstStage(BSRBR, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# samples_SCQM <- firstStage(SCQM, "first stage-bayesLASSO.txt", index = c("a", "b", "c", "d", "sigma", "sdGamma"))
# 
# save(samples_REFLEX, file = "REFLEX-ApproachI-bayesLASSO.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI-bayesLASSO.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI-bayesLASSO.RData")
# save(samples_SCQM, file = "SCQM-ApproachI-bayesLASSO.RData")

# second stage analysis
setwd("C:/Users/mike/Desktop/RData")
load("REFLEX-ApproachI-bayesLASSO.RData")
load("TOWARD-ApproachI-bayesLASSO.RData")
load("BSRBR-ApproachI-bayesLASSO.RData")
load("SCQM-ApproachI-bayesLASSO.Rdata")

setwd("C:/Users/mike/Desktop/Github/phd/book chapter/book chapter part3")

#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)
r1$y <- r1$y[11:30] #take out intercept and main effects
r1$Sigma <- r1$Sigma[11:30, 11:30]

r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)
r2$y <- r2$y[11:30] 
r2$Sigma <- r2$Sigma[11:30, 11:30]

r3 <- summarize_each_study(samples_REFLEX)
r3 <- unstandardize_coefficients(r3, REFLEX)
r3$y <- r3$y[11:20] 
r3$Sigma <- r3$Sigma[11:20, 11:20]

r4 <- summarize_each_study(samples_TOWARD)
r4 <- unstandardize_coefficients(r4, TOWARD)
r4$y <- r4$y[11:20] 
r4$Sigma <- r4$Sigma[11:20, 11:20]

y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]])

W50 <- matrix(0.5, nrow = 20, ncol = 20)
W25 <- matrix(0.25, nrow = 20, ncol = 20)

result <- secondStage(y = y, Sigma = Sigma, W = W25, jags_file = "second stage weighted.txt")
#result <- secondStage(y = y, Sigma = Sigma, W = W50, jags_file = "second stage weighted.txt")
summary(result)