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
y_TOWARD2 <- c(4.9625, 0.1060, -0.0278, 0.0143, -0.0066, 0.1309, -0.0560, 0.0974, 0.1670, 0.4551,
               -0.0196, 0.0416, -0.0129, -0.0222, -0.0291, -0.0275, -0.0015, -0.0100, -0.0043, -1.6943)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2_bayesLASSO.xlsx", col_names = FALSE))

setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")

###############################################################################

### apparent performance
#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)


###internal validation
y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)
result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII.txt")

result_SCQM <- result
for(i in 1:10){
  result_SCQM[[1]][,i] <- samples_SCQM[[1]][,i]
  result_SCQM[[2]][,i] <- samples_SCQM[[2]][,i]
  result_SCQM[[3]][,i] <- samples_SCQM[[3]][,i]
}

result_BSRBR <- result
for(i in 1:10){
  result_BSRBR[[1]][,i] <- samples_BSRBR[[1]][,i]
  result_BSRBR[[2]][,i] <- samples_BSRBR[[2]][,i]
  result_BSRBR[[3]][,i] <- samples_BSRBR[[3]][,i]
}

prediction_SCQM <- findPrediction(SCQM, result_SCQM)
prediction_BSRBR <- findPrediction(BSRBR, result_BSRBR)
performance_SCQM <- findPerformance(prediction_SCQM)
performance_BSRBR <- findPerformance(prediction_BSRBR)
apparent_performance_SCQM <- unlist(lapply(performance_SCQM, mean))
apparent_performance_BSRBR <- unlist(lapply(performance_BSRBR, mean))

##########################

##### Finding optimism: SCQM
r1 <- summarize_each_study(samples_BSRBR)
#r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)

set.seed(1)
optimism <- matrix(NA, nrow = 200, ncol = 8)
colnames(optimism) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3")
for(ii in 1:200){
  
  SCQM_bootstrap <- SCQM[sample(1:dim(SCQM)[1], replace = TRUE),]
  samples_SCQM_bootstrap <- firstStage(SCQM_bootstrap, "first stage.txt", mm = 1)
  r2 <- summarize_each_study(samples_SCQM_bootstrap)
  
  y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
  Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)
  result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII.txt", n.iter = 10000)
  
  result_SCQM <- result
  for(i in 1:10){
    result_SCQM[[1]][,i] <- samples_SCQM_bootstrap[[1]][,i]
    result_SCQM[[2]][,i] <- samples_SCQM_bootstrap[[2]][,i]
    result_SCQM[[3]][,i] <- samples_SCQM_bootstrap[[3]][,i]
  }
  
  prediction_SCQM_bootstrap <- findPrediction(SCQM_bootstrap, result_SCQM)
  performance_SCQM_bootstrap <- findPerformance(prediction_SCQM_bootstrap)
  
  prediction_SCQM_test <- findPrediction(SCQM, result_SCQM)
  performance_SCQM_test <- findPerformance(prediction_SCQM_test)
  
  optimism[ii,] <- mapply('-',lapply(performance_SCQM_bootstrap, mean),lapply(performance_SCQM_test, mean),SIMPLIFY=TRUE)
}
optimism_averaged <- apply(optimism, 2, mean, na.rm = TRUE)
optimism_corrected_performance_SCQM <- apparent_performance_SCQM - optimism_averaged



##### Finding optimism: BSRBR
#r1 <- summarize_each_study(samples_BSRBR)
r2 <- summarize_each_study(samples_SCQM)
r3 <- summarize_each_study(samples_REFLEX)
r4 <- summarize_each_study(samples_TOWARD)

set.seed(1)
optimism2 <- matrix(NA, nrow = 200, ncol = 8)
colnames(optimism2) <- c("mse", "bias", "mse1", "bias1", "mse2", "bias2", "mse3", "bias3")
for(ii in 1:200){
  
  BSRBR_bootstrap <- BSRBR[sample(1:dim(BSRBR)[1], replace = TRUE),]
  samples_BSRBR_bootstrap <- firstStage(BSRBR_bootstrap, "first stage.txt", mm = 1)
  r1 <- summarize_each_study(samples_BSRBR_bootstrap)
  
  y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = y_TOWARD2)
  Omega <- list(Omega1 = r1[[2]], Omega2 = r2[[2]], Omega3 = r3[[2]], Omega4 = r4[[2]], Omega5 = Omega_TOWARD2)
  result <- secondStage(y = y, Omega = Omega, jags_file = "second stage-ApproachII.txt", n.iter = 10000)
  
  result_BSRBR <- result
  for(i in 1:10){
    result_BSRBR[[1]][,i] <- samples_BSRBR_bootstrap[[1]][,i]
    result_BSRBR[[2]][,i] <- samples_BSRBR_bootstrap[[2]][,i]
    result_BSRBR[[3]][,i] <- samples_BSRBR_bootstrap[[3]][,i]
  }
  
  prediction_BSRBR_bootstrap <- findPrediction(BSRBR_bootstrap, result_BSRBR)
  performance_BSRBR_bootstrap <- findPerformance(prediction_BSRBR_bootstrap)
  
  prediction_BSRBR_test <- findPrediction(BSRBR, result_BSRBR)
  performance_BSRBR_test <- findPerformance(prediction_BSRBR_test)
  
  optimism2[ii,] <- mapply('-',lapply(performance_BSRBR_bootstrap, mean),lapply(performance_BSRBR_test, mean),SIMPLIFY=TRUE)
}
optimism2_averaged <- apply(optimism2, 2, mean, na.rm = TRUE)
optimism_corrected_performance_BSRBR <- apparent_performance_BSRBR - optimism2_averaged
