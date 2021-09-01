setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")

library(readxl)
library(writexl)
library(tidyverse)
library(mice)
library(rjags)

setwd("C:/Users/ms19g661/Desktop")

library(twang)
#library(survey)

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

# save(samples_REFLEX, file = "REFLEX-ApproachI.RData")
# save(samples_TOWARD, file = "TOWARD-ApproachI.RData")
# save(samples_BSRBR, file = "BSRBR-ApproachI.RData")
# save(samples_SCQM, file = "SCQM-ApproachI.RData")

# samples_BSRBR <- firstStage.propensity(BSRBR, "first stage-propensity.txt")
# samples_SCQM <- firstStage.propensity(SCQM, "first stage-propensity.txt")
# 
# save(samples_BSRBR, file = "BSRBR-ApproachIV-propensity.RData")
# save(samples_SCQM, file = "SCQM-ApproachIV-propensity.RData")



# second stage analysis - load datasets
setwd("C:/Users/ms19g661/Desktop/RData")
load("REFLEX-ApproachI.RData")
load("TOWARD-ApproachI.RData")
load("BSRBR-ApproachIV-propensity.RData")
load("SCQM-ApproachIV-propensity.Rdata")

y_TOWARD2 <- c(4.9562, 0.1472, -0.1151, 0.0202, 0.0608, 0.1905, 0.0166,
               0.0774, 0.1813, 0.4509, -0.0800, 0.1921, -0.0379, -0.1439,
               -0.1313, -0.1356, 0.0382, -0.0359, -0.0117, -1.6994)
Omega_TOWARD2 <- as.matrix(read_excel("Omega_TOWARD2.xlsx", col_names = FALSE))
X_mean <- c(0.83, 52.33, 9.10, 27.74, 0.82, 1.59, 1.53, 46.39, 6.54)
X_sd <- c(0.38, 12.11, 8.18, 6.44, 0.38, 1.46, 0.61, 24.74, 0.96)
# S <- solve(r1[[2]])
# R <- cov2cor(S)
# #Check if Omega_TOWARD2 typed correctly
# aa <- matrix(NA, nrow = 20, ncol = 20)
# for(i in 1:20){
#   for(j in 1:20){
#     aa[i,j] <- Omega_TOWARD2[i,j] == Omega_TOWARD2[j,i]
#   }
# }
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files") #set the location to where JAGS file exists

################################################################################



######## first stage model
firstStage.propensity <- function(study_data, jags_file, mm = 20, index = c("a", "b", "c", "d", "sigma"), scale = TRUE, propensity.score = FALSE){
  
  y <- study_data$DAS28
  treat <- factor(study_data$treat, level = c(1,2,3))
  X <- as.matrix(study_data[,c(-1,-2,-3)])
  if(scale == TRUE){
    X <- apply(X, 2, scale)  
  }
  ncov <- dim(X)[2]
  options(na.action='na.pass')
  X.ext <- model.matrix(y~ -1 + X + treat + X:treat)
  X.ext <- X.ext[,colnames(X.ext) != "treat1"]
  colnames(X.ext) <- gsub("X","",colnames(X.ext))
  X.ext <- cbind(y, X.ext)
  X.ext[,grepl(":", colnames(X.ext))] <- NA
  ini <- mice(X.ext, max = 0, print = FALSE)
  meth <- ini$meth
  
  namess <- colnames(X.ext)[grep(":", colnames(X.ext))]
  meth[grep(":", colnames(X.ext))] <- paste0("~I(", gsub(":", "*", namess),")")
  
  pred<- ini$pred
  XX <- mice(X.ext, meth = meth, pred = pred, maxit = 10, m = mm)

  if(length(unique(study_data$treat)) == 2){
    study_data$treat[study_data$treat == "3"] <- "2"
  }
  
  completedData <- complete(XX, 1)
  completedData$treat <- as.factor(study_data$treat)

  mnps.RA <- mnps(treat ~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS,
                  data = completedData,
                  estimand = "ATE",
                  verbose = FALSE,
                  stop.method = c("es.mean", "ks.mean"),
                  n.trees = 3000)
  weights <- get.weights(mnps.RA, stop.method = "es.mean")
  
  jags_data <- list(
    N = length(y),
    treat = study_data$treat,
    Ncovariate = dim(X)[2],
    Ntreat = length(unique(study_data$treat)),
    y = y,
    #X = complete(XX, 1)[,-1]
    X = complete(XX, 1)[,2:(ncov+1)],
    weights = weights
  )
  mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
  stats::update(mod, 1000)
  samples <- coda.samples(mod, variable.names = index, n.iter = 10000)
  
  if(mm != 1){
    # add more samples
    for(i in 2:mm){
      
      completedData <- complete(XX, i)
      completedData$treat <- as.factor(study_data$treat)
      
      mnps.RA <- mnps(treat ~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS,
                      data = completedData,
                      estimand = "ATE",
                      verbose = FALSE,
                      stop.method = c("es.mean", "ks.mean"),
                      n.trees = 3000)
      weights <- get.weights(mnps.RA, stop.method = "es.mean")
      
      jags_data <- list(
        N = length(y),
        treat = study_data$treat,
        Ncovariate = dim(X)[2],
        Ntreat = length(unique(study_data$treat)),
        y = y,
        #X = complete(XX, i)[,-1]
        X = complete(XX, i)[,2:(ncov+1)],
        weights = weights
      )
      mod <- jags.model(file = jags_file, data = jags_data, n.chains = 3, n.adapt = 1000)
      stats::update(mod, 1000)
      sample_more <- coda.samples(mod, variable.names = index, n.iter = 10000)
      samples <- add.mcmc(samples, sample_more)
    }
  }  
  return(samples)
} 




#find summary mean and covariance matrix for each study
r1 <- summarize_each_study(samples_BSRBR)
r1 <- unstandardize_coefficients(r1, BSRBR)

r2 <- summarize_each_study(samples_SCQM)
r2 <- unstandardize_coefficients(r2, SCQM)

r3 <- summarize_each_study(samples_REFLEX)
r3 <- unstandardize_coefficients(r3, REFLEX)

r4 <- summarize_each_study(samples_TOWARD)
r4 <- unstandardize_coefficients(r4, TOWARD)

r5 <- list(y = y_TOWARD2, Omega = Omega_TOWARD2)
r5 <- unstandardize_coefficients(r5, X_mean = X_mean, X_sd = X_sd)

y <- list(y1 = r1[[1]], y2 = r2[[1]], y3 = r3[[1]], y4 = r4[[1]], y5 = r5[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r2[[2]], Sigma3 = r3[[2]], Sigma4 = r4[[2]], Sigma5 = r5[[2]])

#internal validation
result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII.txt")
prediction_SCQM_internal <- findPrediction(SCQM, result)
prediction_BSRBR_internal <- findPrediction(BSRBR, result)
performance_SCQM_internal <- findPerformance(prediction_SCQM_internal)
performance_BSRBR_internal <- findPerformance(prediction_BSRBR_internal)
lapply(performance_SCQM_internal, mean)
lapply(performance_BSRBR_internal, mean)
calibration_SCQM_internal <- findPerformance2(prediction_SCQM_internal)
calibration_BSRBR_internal <- findPerformance2(prediction_BSRBR_internal)

#internal-external validation
#all datasets except SCQM
y <- list(y1 = r1[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = r5[[1]])
Sigma <- list(Sigma1 = r1[[2]], Sigma2 = r3[[2]], Sigma3 = r4[[2]], Sigma4 = r5[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII-external.txt")
prediction_SCQM_external <- findPrediction(SCQM, result)
performance_SCQM_external <- findPerformance(prediction_SCQM_external)
lapply(performance_SCQM_external, mean)
calibration_SCQM_external <- findPerformance2(prediction_SCQM_external)

#all datasets except BSRBR
y <- list(y1 = r2[[1]], y2 = r3[[1]], y3 = r4[[1]], y4 = r5[[1]])
Sigma <- list(Sigma1 = r2[[2]], Sigma2 = r3[[2]], Sigma3 = r4[[2]], Sigma4 = r5[[2]])

result <- secondStage(y = y, Sigma = Sigma, jags_file = "second stage-ApproachII-external.txt")
prediction_BSRBR_external <- findPrediction(BSRBR, result)
performance_BSRBR_external <- findPerformance(prediction_BSRBR_external)
lapply(performance_BSRBR_external, mean)
calibration_BSRBR_external <- findPerformance2(prediction_BSRBR_external)

#weighted performance measure
performance_weighted <- mapply(c, performance_SCQM_external, performance_BSRBR_external)
lapply(performance_weighted, mean, na.rm = TRUE)

# Approach4-propensity.result <- list(prediction_BSRBR_internal = prediction_BSRBR_internal, prediction_BSRBR_external = prediction_BSRBR_external,
#                          prediction_SCQM_internal = prediction_SCQM_internal, prediction_SCQM_external = prediction_SCQM_external,
#                          calibration_BSRBR_internal = calibration_BSRBR_internal, calibration_BSRBR_external = calibration_BSRBR_external,
#                          calibration_SCQM_internal = calibration_SCQM_internal, calibration_SCQM_external = calibration_SCQM_external) 
# 
# setwd("~/GitHub/phd/ra/Result")
# save(Approach4-propensity.result, file = "Approach4-propensity.result.RData")

