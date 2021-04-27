library(dplyr)
library(mvtnorm)
library(mice) #pmm
library(miceadds) #2l.pmm
library(micemd) #2l.2stage.norm
library(mitools)
library(lme4)

setwd("~/GitHub/phd/missing")
source("helpful.functions.R")

####################################
simulated_data <- generate_data()

# Some notes:
# Missing data imputation (partially) following Chapter 7 in the book https://stefvanbuuren.name/fimd/sec-mlguidelines.html 
#2l.lmer Jolani (2018) works, but is too slow
#2l.pmm uses homoscedastic assumption
#2l.2stage.pmm does not work; gives error
#2l.2stage.norm implements the two-stage method by Resche-Rigon and White (2018)


# Approach 1: naive analysis
naivepred <- findPrediction(simulated_data, method = "naive")
save(naivepred, file = "naivepred.RData")
load("naivepred.RData")


# average predictions
averagepred <- findPrediction(simulated_data, method = "average_predictions")
save(averagepred, file = "averagepred.RData")
load("averagepred.RData")

lmerperf1 <- findPerformance(simulated_data, averagepred)



#simulated_data <- simulated_data[complete.cases(simulated_data),]
with(data = simulated_data, lmer(y ~ (x1 +x2 +x3 +x4)*treat + (1|study) + (0 + treat|study)))
#simulated_data <- simulated_data %>% mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)

training_set <- simulated_data %>% mutate(x1.treat = x1*treat, x2.treat = x2*treat, x3.treat = x3*treat, x4.treat = x4*treat)

meth <- make.method(training_set)
#meth[c("x1","x2","x3","x4")] <-"2l.pmm"
meth[c("x1","x2","x3","x4", "x1.treat", "x2.treat", "x3.treat", "x4.treat")] <-"2l.2stage.norm"

pred <- make.predictorMatrix(training_set)
pred[,] <- 0
pred[, "study"] <- -2

#codes <- c(4, 4, 4, rep(1, 3))
codes <- c(1, 1, 1, 1, 2,  rep(1, 3))
pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes
pred["x1.treat", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
pred["x2.treat", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
pred["x3.treat", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
pred["x4.treat", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes

dataa <- as.data.frame(training_set)
imp <- mice(dataa, predictorMatrix = pred, method = meth)



training_set <- simulated_data %>% mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)

meth <- make.method(training_set)
#meth[c("x1","x2","x3","x4")] <-"2l.norm"
meth[c("x1","x2","x3","x4")] <-"2l.pmm"
#meth[c("x1","x2","x3","x4")] <-"2l.2stage.pmm"
#meth[c("x1","x2","x3","x4")] <-"2l.2stage.norm"
#meth[c("x1","x2","x3","x4")] <-"2l.lmer"



pred <- make.predictorMatrix(training_set)
pred[,] <- 0
pred[, "study"] <- -2

codes <- c(1, 1, 1, 1, 2, rep(1, 3))
pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes

# derive interactions
meth["x1.treat"] <- "~ I(x1 * treat)"
meth["x2.treat"] <- "~ I(x2 * treat)"
meth["x3.treat"] <- "~ I(x3 * treat)"
meth["x4.treat"] <- "~ I(x4 * treat)"

imp <- mice(training_set, predictorMatrix = pred, method = meth)
impc <- complete(imp, "long", include = "TRUE")
fit <- with(imp, lmer(y ~ (x1 +x2 +x3 +x4)*treat + (1|study) + (0 + treat|study)))
#fit <- with(imp, lmer(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat + x3*treat + x4*treat + (1| study)))
#t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))

library(jomo)
options(na.action = "na.pass")
bb <- model.matrix(y ~ (x1 + x2 + x3 + x4)*treat, simulated_data)
colnames(bb)[7:10] <- c("x1treat", "x2treat", "x3treat", "x4treat")
bb <- cbind(bb, y = simulated_data$y)

y_imputation <- bb[,c("x1", "x1treat", "x2", "x2treat", "x3", "x3treat", "x4", "x4treat")]
#y_imputation <- bb[,c("x1", "x2", "x3", "x4")]
X_imputation <- bb[,c("(Intercept)", "treat", "y")]

clus <- simulated_data$study
Z <- bb[,"treat", drop = FALSE]

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "common", nburn = 100, nbetween = 100)

#beta.start <- matrix(0, 3, 8)
#l1cov.start <- diag(1, 8)
#l1cov.prior <- diag(2, 8)
imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, Z = Z, meth = "random", nburn = 100, nbetween = 100)


#save(imp, file = "imprandom.RData")
#save(imp, file = "impcommon.RData")
#load("impcommon.RData")

imp.list <- imputationList(split(imp, imp$Imputation)[-1])
fit.imp <- with(data = imp.list, lmer(y ~ (x1 +x2 +x3 +x4)*treat + (1|clus)))
coefs <- MIextract(fit.imp, fun = fixef)
vars <- MIextract(fit.imp, fun = function(x) diag(vcov(x)))
results <- MIcombine(coefs, vars)
summary(results)


lmer_model <- lmer(y ~ (x1 + x3 + x4) *treat + (1|study), data = simulated_data, na.action = na.omit)


# meth <- make.method(simulated_data)
# meth[c("x1", "x3", "x4")] <-"2l.pmm"

pred <- make.predictorMatrix(simulated_data)
pred[,] <- 0
pred[, "study"] <- -2

codes <- c(1, 1, 1, 1, 1)
pred["x1", c("y", "x2", "x3", "x4", "treat")] <- codes
pred["x2", c("y", "x1", "x3", "x4", "treat")] <- codes
pred["x3", c("y", "x1", "x2", "x4", "treat")] <- codes
pred["x4", c("y", "x1", "x2", "x3", "treat")] <- codes



#codes <- c(4, 4, 4, rep(1, 3))
codes <- c(1, 1, 1, 1, rep(1, 4))
pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes

# derive interactions
# meth["x1.treat"] <- "~ I(x1 * treat)"
# meth["x2.treat"] <- "~ I(x2 * treat)"
# meth["x3.treat"] <- "~ I(x3 * treat)"
# meth["x4.treat"] <- "~ I(x4 * treat)"

simulated_data <- simulated_data %>% select(y, x1, x2, study)

imp0 <- mice(simulated_data, print = F, maxit = 0)
pred <- imp0$pred

pred[pred[,"study"] != 0, "study"] <- -2
#pred[pred == 1] <- 2

imp <- mice(simulated_data, pred = pred, meth = "2l.cont")

