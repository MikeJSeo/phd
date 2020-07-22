# load packages needed
library(readstata13) #reading stat data file
library(lme4) #for fitting glmm
library(glmnet) #for lasso/ridge

## The github library ("bipd") contains functions for generating sample data and running Bayesian IPD-MA methods.
library(devtools)
#devtools::install_github("MikeJSeo/bipd")
library(bipd) 

setwd("C:/Users/ms19g661/Desktop")
stent <- read.dta13("1_year_stent_data_21092018.dta")


######### Data cleaning
# "age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "gpi", "new_p2y12", "num_stent"
# gender=1 is men, gender=0 is female
# dropped gpi, new_p2y12
# dropped study 19: too big
study_inclusion <- c(3, 4, 5, 12, 13, 14, 15, 17)
data0 <- stent[stent$trial_name %in% study_inclusion,]

#rename study
for(i in 1:dim(data0)[1]){
  data0$trial_name[i] <- which(data0$trial_name[i] == study_inclusion)
}
data0$trial_name <- as.factor(data0$trial_name)

X <-  data0[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")]

#recode categorical variables
ii <- sapply(X, is.factor)
X[ii] <- lapply(X[ii], as.character)
X[X == "Yes"] <- 1
X[X == "Male"] <- 1
X[X == "No"] <- 0
X[X == "Female"] <- 0
X[ii] <- lapply(X[ii], as.numeric)

data_stent <- cbind(X, y = data0$a_death_5yr_yn, treat = ifelse(data0$rand == "BMS", 0, 1), studyid = data0$trial_name)
mydata <- na.omit(data_stent)
#mydata2 <- data_stent[!complete.cases(data_stent),]

# X is unscaled covariates
X <- mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")]

# scale all variables
scale_mean <- apply(mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")], 2, mean)
scale_sd <- apply(mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")], 2, sd)
mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")] <- apply(mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")], 2, scale)

expit <- function(x){
  exp(x)/(1+exp(x))
}

### two patient subgroups
# 80 years: 0.9325098, female(gender = 0): -1.6474649, diabetes=1: 1.8095668, stable_cad=0: -0.7168387, multivessel=1: 1.0778950, ladtreated = 1: 1.0691515, overlap=1: 2.348039, m_dia_above_3=0: -5.3789130, num_stents = 5 (3.1705418)
group1 <- (c(80, 0, 1, 0, 1, 1, 1, 0, 5) - scale_mean) / scale_sd

# 50 years: -1.522422, male(gender =1): 0.606939, diabetes=0: -0.5525687, stable_cad=1: 1.3948883, multivessel=0: -0.9276506, ladtreated=0: -0.9352369, overlap=0: -0.425849, m_dia_above_3=1: 0.1858944, num_stents = 1: -0.6300019
group2 <- (c(50, 1, 0, 1, 0, 0, 0, 1, 1) - scale_mean) / scale_sd



#####################################################
############ Different models########################

############## glmm_full
m2 <- glmer(y ~ studyid + (age + gender + diabetes + stable_cad + multivessel + ladtreated + overlap + m_dia_above_3 + num_stent)*treat + (-1 + treat|studyid), data = mydata, family = binomial)
summary(m2)

# group 1
contr <- c(rep(0, 8), rep(0, 9), 1, group1)
v1 <- vcov(m2)
se1 <- c(sqrt(contr %*% v1 %*% contr))
mean1 <- c(contr %*% summary(m2)$coefficients[,"Estimate"])
exp(mean1 + qnorm(c(.025,0.5,.975))* as.vector(se1[[1]]))

# group 2
contr <- c(rep(0, 8), rep(0, 9), 1, group2)
v2 <- vcov(m2)
se2 <- c(sqrt(contr %*% v2 %*% contr))
mean2 <- c(contr %*% summary(m2)$coefficients[,"Estimate"])
exp(mean2 + qnorm(c(.025,0.5,.975))* as.vector(se2[[1]]))


## naive step
m3 <- glm(y ~ studyid + (age + gender + diabetes + stable_cad + multivessel + ladtreated + overlap + m_dia_above_3 + num_stent)*treat, family = binomial(link = "logit"), data = mydata)
s1 <- step(m3, scope=list(lower = ~ studyid + age + gender + diabetes + stable_cad + multivessel + ladtreated + overlap + m_dia_above_3 + num_stent + treat), direction = "both")
summary(s1)

# ladtreated = 1: 1.0691515, num_stents = 5: 3.1705418 
contr <- c(rep(0, 8), rep(0, 9), 1, 1.0691515, 3.1705418)
v1 <- vcov(s1)
se1 <- c(sqrt(contr %*% v1 %*% contr))
mean1 <- c(contr %*% coef(s1))
exp(mean1 + qnorm(c(.025,0.5,.975))* as.vector(se1[[1]]))

# ladtreated=0: -0.9352369, num_stents = 1: -0.6300019
contr <- c(rep(0, 8), rep(0, 9), 1, -0.9352369, -0.6300019)
v2 <- vcov(s1)
se2 <- c(sqrt(contr %*% v2 %*% contr))
mean2 <-  c(contr %*% coef(s1))
exp(mean2 + qnorm(c(.025,0.5,.975))* as.vector(se2[[1]]))


## LASSO
set.seed(1)
data_glmnet <- model.matrix(y~ studyid + (age + gender + diabetes + stable_cad + multivessel + ladtreated + overlap + m_dia_above_3 + num_stent)*treat, data = mydata)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = mydata$y, data_glmnet = data_glmnet)

p.fac.stent <- c(rep(0, 7), rep(0, 9), 0, rep(1,9)) #No shrinkage for treatment effect and baseline effect
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.stent, family = "binomial", type.measure = "deviance", standardize = FALSE)
coef(cvfit, s = "lambda.min")

#group1
contr <- c(rep(0, 8), rep(0, 9), 1, group1)
exp(contr %*% coef(cvfit, s = "lambda.min"))

#group2
contr <- c(rep(0, 8), rep(0, 9), 1, group2)
exp(contr %*% coef(cvfit, s = "lambda.min"))

# function calculating confidence interval of the estimates
bootstrap_function  <- function(model_data, ndraws, p.fac, family, alpha = 1, col_labels, lambdas = NULL) {
  
  coeff_mtx <- matrix(0, nrow = ndraws, ncol = length(col_labels))
  
  for (ii in 1:ndraws) {
    
    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    if(alpha == 0 & family == "binomial"){
      bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha, lambda = lambdas)  
    }else {
      bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha)  
    }
    aa <- coef(bootstrap_model, s = "lambda.min")
    coeff_mtx[ii,] <- sapply(col_labels, function(x) ifelse(x %in% rownames(aa)[aa[,1] != 0], aa[x,1], 0))  
  }
  se <- apply(coeff_mtx, 2, sd, na.rm = TRUE)
  return(se)
}


# function for calculating standard deviation of the predictions
bootstrap_function2  <- function(model_data, ndraws, p.fac, family, alpha = 1, contr_vec, lambdas = NULL) {
  
  predictions <- rep(NA, ndraws)
  
  for (ii in 1:ndraws) {
    
    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    if(alpha == 0 & family == "binomial"){
      bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha, lambda = lambdas)  
    }else {
      bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha)  
    }
    predictions[ii] <- exp(contr_vec %*% coef(bootstrap_model, s = "lambda.min"))
  }
  return(predictions)
}


col_labels <- c("age", "gender", "diabetes", "stable_cad", "multivessel", "ladtreated", "overlap", "m_dia_above_3", "num_stent", "age:treat", "gender:treat", "diabetes:treat", "stable_cad:treat", "multivessel:treat", "ladtreated:treat", "overlap:treat", "m_dia_above_3:treat", "num_stent:treat", "treat")
set.seed(1)
sd_values <- bootstrap_function(model_data = data_glmnet, ndraws = 50, p.fac = p.fac.stent, family = "binomial", alpha = 1, col_labels)
sd_values


set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, 0.9325098, -1.6474649, 1.8095668, -0.7168387, 1.0778950, 1.0691515, 2.348039, -5.3789130, 3.1705418)
predictions_group1 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent, family = "binomial", alpha = 1, contr_vec = contr)
quantile(predictions_group1, probs = c(0.025, 0.5, 0.975))

set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, -1.522422, 0.606939, -0.5525687, 1.3948883, -0.9276506, -0.9352369, -0.425849, 0.1858944, -0.6300019)
predictions_group2 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent, family = "binomial", alpha = 1, contr_vec = contr)
quantile(predictions_group2, probs = c(0.025, 0.5, 0.975))


## RIDGE
set.seed(1)
lambdas <- 10^seq(3, -3, by = -.1) #manually specify lambda value to cross validate
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.stent, family = "binomial", type.measure = "deviance", standardize = FALSE, alpha = 0, lambda = lambdas)
coef(cvfit, s = "lambda.min")
best_ridge_coef <- as.numeric(coef(cvfit, s = cvfit$lambda.min))[-1]

set.seed(1)
sd_values2 <- bootstrap_function(model_data = data_glmnet, ndraws = 50, p.fac = p.fac.stent, family = "binomial", alpha = 0, col_labels, lambda = lambdas)
sd_values2

set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, group1)
predictions_group1 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent, family = "binomial", alpha = 0, contr_vec = contr, lambda = lambdas)
quantile(predictions_group1, probs = c(0.025, 0.5, 0.975))

set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, group2)
predictions_group2 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent, family = "binomial", alpha = 0, contr_vec = contr, lambda = lambdas)
quantile(predictions_group2, probs = c(0.025, 0.5, 0.975))


###### adaptive LASSO
glmmresult <- summary(m2)$coefficients[,"Estimate"][-1]
p.fac.stent2 <- p.fac.stent/ abs(glmmresult)

set.seed(1)
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.stent2, family = "binomial", type.measure = "deviance", standardize = FALSE, alpha = 1)
coef(cvfit, s = "lambda.min")

set.seed(1)
col_labels <- c("age", "gender", "diabetes", "stable_cad", "multivessel", "ladtreated", "overlap", "m_dia_above_3", "num_stent", "age:treat", "gender:treat", "diabetes:treat", "stable_cad:treat", "multivessel:treat", "ladtreated:treat", "overlap:treat", "m_dia_above_3:treat", "num_stent:treat", "treat")
sd_values2 <- bootstrap_function(model_data = data_glmnet, ndraws = 50, p.fac = p.fac.stent2, family = "binomial", alpha = 1, col_labels)
sd_values2

set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, group1)
predictions_group1 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent2, family = "binomial", alpha = 1, contr_vec = contr)
quantile(predictions_group1, probs = c(0.025, 0.5, 0.975))

set.seed(1)
contr <- c(rep(0, 8), rep(0, 9), 1, group2)
predictions_group2 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.stent2, family = "binomial", alpha = 1, contr_vec = contr)
quantile(predictions_group2, probs = c(0.025, 0.5, 0.975))



########### Bayesian LASSO (ie Laplacian shrinkage)
ipd <- with(mydata, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = X, response = "binomial", shrinkage = "laplace", lambda.prior = list("dgamma",2,0.1)))

samples <- ipd.run.parallel(ipd, pars.save = c("lambda", "beta", "gamma", "delta"))

treatment.effect(ipd, samples, newpatient = c(80, 0, 1, 0, 1, 1, 1, 0, 5), response = "binomial")
treatment.effect(ipd, samples, newpatient = c(50, 1, 0, 1, 0, 0, 0, 1, 1), response = "binomial")


########### SSVS
ipd <- with(mydata, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = X, response = "binomial", shrinkage = "SSVS", g = 100))
samples <- ipd.run.parallel(ipd, pars.save = c("Ind", "eta", "beta", "gamma", "delta"))

samples2 <- samples[,-19] #remove delta[1] which is 0
summary(samples2)
plot(samples2) #traceplot and posterior of parameters
coda::gelman.plot(samples2) #gelman diagnostic plot

treatment.effect(ipd, samples, newpatient = c(80, 0, 1, 0, 1, 1, 1, 0, 5), response = "binomial")
treatment.effect(ipd, samples, newpatient = c(50, 1, 0, 1, 0, 0, 0, 1, 1), response = "binomial")

