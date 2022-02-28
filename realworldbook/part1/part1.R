# load packages needed
library(readstata13) #reading stat data file
library(lme4) #for fitting glmm

## The github library ("bipd") contains functions for generating sample data and running Bayesian IPD-MA methods.
library(devtools)
#install.packages("bipd")
#or install by devtools::install_github("MikeJSeo/bipd")
library(bipd) 

setwd("C:/Users/mike/Desktop")
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


########### Bayesian LASSO (ie Laplacian shrinkage)
ipd <- with(mydata, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = X, response = "binomial", shrinkage = "laplace", lambda.prior = list("dgamma",2,0.1)))

samples <- ipd.run.parallel(ipd, pars.save = c("lambda", "beta", "gamma", "delta", "sd"))

treatment.effect(ipd, samples, newpatient = c(80, 0, 1, 0, 1, 1, 1, 0, 5))
treatment.effect(ipd, samples, newpatient = c(50, 1, 0, 1, 0, 0, 0, 1, 1))

########### SSVS
ipd <- with(mydata, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = X, response = "binomial", shrinkage = "SSVS", g = 100))
samples <- ipd.run.parallel(ipd, pars.save = c("Ind", "eta", "beta", "gamma", "delta", "sd"))

samples2 <- samples[,-19] #remove delta[1] which is 0
summary(samples2)
plot(samples2) #traceplot and posterior of parameters
coda::gelman.plot(samples2) #gelman diagnostic plot

treatment.effect(ipd, samples, newpatient = c(80, 0, 1, 0, 1, 1, 1, 0, 5))
treatment.effect(ipd, samples, newpatient = c(50, 1, 0, 1, 0, 0, 0, 1, 1))
