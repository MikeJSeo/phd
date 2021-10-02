
#The github library ("bipd") contains functions for generating sample data and running Bayesian IPD-MA methods.
#library(devtools)
#devtools::install_github("MikeJSeo/bipd") #parallel package take a while to install
library(bipd) 

##load in data
ds <- generate_ipdma_example(type = "continuous")
ds2 <- generate_ipdma_example(type = "binary")

########### GLMM
#continuous outcome
library(lme4) #for fitting glmm
m1 <- lmer(y ~ studyid + (z1+z2)*treat + (-1 + treat|studyid), data = ds)
summary(m1)

#finding treatment effect
contr <- c(rep(0, 8), 1, 1, 0.5) #if covariates are standardized this needs to be modified as well
v1 <- vcov(m1)
se1 <- c(sqrt(contr %*% v1 %*% contr))
mean1 <- c(contr %*% summary(m1)$coefficients[,"Estimate"])
mean1 + qnorm(c(.025,0.5,.975))* as.vector(se1[[1]])

#binary outcome
m2 <- glmer(y ~ studyid + (w1+w2)*treat + (-1 + treat|studyid), data = ds2, family = binomial)
summary(m2)

#finding treatment effect
contr <- c(rep(0, 8), 1, 1, 0.5) #if covariates are standardized this needs to be modified as well
v1 <- vcov(m2)
se1 <- c(sqrt(contr %*% v1 %*% contr))
mean1 <- c(contr %*% summary(m2)$coefficients[,"Estimate"])
exp(mean1 + qnorm(c(.025,0.5,.975))* as.vector(se1[[1]])) # calculate odds ratio
              

########### STEP
#continuous outcome
m3 <- glm(y ~ studyid + (z1+z2)*treat, data = ds) #glm model without mixed effects 
s1 <- step(m3, scope=list(lower = ~ z1+z2+treat), direction = "both")
summary(s1)

#binary outcome
m4 <- glm(y ~ studyid + (w1+w2)*treat, family = binomial(link = "logit"), data = ds2)
s2 <- step(m4, scope=list(lower = ~ w1+w2+treat), direction = "both") 
summary(s2)


############# LASSO
#continuous outcome
library(glmnet)
p.fac <- c(rep(0, 5), rep(0, 2), 0, rep(1,2)) # Shrinkage is only on effect modifiers
lambdas <- 10^seq(3, -3, by = -.1) # manually specify lambda value to cross validate

data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "gaussian", type.measure = "deviance", lambda = lambdas)
coef(cvfit, s = "lambda.min")

#binary outcome
data_glmnet <- model.matrix(y~ studyid + (w1+w2)*treat, data = ds2)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds2$y, data_glmnet = data_glmnet)
cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "binomial", type.measure = "deviance", lambda = lambdas)
coef(cvfit, s = "lambda.min")

################ RIDGE
# continuous outcome
data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
cvfit.ridge = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "gaussian", alpha = 0, type.measure = "deviance", lambda = lambdas)
coef(cvfit.ridge, s = "lambda.min")

# binary outcome
data_glmnet <- model.matrix(y~ studyid + (w1+w2)*treat, data = ds2)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds2$y, data_glmnet = data_glmnet)
cvfit.ridge2 = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "binomial", alpha = 0, type.measure = "deviance", lambda = lambdas)
coef(cvfit.ridge2, s = "lambda.min")


###### adaptive LASSO
# continuous outcome
ridge_result <- coef(cvfit.ridge, s = "lambda.min")[-1]
p.fac2 <- p.fac/ abs(ridge_result)

data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac2, family = "gaussian", type.measure = "deviance", lambda = lambdas)
coef(cvfit, s = "lambda.min")

# binary outcome
ridge_result <- coef(cvfit.ridge2, s = "lambda.min")[-1]
p.fac2 <- p.fac/ abs(ridge_result)

data_glmnet <- model.matrix(y~ studyid + (w1+w2)*treat, data = ds2)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds2$y, data_glmnet = data_glmnet)
cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac2, family = "binomial", type.measure = "deviance", lambda = lambdas)
coef(cvfit, s = "lambda.min")


########### Bayesian LASSO (ie Laplacian shrinkage)
# Stored variable names are as follows:
#"beta" - coefficients for main effects of the covariates
#"gamma" - coefficients for effect modifiers
#"delta" - average treatment effect
#"lambda" -  shrinkage parameter

# continuous outcome
ipd <- with(ds, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "laplace", lambda.prior = list("dgamma",2,0.1)))
##To see the JAGS code used to run the model use the command: 
cat(ipd$code) 
samples <- ipd.run(ipd, pars.save = c("lambda", "beta", "gamma", "delta"), n.chains = 3, n.burnin = 500, n.iter = 5000)

samples <- samples[,-3] #remove delta[1] which is 0
summary(samples)
plot(samples) #traceplot and posterior of parameters
coda::gelman.plot(samples) #gelman diagnostic plot

# can also find treatment effect
treatment.effect(ipd, samples, newpatient = c(1,0.5))

# binary outcome
ipd <- with(ds2, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(w1, w2), response = "binomial", shrinkage = "laplace"))
samples <- ipd.run(ipd, pars.save = c("lambda", "beta", "gamma", "delta"))
summary(samples)

# can also run methods in parallel using dclone package
#library(dclone)
#library(R2WinBUGS)
#samples2 <- ipd.run.parallel(ipd, pars.save = c("lambda", "beta", "gamma", "delta"))
#summary(samples2)

########################## SSVS
# Stored variable names are as follows:
#"beta" - coefficients for main effects of the covariates
#"gamma" - coefficients for effect modifiers
#"delta" - coefficient of average treatment effect
#"Ind" - Indicator for assigning a slab prior (instead of a spike prior) i.e. indicator for including a covariate
#"eta" - Standard deviation of the slab prior

ipd <- with(ds, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "SSVS", hy.prior.eta = list("dunif", 0, 5), g = 1000))
samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta", "Ind", "eta"))

samples <- samples[,-5] #remove delta[1] which is 0
summary(samples)
plot(samples) #traceplot and posterior of parameters
coda::gelman.plot(samples) #gelman diagnostic plot

# binary outcome
ipd <- with(ds2, ipdma.model.onestage(y = y, study = studyid, treat = treat, X = cbind(w1, w2), response = "binomial", shrinkage = "SSVS"))
samples <- ipd.run(ipd, pars.save = c("beta", "gamma", "delta", "Ind", "eta"))
summary(samples)
treatment.effect(ipd, samples, newpatient = c(1,0.5)) # binary outcome reports odds ratio



###########################Aside: not in the paper#######################################
########################## Using "deft" approach without penalization
ipd <- with(ds, ipdma.model.deft.onestage(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal"))
cat(ipd$code)
samples <- ipd.run(ipd, pars.save = c("beta", "gamma.within", "gamma.across", "delta"))
treatment.effect(ipd, samples, newpatient= c(1,0.5), reference = c(0, 0))

ipd <- with(ds2, ipdma.model.deft.onestage(y = y, study = studyid, treat = treat, X = cbind(w1, w2), response = "binomial"))
cat(ipd$code)
samples <- ipd.run(ipd, pars.save = c("beta", "gamma.within", "gamma.across", "delta"))
treatment.effect(ipd, samples, newpatient= c(1,0.5), reference = c(0, 0))