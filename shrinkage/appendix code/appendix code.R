
## The github library ("bipd") contains functions for generating sample data, calculating treatment effect,
## and running Bayesian IPD-MA methods.
library(devtools)
devtools::install_github("MikeJSeo/bipd")
library(bipd) 

ds <- generate_ipdma_example(type = "continuous")
ds2 <- generate_ipdma_example(type = "binary")

###### GLMM
# continuous outcome
library(lme4) #for fitting glmm
m1 <- lmer(y ~ studyid + (z1+z2)*treat + (-1 + treat|studyid), data = ds)
summary(m1)
treatment.effect(newpatient = c(1,1), type = "continuous",
                 coef = summary(m1)$coefficients[,"Estimate"], cov = vcov(m1),
                 treatment.covariate.names = c("treat", "z1:treat", "z2:treat"))
  
# binary outcome
m2 <- glmer(y ~ studyid + (w1+w2)*treat + (-1 + treat|studyid), data = ds2, family = binomial)
summary(m2)
treatment.effect(newpatient = c(1,1), type = "binary",
                 coef = summary(m2)$coefficients[,"Estimate"], cov = vcov(m2),
                 treatment.covariate.names = c("treat", "w1:treat", "w2:treat"))
              
###### STEP
# continuous outcome
m3 <- glm(y ~ studyid + (z1+z2)*treat, data = ds) #glm model without mixed effects 
s1 <- step(m3, scope=list(lower = ~ z1+z2+treat), direction = "both")
summary(s1)

# binary outcome
m4 <- glm(y ~ studyid + (w1+w2)*treat, family = binomial(link = "logit"), data = ds2)
s2 <- step(m4, scope=list(lower = ~ w1+w2+treat), direction = "both") 
summary(s2)


############# LASSO
# continuous outcome
library(glmnet)
p.fac <- c(rep(0, 5), rep(0, 2), 0, rep(1,2)) # Shrinkage is only on effect modifiers
lambdas <- 10^seq(3, -3, by = -.1) # manually specify lambda value to cross validate

data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "gaussian", type.measure = "deviance", lambda = lambdas)
coef <- as.vector(coef(cvfit, s = "lambda.min"))
names(coef) <- rownames(coef(cvfit, s = "lambda.min"))
# For LASSO and RIDGE, we don't specify covariance matrix.
treatment.effect(newpatient = c(1,1), type = "continuous",
                 coef = coef,
                 treatment.covariate.names = c("treat", "z1:treat", "z2:treat"))


# binary outcome
data_glmnet <- model.matrix(y~ studyid + (w1+w2)*treat, data = ds2)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds2$y, data_glmnet = data_glmnet)
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "binomial", type.measure = "deviance", lambda = lambdas)
coef <- as.vector(coef(cvfit, s = "lambda.min"))
names(coef) <- rownames(coef(cvfit, s = "lambda.min"))
treatment.effect(newpatient = c(1,1), type = "binary",
                 coef = coef,
                 treatment.covariate.names = c("treat", "w1:treat", "w2:treat"))


################ RIDGE
# continuous outcome
data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "gaussian", alpha = 0, type.measure = "deviance", lambda = lambdas)
coef <- as.vector(coef(cvfit, s = "lambda.min"))
names(coef) <- rownames(coef(cvfit, s = "lambda.min"))

# binary outcome
data_glmnet <- model.matrix(y~ studyid + (w1+w2)*treat, data = ds2)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds2$y, data_glmnet = data_glmnet)
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = "binomial", alpha = 0, type.measure = "deviance", lambda = lambdas)
coef <- as.vector(coef(cvfit, s = "lambda.min"))
names(coef) <- rownames(coef(cvfit, s = "lambda.min"))


########### Bayesian LASSO (ie Laplacian shrinkage)
# Variable names are as follows:
#"beta" - coefficients for main effects of the covariates
#"gamma" - coefficients for effect modifiers
#"delta" - average treaetment effect
#"lambda" -  shrinkage parameter

# continuous outcome
ipd <- with(ds, ipd.model(y = y, study = studyid, treat = treat, X = cbind(z1, z2), response = "normal", shrinkage = "laplace"))
##To see the JAGS code used to run the model use the command: cat(ipd$code) 
samples <- ipd.run(ipd, pars.save = c("lambda", "beta", "gamma", "delta"))

samples <- samples[,-3] #remove delta[1] which is 0
summary(samples)
plot(samples) #traceplot and posterior of parameters
coda::gelman.plot(samples) #gelman diagnostic plot

# binary outcome
ipd <- with(ds2, ipd.model(y = y, study = studyid, treat = treat, X = cbind(w1, w2), response = "binomial", shrinkage = "laplace"))
result <- ipd.run(ipd, pars.save = c("lambda", "beta", "gamma", "delta"))
summary(samples)

treatment.effect()


########################## SSVS
# Variable names are as follows:
#"beta" - coefficients for main effects of the covariates
#"gamma" - coefficients for effect modifiers
#"delta" - average treaetment effect
#

model_SSVS <- jags(data = data_jags, inits = NULL,
                        parameters.to.save =  c("gamma", "delta", "beta", "sd", "Ind"),
                        n.chains = 3, n.iter = 10000,
                        n.burnin = 1000,DIC=F,
                        model.file = modelSSVS)

model_SSVS
jagsfit.mcmc3 <- as.mcmc(model_SSVS)
jagsfit.mcmc3 <- jagsfit.mcmc3[,-3] #remove delta[1] which is 0
plot(jagsfit.mcmc3) #traceplot and posterior of parameters
gelman.plot(jagsfit.mcmc3) #gelman diagnostic plot


model_SSVS_binary <- jags(data = data_jags_binary, inits = NULL,
                   parameters.to.save =  c("gamma", "delta", "beta", "sd", "Ind"),
                   n.chains = 3, n.iter = 10000,
                   n.burnin = 1000,DIC=F,
                   model.file = modelSSVS_binary)

model_SSVS_binary
jagsfit.mcmc4 <- as.mcmc(model_SSVS_binary)
jagsfit.mcmc4 <- jagsfit.mcmc4[,-3] #remove delta[1] which is 0
plot(jagsfit.mcmc4) #traceplot and posterior of parameters
gelman.plot(jagsfit.mcmc4) #gelman diagnostic plot

