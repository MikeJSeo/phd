set.seed(1)
N <- 1000 #number of patients per trial
N.trials <- 6 #number of trials
alpha <- c(11, 8, 10.5, 9.6, 12.9, 15.8) #study effects
delta <- c(-2.95, -2.97, -2.89, -2.91, -2.93, -2.90) #treatment effects
beta1 <- c(0.24, 0.21, 0.20, 0.18, 0.25, 0.22) #prognostic effects of z1
gamma1 <- c(-0.9, -0.5, -0.6, -0.7, -0.1, -0.3) #interaction effects of z1
beta2 <- c(0.15, 0.21, 0.30, 0.38, 0.45, 0.42) #prognostic effects of z2
gamma2 <- c(0.9, 0.5, 0.5, 0.7, 0.1, 0.5) #interaction effects of z2


studyid <- c(1:6)
ds <- as.data.frame(array(NA, dim=c(N*6, 5)))
colnames(ds) <- c("studyid", "treat", "z1","z2", "y")

for (i in 1:N.trials) {
  treat <- rbinom(N,1,0.5)
  z1 <- rnorm(N, mean=0, sd=1) # standardize all of the covariates to make them have a similar scale
  z2 <- rnorm(N, mean=0, sd=1)
  y <- round(alpha[i] + delta[i]*treat + beta1[i]*z1 + beta2[i]*z2 + gamma1[i]*z1*treat + gamma2[i]*z2*treat)
  ds[(((i-1)*N)+1):(i*N), ] <- cbind(studyid[i], treat, z1,z2, y)
}

ds$studyid <- as.factor(ds$studyid)
head(ds)


## GLMM
library(lme4) #for fitting glmm
m1 <- lmer(y ~ studyid + (z1+z2)*treat + (-1 + treat|studyid), data = ds)
summary(m1)

## STEP

m2 <- glm(y ~ studyid + (z1+z2)*treat, data = ds)
s2 <- step(m2, scope=list(lower = ~ z1+z2+treat), direction = "both")
summary(s2)

## LASSO
library(glmnet)
data_glmnet <- model.matrix(y~ studyid + (z1+z2)*treat, data = ds)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = ds$y, data_glmnet = data_glmnet)
p.fac.stent <- c(rep(0, 6), rep(0, 2), 0, rep(1,2)) #No shrinkage for treatment effect and baseline effect
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.stent, family = "gaussian", type.measure = "deviance")
coef(cvfit, s = "lambda.min")

## RIDGE
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.stent, family = "gaussian", type.measure = "deviance", alpha = 0)
coef(cvfit, s = "lambda.min")

## BAYES-LASSO
library(R2jags)# for Bayesian models

X <- cbind(ds$z1, ds$z2)
data_jags <- with(ds, {
  list(Nstudies = length(unique(studyid)),
       Ncovariate = 2,
       X = X,
       Np = dim(X)[1],
       studyid = studyid,
       treat = treat + 1,
       y = y
)})


modelBayesLasso <- function(){
  
  ########## IPD-MA model
  for (i in 1:Np){
    y[i] ~ dnorm(mu[i], sigma)  
    mu[i]<-a[studyid[i]] + inprod(beta[], X[i,]) +
      (1 - equals(treat[i],1)) * inprod(gamma[], X[i,]) + d[studyid[i],treat[i]] 
  }
  sigma ~ dgamma(0.001, 0.001)
  
  #####treatment effect
  for(j in 1:Nstudies){
    d[j,1] <- 0
    d[j,2] ~ dnorm(delta[2], tau)
  }
  
  ## prior distribution for heterogeneity of the treatment effect
  tau <- pow(sd, -2)
  sd ~ dnorm(0,1);T(0,)
  
  ## prior distribution for average treatment effect
  delta[1] <- 0
  delta[2] ~ dnorm(0, 0.001)
  
  ## prior distribution for baseline effect
  for (j in 1:Nstudies){
    a[j] ~ dnorm(0, 0.001)	
  }
  
  tt <- lambda * sigma  # uses conditional prior
  lambda ~ dgamma(2, 0.1) # this diffuse prior has mean 20, variance 200 
  
  for(k in 1:Ncovariate){
    beta[k] ~ dnorm(0, 0.001)
  }
  
  for(k in 1:Ncovariate){
    gamma[k] ~ ddexp(0, tt) # penalization is only on gamma
  }
}

model_bayesLASSO<- jags(data = data_jags, inits = NULL,
                        parameters.to.save =  c("gamma", "delta", "beta", "sd"),
                        n.chains = 3, n.iter = 10000,
                        n.burnin = 1000,DIC=F,
                        model.file = modelBayesLasso)
model_bayesLASSO



## SSVS

modelSSVS <- function(){
  
  ########## IPD-MA model
  for (i in 1:Np){
    y[i] ~ dnorm(mu[i], sigma)  
    mu[i]<- a[studyid[i]] + inprod(beta[], X[i,]) +
      (1 - equals(treat[i],1)) * inprod(gamma[], X[i,]) + d[studyid[i],treat[i]] 
  }
  sigma ~ dgamma(0.001, 0.001)
  
  #####treatment effect
  for(j in 1:Nstudies){
    d[j,1] <- 0
    d[j,2] ~ dnorm(delta[2], tau)
  }
  
  ## prior distribution for heterogeneity of the treatment effect
  tau <- pow(sd, -2)
  sd ~ dnorm(0,1);T(0,)
  
  ## prior distribution for average treatment effect
  delta[1] <- 0
  delta[2] ~ dnorm(0, 0.001)
  
  ## prior distribution for baseline effect
  for (j in 1:Nstudies){
    a[j] ~ dnorm(0, 0.001)	
  }
  
  for(k in 1:Ncovariate){
    beta[k] ~ dnorm(0, 0.001)
  }
  
  for(k in 1:Ncovariate){
    IndA[k] ~ dcat(Pind[])
    Ind[k] <- IndA[k] - 1
    gamma[k] ~ dnorm(0, tauCov[IndA[k]])
  }
  
  zeta <- pow(eta, -2)
  eta ~ dunif(0, 5)
  tauCov[1] <- zeta
  tauCov[2] <- zeta * 0.01  # g = 100
  
  Pind[1] <- 0.5 #P(I_j=1)= 0.5
  Pind[2] <- 0.5 
}

model_SSVS <- jags(data = data_jags, inits = NULL,
                        parameters.to.save =  c("gamma", "delta", "beta", "sd"),
                        n.chains = 3, n.iter = 10000,
                        n.burnin = 1000,DIC=F,
                        model.file = modelSSVS)
model_SSVS

