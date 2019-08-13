library(R2jags)
library(meta)

#######################
#this is the data
#######################

#generate data
n_size = 100
m1 <- rnorm(n_size, mean = 4, sd = 2)
m2 <- rnorm(n_size, mean = 3, sd = 1.5)  
sd1 <- runif(n_size, 5, 10)  
sd2 <- runif(n_size, 4, 6)
n1 <- sample(50:70, n_size, replace = TRUE)
n2 <- sample(40:60, n_size, replace = TRUE)

#calculate cohen's d
mean_diff <- m2 - m1
pooled_sd <- sqrt(((n1 -1) * sd1^2 + (n2-1) * sd2^2)/ (n1 + n2 -2))
smd <- mean_diff/pooled_sd
varr <- (n1+n2)/(n1*n2) + smd^2/(2*(n1+n2))
mydata = list(ns=n_size,y= smd, varr = varr)

# Using arm level data
mydata2 = list(ns =n_size, y = cbind(m1, m2)/pooled_sd, varr = cbind(sd1^2/n1, sd2^2/n2)/pooled_sd^2, n = cbind(n1, n2))

# Using arm-level data with pooled.sd separate
mydata3 = list(ns =n_size, y = cbind(m1, m2), w = cbind(n1/sd1^2, n2/sd2^2), pooled.sd=pooled_sd )

# Fit using metacont
pooledSMD1 = metacont(n2, m2, sd2, n1, m1, sd1, studlab = 1:n_size, sm = "SMD")
#######################
#then make the model
#######################

Normal.model <- function(){    
  
  #ns=number of studies
  # y=the ln(OR)
  # var=the variance of the ln(OR)
  #tau.sq=heterogeneity
    
  for (i in 1:ns) {
    ##normal likelihood of observed ln(OR)
    y[i] ~ dnorm(theta[i],w[i])
    w[i] <- 1/varr[i]
    ##distribution of random effects
    
    theta[i] ~ dnorm(mean,prec)
  }

  mean ~ dnorm(0,.0001)
  for (i in 1:ns) {u[i] ~ dnorm(0,.0001)}
    
  ##prior distribution for heterogeneity
  tau ~ dunif(0,100) ##I(0,) truncates the normal above zero
  prec <- 1/tau.sq
  tau.sq <- pow(tau,2)
    
}

Normal.model2 <- function(){    
  
  #ns=number of studies
  # y=the ln(OR)
  # var=the variance of the ln(OR)
  #tau.sq=heterogeneity
  
  for (i in 1:ns) {
 
    y[i,1] ~ dnorm(theta[i,1],w[i,1])
    y[i,2] ~ dnorm(theta[i,2],w[i,2])
    
    w[i,1] <- 1/varr[i,1]
    w[i,2] <- 1/varr[i,2]
    
    theta[i,1]<- u[i]
    theta[i,2]<- u[i] + delta[i]
    
    delta[i] ~ dnorm(mean, prec)
  }

  mean ~ dnorm(0,.0001)
  for (i in 1:ns) {u[i] ~ dnorm(0,.0001)}
  
  ##prior distribution for heterogeneity
  tau ~ dunif(0,100) ##I(0,) truncates the normal above zero
  prec <- 1/tau.sq
  tau.sq <- pow(tau,2)
}

Normal.model3 <- function(){    
  
  #ns=number of studies
  # y=the ln(OR)
  # var=the variance of the ln(OR)
  #tau.sq=heterogeneity
  
  for (i in 1:ns) {
    
    y[i,1] ~ dnorm(theta[i,1],w[i,1])
    y[i,2] ~ dnorm(theta[i,2],w[i,2])
    
    theta[i,1]<- u[i]*pooled.sd[i]
    theta[i,2]<- (u[i] + delta[i])*pooled.sd[i]
    
    delta[i] ~ dnorm(mean, prec)
  }
  
  mean ~ dnorm(0,.0001)
  for (i in 1:ns) {u[i] ~ dnorm(0,.0001)}
  
  ##prior distribution for heterogeneity
  tau ~ dunif(0,100) ##I(0,) truncates the normal above zero
  prec <- 1/tau.sq
  tau.sq <- pow(tau,2)
}


#######################
# initial values
#######################

initialval = NULL
#initialval = list(list(tau=0.2,mean=0.3))

#######################
# run the model
#######################

model <- jags(mydata,initialval,parameters.to.save = c("tau.sq","mean"), n.chains = 2, n.iter = 10000, n.burnin = 1000, DIC=F, model.file = Normal.model)

#for model2
model2 <- jags(mydata2,initialval,parameters.to.save = c("tau.sq", "mean"), n.chains = 2, n.iter = 10000, n.burnin = 1000, DIC=F, model.file = Normal.model2)

#for model3
model3 <- jags(mydata3,initialval,parameters.to.save = c("tau.sq", "mean"), n.chains = 2, n.iter = 10000, n.burnin = 1000, DIC=F, model.file = Normal.model3)

#results
print(model)
print(model2)
print(model3)
summary(pooledSMD1)


#check chain mixing
traceplot(model)
# 
# #what else is there
# names(model)
# names(model$BUGSoutput)
# model$BUGSoutput$summary


#######################
# parallelising
#######################

# start_time <- Sys.time()
# PMAinJAGS<- jags(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 4, n.iter = 100000, n.burnin = 1000, DIC=F, model.file = Normal.model)
# end_time <- Sys.time()
# end_time - start_time
# 
# start_time <- Sys.time()
# PMAinJAGS<- jags.parallel(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 4, n.iter = 100000, n.burnin = 1000, DIC=F, model.file = Normal.model)
# end_time <- Sys.time()
# end_time - start_time




