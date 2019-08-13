# In the antidepressants data (GRISELDA)
# 
# Run NMA for the response outcome using the model that is already there
# Create a random variable called year of randomisation
# Modify the NMA model for dichotomous outcomes to be a meta-regression model
# Run network meta-regression for response versus year of randomisation

#*********************************************************************************
#             Load the libraries needed             
#*********************************************************************************
library(meta)
library(metafor)
library(netmeta)
library(readxl)

library(devtools)
install_github("esm-ispm-unibe-ch/NMAJags")
library(NMAJags)
library(R2jags)
library(RCurl)

url <- "https://raw.githubusercontent.com/MikeJSeo/jags/master/data/Depression.csv"
data <- getURL(url)                
data <- read.csv(textConnection(data), sep = ";", dec = ".")

depression = data[1:361,c("studyID", "drug_name", "Ntotal", "Responders", "study_year")]
depression$drug_name = factor(depression$drug_name)
depression$study_year[is.na(depression$study_year)] = 2002
depression$study_year = depression$study_year - mean(depression$study_year) # try to center the covariate

NMAdataDiscrete=make.jagsNMA.data(studyid=studyID,t=drug_name,r= Responders, n = Ntotal, type = "binary", data= depression, reference = "Agomelatine")
#run Jags and create a jags object
NMAinJAGS<- jags(data = NMAdataDiscrete, inits = NULL,
                 parameters.to.save = c("d","tau"), n.chains = 2, n.iter = 10000,
                 n.burnin = 1000,DIC=F,n.thin=10,
                 model.file = modelNMABinary)
print(NMAinJAGS)


modelNMRBinary <- function(){
  
  for (i in 1:ns) {
    w[i, 1] <- 0
    theta[i, t[i, 1]] <- 0
    for (k in 1:na[i]) {
      r[i, t[i, k]] ~ dbin(p[i, t[i, k]], n[i, t[i, k]])
      ######added this line
      logit(p[i, t[i, k]]) <- u[i] + theta[i, t[i, k]] +  beta[t[i,1], t[i, k]] * variab[i]
    }
    
    for (k in 2:na[i]) {
      theta[i, t[i, k]] ~ dnorm(md[i, t[i, k]], precd[i, 
                                                      t[i, k]])
      md[i, t[i, k]] <- mean[i, k] + sw[i, k]
      w[i, k] <- (theta[i, t[i, k]] - mean[i, k])
      sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      precd[i, t[i, k]] <- prec * 2 * (k - 1)/k
      mean[i, k] <- d[t[i, k]] - d[t[i, 1]]
    }
  }
  for (i in 1:ns) {
    u[i] ~ dnorm(0, 0.01)
  }
  tau ~ dnorm(0, 1) %_% T(0, )
  prec <- 1/pow(tau, 2)
  tau.sq <- pow(tau, 2)
  d[ref] <- 0
  for (k in 1:(ref - 1)) {
    d[k] ~ dnorm(0, 0.01)
  }
  for (k in (ref + 1):nt) {
    d[k] ~ dnorm(0, 0.01)
  }
  for (i in 1:(nt - 1)) {
    for (j in (i + 1):nt) {
      OR[j, i] <- exp(d[j] - d[i])
      LOR[j, i] <- d[j] - d[i]
    }
  }
  for (j in 1:(ref - 1)) {
    ORref[j] <- exp(d[j] - d[ref])
  }
  for (j in (ref + 1):nt) {
    ORref[j] <- exp(d[j] - d[ref])
  }
  
  #added this
  for (i in 1:nt) {
    for (j in 1:nt) {
      beta[i, j] <- b[j] - b[i]
    }
  }
  b[ref] <- 0
  for (k in 1:(ref - 1)) {
    b[k] ~ dnorm(0, 1e-04)
  }
  for (k in (ref + 1):nt) {
    b[k] ~ dnorm(0, 1e-04)
  }
}

NMAdataDiscrete=make.jagsNMA.data(studyid=studyID,t=drug_name,r= Responders, n = Ntotal, type = "binary", data= depression, reference = "Agomelatine", other = study_year)
#run Jags and create a jags object
NMAinJAGS<- jags(data = NMAdataDiscrete, inits = NULL,
                 parameters.to.save = c("d","tau", "b"), n.chains = 2, n.iter = 10000,
                 n.burnin = 1000,DIC=F,n.thin=10,
                 model.file = modelNMRBinary)
print(NMAinJAGS)