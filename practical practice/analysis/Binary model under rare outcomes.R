########################################### rare event Simulation

OR <- 2
tau_sq <- 0.3 #tau is 0.547
n.study <- 50
theta <- rnorm(n.study, log(OR), sqrt(tau_sq))

pA <- runif(n.study, 0.2, 0.5)

#rare probability - we see that normal approximation doesn't work well under this scenario
#pA <- runif(n.study, 0.01, 0.02)

logit = function(p) { log(p/(1-p)) }
invlogit = function(x) { 1/(1+exp(-x)) }
pB <- invlogit(logit(pA) + theta)

n.A <- sample(40:60, size = n.study, replace = TRUE)
n.B <- sample(40:60, size = n.study, replace = TRUE)

r.A <- r.B <- NULL
for(i in 1:length(n.A)){
  r.A[i] <- rbinom(1, n.A[i], pA[i])
  r.B[i] <- rbinom(1, n.B[i], pB[i])
}

data_jags <- list(ns=n.study, 
                  r.A=r.A,
                  r.B=r.B,
                  n.A=n.A,
                  n.B=n.B)

url <- "https://raw.githubusercontent.com/MikeJSeo/jags/master/code/practical8-1.txt"
data <- getURL(url)

model <- jags.model(textConnection(data), data = data_jags)
samples <- coda.samples(model, variable.names = c("OR","tau"), n.iter = 50000)
summary(samples) # we find similar OR and tau


########## now use normal approximation to estimate
#calculate log odds ratio
#continuity correction
r.A[r.A == 0] = 0.5
r.B[r.B == 0] = 0.5

logOR <- log((r.B/(n.B-r.B)) / (r.A/(n.A-r.A)))
varr <- 1/r.A + 1/(n.A - r.A) + 1/(n.B - r.B) + 1/r.B

data_jags <- list(ns=n.study, 
                  y=logOR,
                  varr=varr)

url <- "https://raw.githubusercontent.com/MikeJSeo/jags/master/code/practical8-2.txt"
data <- getURL(url)

model <- jags.model(textConnection(data), data = data_jags)

samples <- coda.samples(model, variable.names = c("OR","tau"), n.iter = 50000)
summary(samples)




###################################################################
###################################################################
############### Run this multiple times############################

store <- matrix(NA, 100, 2)


for(ii in 1:100){
  
  
  OR <- 2
  tau_sq <- 0.3 #tau is 0.547
  n.study <- 100
  theta <- rnorm(n.study, log(OR), sqrt(tau_sq))
  
  #pA <- runif(n.study, 0.2, 0.5)
  
  #rare probability
  pA <- runif(n.study, 0.01, 0.02)
  
  logit = function(p) { log(p/(1-p)) }
  invlogit = function(x) { 1/(1+exp(-x)) }
  pB <- invlogit(logit(pA) + theta)
  
  n.A <- sample(40:60, size = n.study, replace = TRUE)
  n.B <- sample(40:60, size = n.study, replace = TRUE)
  
  r.A <- r.B <- NULL
  for(i in 1:length(n.A)){
    r.A[i] <- rbinom(1, n.A[i], pA[i])
    r.B[i] <- rbinom(1, n.B[i], pB[i])
  }
  
  #continuity correction
  r.A[r.A == 0] = 0.5
  r.B[r.B == 0] = 0.5
  
  logOR <- log((r.B/(n.B-r.B)) / (r.A/(n.A-r.A)))
  varr <- 1/r.A + 1/(n.A - r.A) + 1/(n.B - r.B) + 1/r.B
  
  data_jags <- list(ns=n.study, 
                    y=logOR,
                    varr=varr)
  
  url <- "https://raw.githubusercontent.com/MikeJSeo/jags/master/code/practical8-2.txt"
  data <- getURL(url)
  
  model <- jags.model(textConnection(data), data = data_jags)
  
  samples <- coda.samples(model, variable.names = c("OR","tau"), n.iter = 10000)
  store[ii,] <- c(summary(samples)$statistics["OR", "Mean"], summary(samples)$statistics["tau", "Mean"])
  
}

hist(store[,1]) # for OR
hist(store[,2]) # for tau
