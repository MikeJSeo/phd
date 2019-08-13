##### Data generation

set.seed(1115)
N <- 1000 #number of patients per trial
N.trials <- 6 #number of trials
alpha <- c(11, 8, 10.5, 9.6, 12.9, 15.8) #study effects
beta <- c(-2.95, -2.97, -2.89, -2.91, -2.93, -2.90) #treatment effects
gamma <- c(0.24, 0.21, 0.20, 0.18, 0.25, 0.22) #prognostic effects of z
theta <- c(-0.9, -0.5, -0.6, -0.7, -0.1, -0.3) #interaction effects of z
trialid <- c(1:6)
ds <- as.data.frame(array(NA, dim=c(N*6, 4)))
colnames(ds) <- c("trialid", "x", "z", "y")

for (i in 1:N.trials) {
  x <- rbinom(N,1,0.5)
  z <- rnorm(N, mean=rnorm(1, mean=0, sd=0.5), sd=1)
  y <- round(alpha[i] + beta[i]*x + gamma[i]*z + theta[i]*x*z)
  ds[(((i-1)*N)+1):(i*N), ] <- cbind(trialid[i], x, z, y)
}
ds$trialid <- as.factor(ds$trialid)
head(ds)


results <- as.data.frame(array(NA, dim=c(N.trials,2)))
colnames(results) <- c("betai", "var_betai")
for (i in 1:N.trials) {
  dsi <- as.data.frame(ds[which(ds$trialid==i),])
  fit <- glm(y~x, data=dsi)
  results[i,] <- c(coefficients(fit)[2], vcov(fit)[2,2])
}

library(mvmeta)
fit <- mvmeta(betai~1, S = var_betai, data= results, method = "reml")
summary(fit)

library(lme4)
lmer(y ~ 0 + trialid + x + (x-1| trialid), data = ds)

###### adding heterogeneity
results <- as.data.frame(array(NA, dim=c(N.trials,2)))
colnames(results) <- c("betai", "var_betai")
for (i in 1:N.trials) {
  dsi <- as.data.frame(ds[which(ds$trialid==i),])
  fit <- glm(y~x+z+x:z, data=dsi)
  results[i,] <- c(coefficients(fit)["x"], vcov(fit)["x", "x"])
}

fit <- mvmeta(betai~1, S = var_betai, data= results, method = "reml")
summary(fit)

# one stage
lmer(y ~ 0 + trialid + x + (x-1|trialid) + z:trialid + z:x:trialid, data=ds)

###################
#Can you change the code to allow for exchangeable slope coefficients across studies? 
lmer(y ~ 0 + trialid + x + z + (x-1|trialid) + (z-1|trialid) + z:x:trialid, data=ds)

#lmer(y ~ 0 + trialid + x + (x-1|trialid) + (z|trialid)  + z:x:trialid, data=ds)



#Can you change the code to fit the model that disentangles within- from across-studies relationships (i.e. to account for ecological bias) 
z_mean0 <- aggregate(ds$z, list(ds$trialid), mean)
z_mean <- z_mean0[rep(row.names(z_mean0), rep(1000,6)), 2]

ds$z_mean <- z_mean
ds$z_subtracted <- ds$z - ds$z_mean

lmer(y ~ 0 + trialid + x + (x-1|trialid) + z:trialid + z_mean:x + z_subtracted:x, data=ds)




#how do you interpret this...