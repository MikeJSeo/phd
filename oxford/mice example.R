library(mvtnorm)
library(dplyr) #for data manipulation
library(mice) #2l.norm
library(miceadds) #2l.pmm
library(micemd) #2l.2stage.norm
library(mitools) # for function imputationList
library(lme4) #lmer
library(broom.mixed) #for pool function


generate_ipdma_example_with_systematic_missing <- function(){
  
  set.seed(1)
  Nstudies <- 50
  Npatients <- 1500
  Npatients.tot <- Nstudies*Npatients
  study <- rep(1:Nstudies, each = Npatients)
  treat <- rbinom(Npatients.tot, 1, 0.5)
  
  a <- 1
  b <- c(0.7, 1, 0.7, 0.5)
  c <- c(0.1, 0.5, 0.2, 0.2)
  d <- 0.5
  Sigma <- matrix(c(0.2^2, -0.1*0.2*0.2, -0.1*0.2*0.2, 0.2^2), nrow = 2)
  
  u <- rmvnorm(Nstudies, rep(0, 2), matrix(c(0.2^2, 0.2 * 0.2 * -0.1, 0.2 * 0.2 * -0.1, 0.2^2), nrow = 2) )
  
  #generate x1
  gamma1 <- rnorm(Nstudies, 0, 0.5)
  e1 <- rnorm(Npatients.tot, 0, 0.2)
  x1 <- rep(gamma1, each = Npatients)+ e1
  
  #generate x2
  gamma2 <- rmvnorm(Nstudies, c(0, 0), matrix(c(0.5^2, 0.2 * 0.5 * 0.5, 0.2 * 0.5 * 0.5, 0.5^2), nrow = 2))
  e2 <- rnorm(Npatients.tot, 0, 0.1)
  x2 <- 0.3 * x1 + gamma2[,1] + gamma2[,2]*x1 + e2
  #  gamma2 <- rnorm(Nstudies, 0, 0.5)
  #  x2 <- rep(gamma2, each = Npatients) + e2
  
  #generate x3
  p3 <- runif(Nstudies, 0.05, 0.15)
  x3 <- rbinom(Npatients.tot, 1, rep(p3, each = Npatients))
  
  #generate x4
  p4 <- runif(Nstudies, 0.15, 0.25)
  x4 <- rbinom(Npatients.tot, 1, rep(p4, each = Npatients))
  
  #generate y
  y <- rep(a, Npatients.tot) + b[1] * x1 + b[2] * x2 + b[3] * x3 + b[4] * x4 + 
    c[1] * x1 * treat + c[2] * x2 * treat + c[3] * x3 * treat + c[4] * x4 * treat +
    d * treat + u[,1] + u[,2] * treat
  
  #generate systematically missing framework in x2
  pi_sys <- 0.2
  study_missing <- as.logical(rep(rbinom(Nstudies, 1, 0.2), each = Npatients))
  x2[study_missing] <- NA
  
  #generate sporadically missing framework for all variables
  X <- cbind(x1, x2, x3, x4)
  X[as.logical(rbinom(Npatients.tot * 4, 1, 0.05))] <- NA
  
  return(as_tibble(cbind(y, X, study, treat)))
}

simulated_data <- generate_ipdma_example_with_systematic_missing() #x2 is the systematically missing variable


#For more complete information, refer to Chapter 7 in the book https://stefvanbuuren.name/fimd/sec-mlguidelines.html 

#Case1: No systematically missing data and no covariate-treatment interactions
#2l.norm: Multilevel method using the linear mixed model with heterogeneous error variances;
#2l.pmm: Predictive mean matching based on predictions from the linear mixed model, with random draws from the regression coefficients and the random effects, using five donors.

data <- simulated_data %>% select(-x2)

meth <- make.method(data)
meth[c("x1", "x3", "x4")] <-"2l.norm"
#meth[c("x1", "x3", "x4")] <-"2l.pmm"

pred <- make.predictorMatrix(data)
pred[,] <- 0
pred[, "study"] <- -2 # -2 specifies the clustering variable

codes <- c(1, 1, 1, 2) # 2 incorporates random effects on treatment effect
pred["x1", c("y", "x3", "x4", "treat")] <- codes
pred["x3", c("y", "x1", "x4", "treat")] <- codes
pred["x4", c("y", "x1", "x3", "treat")] <- codes

imp <- mice(data, pred = pred, meth = meth)
impc <- complete(imp, "long", include = "TRUE")
imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations #this list can be used in a for loop to run bayesian model for instance; just a convenient function

fit <- with(imp, lmer(y ~ 1 + x1 + x3 + x4 + treat + (1| study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))


#Case2: No systematically missing data and with covariate-treatment interactions

data <- simulated_data %>% select(-x2) %>% mutate(x1.treat = NA, x3.treat = NA, x4.treat = NA)

meth <- make.method(data)
meth[c("x1", "x3", "x4")] <-"2l.norm"

# derive interactions
meth["x1.treat"] <- "~ I(x1 * treat)"
meth["x3.treat"] <- "~ I(x3 * treat)"
meth["x4.treat"] <- "~ I(x4 * treat)"

pred <- make.predictorMatrix(data)
pred[,] <- 0
pred[, "study"] <- -2

codes <- c(1, 1, 1, 2, 1, 1)
pred["x1", c("y", "x3", "x4", "treat", "x3.treat", "x4.treat")] <- codes
pred["x3", c("y", "x1", "x4", "treat", "x1.treat", "x4.treat")] <- codes
pred["x4", c("y", "x1", "x3", "treat", "x1.treat", "x3.treat")] <- codes

imp <- mice(data, pred = pred, meth = meth)
impc <- complete(imp, "long", include = "TRUE")
imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations

fit <- with(imp, lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))


#Case3: Systematically missing data and with covariate-treatment interactions
#A method named 2l.2stage.norm from micemd implements the two-stage method by Resche-Rigon and White (2018)

data <- simulated_data %>% mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)

meth <- make.method(data)
meth[c("x1","x2","x3","x4")] <- "2l.2stage.norm"

# derive interactions
meth["x1.treat"] <- "~ I(x1 * treat)"
meth["x2.treat"] <- "~ I(x2 * treat)"
meth["x3.treat"] <- "~ I(x3 * treat)"
meth["x4.treat"] <- "~ I(x4 * treat)"

pred <- make.predictorMatrix(data)
pred[,] <- 0
pred[, "study"] <- -2

codes <- c(1, 1, 1, 1, 2, rep(1, 3))
pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes


imp <- mice(data, pred = pred, meth = meth)
impc <- complete(imp, "long", include = "TRUE")
imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations

fit <- with(imp, lmer(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat + x3*treat + x4*treat + (1| study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))

