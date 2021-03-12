##helpful reference
#http://anythingbutrbitrary.blogspot.com/2012/06/random-regression-coefficients-using.html
#http://anythingbutrbitrary.blogspot.com/2012/10/hierarchical-linear-models-and-lmer.html

Nstudies <- 60
Npatients <- 1500
Npatients.tot <- Nstudies*Npatients
study <- rep(1:Nstudies, each = Npatients)
treat <- rbinom(Npatients.tot, 1, 0.5)

x1m <- rnorm(Nstudies,0,0.1)
x1sd <- (rnorm(Nstudies,0,0.1))^2 #square to prevent negative standard deviation
x1 <- rnorm(length(study), x1m[study], x1sd[study])

x2m <- rnorm(Nstudies,0,0.2)
x2sd <- (rnorm(Nstudies,0,0.1))^2 #square to prevent negative standard deviation
x2 <- rnorm(length(study), x2m[study], x2sd[study])

x3m <- runif(Nstudies, min=0.05, max = 0.15)
x3 <- rbinom(length(study), size = 1, x3m[study])

x4m <- runif(Nstudies, min=0.15, max = 0.25)
x4 <- rbinom(length(study), size = 1, x4m[study])


# Random (intercept and treatment) effect model
random1 <- rnorm(Nstudies, 0, 0.4)
random.effects1 <- random1[study] #intercept

random2 <- rnorm(Nstudies, 0, 0.2)
random.effects2 <- random2[study] #treatment

epsilon <- rnorm(Npatients.tot, 0, 0.3)

y <- 1 + random.effects1 + x1 + x2 + x3 + x4 + treat* (0.5 + random.effects2 + 0.3 * x1 + 0.1 * x2 + 0.2 * x3 + 0.2 * x4) + epsilon

dat.complete <- data.frame(study = as.factor(study), treat, x1, x2, x3, x4, y)

#library(lme4)
#re.lm <- lmer(y ~ (x1+ x2 + x3 + x4) * treat + (0 + treat|study) + (1 |study), data = dat.complete) 
#summary(re.lm)

