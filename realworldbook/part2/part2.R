#devtools::install_github("MikeJSeo/bipd")
#install.packages("bipd")
library(bipd)

setwd("C:/Users/mike/Desktop")
data=readRDS("example")

data$treat.m2 <- as.numeric(factor(data$treat.m2, levels = c("4", "1", "2", "3")))
data$gender <- as.numeric(data$gender)
data$employ2=1*(data$employ==2)
data$employ3=1*(data$employ==3)
data$employ4=1*(data$employ==4)

ipd <- with(data, ipdnma.model.onestage(y = y, study = study, treat = treat.m2, 
                                        X = data[,c("baseline", "age", "relstat", "gender", "employ2", "employ3", "employ4")], 
                                        response = "normal", shrinkage = "laplace", prec.d = 0.1, prec.alpha = 0.1,
                                        prec.beta = 0.1, lambda.prior = list("dgamma", 2, 0.1)))
cat(ipd$code)
samples <- ipd.run(ipd,  pars.save = c("beta", "gamma", "delta", "lambda", "tt"), n.chains = 3, n.burnin = 500, n.iter = 5000)
summary(samples)