generate.simulation <- function(Nstudies = NULL, Ncovariate = NULL, continuous.cov = NULL, pf = NULL, em = NULL,
                                b1 = NULL, b2 = NULL, sampleSize = c(500, 1000)){
  
  #treatment effect
  d <- 1
  sd <- 0.2
  delta <- rnorm(Nstudies, d, sd)
  
  studyid <- NULL
  for(i in 1:Nstudies){
    studyid <- c(studyid, rep(i, sample(sampleSize[1]:sampleSize[2], size = 1)))
  }
  
  #study baseline effect
  alpha <- runif(Nstudies, -1, 1)
  treat <- rbinom(length(studyid), 1, 0.5)
  
  #generating data
  rho <- 0.3
  len <- length(continuous.cov)
  
  cov_matrix <- matrix(NA, nrow = len, ncol = len)
  for(ii in 1:len){
    for(jj in 1:len){
      cov_matrix[ii,jj] <- rho^abs(ii - jj) 
    }
  }
  X <- matrix(NA, nrow = length(studyid), ncol = Ncovariate)
  for(i in 1:length(studyid)){
    X[i,continuous.cov] <-  mvrnorm(n = 1, mu = rep(0, len), cov_matrix)
  }
  X[,-continuous.cov] <- rbinom(length(studyid)* (Ncovariate - length(continuous.cov)), 1, 0.5)
  
  data <- model.matrix(~ -1 +  X*treat)
  data[,-(Ncovariate+1)] <- apply(data[,-(Ncovariate+1)], 2, scale) # standardize data except treatment
  
  meany <- alpha[studyid] + delta[studyid] * treat + data[,pf, drop = FALSE] %*% b1 + data[,Ncovariate + 1 + em, drop = FALSE] %*% b2
  sigmay <- 0.5
  
  y <- rnorm(length(studyid), meany, sigmay)

  data <- cbind(y = y, data = data, studyid = studyid)
  data <- as.data.frame(data)
  data$studyid <- as.factor(data$studyid)
  return(data)
}


#simulate data
library(MASS) 
set.seed(6)
data <-generate.simulation(Nstudies = 5, Ncovariate = 5, continuous.cov = c(1, 3, 4), pf =  c(1,2,3), em = 3, b1 = c(0.1, 0.5, 0.1), b2 = 0.1)


## first fit good starting model
library(MASS);library(nlme)

PQL<-glmmPQL(y ~ studyid + (X1 + X2 + X3 + X4 + X5)*treat,
             random = ~ -1 + treat|studyid, family=gaussian,data=data)
PQL$coef$fixed["treat"]
sqrt(as.numeric(VarCorr(PQL)[1,1]))

## glmmLasso
Q_start <- as.numeric(VarCorr(PQL)[1,1])
#Delta.start <- c( 0, rep(0, 15), rep(0, 5))
dummy <- as.numeric(c(PQL$coef$fixed[-which(names(PQL$coef$fixed) == "treat")], PQL$coef$fixed[which(names(PQL$coef$fixed) == "treat")]))
Delta.start <- c(dummy, as.numeric(t(PQL$coef$random$studyid)))


library(glmmLasso)
colnames(data) <- gsub(":", "_", colnames(data))  
glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + X1 + X2 + X3 + X4 + X5 + X1_treat + X2_treat + X3_treat + X4_treat + X5_treat + treat")
glmmLasso_formula2 <- list(studyid =~ -1 + treat) 
glm2 <- glmmLasso(glmmLasso_formula, rnd = glmmLasso_formula2,  
                  family = gaussian(link="identity"), 
                  lambda = 1000,
                  control = list(index = c(NA, 1:((dim(data)[2] - 3)), NA), start = Delta.start, q_start = Q_start),
                  data = data)
glm2$coefficients["treat"]
glm2$StdDev



## first fit good starting model
library(MASS);library(nlme)

#glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + treat")
glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + X1 + X2 + X3 + X4 + X5 + X1_treat + X2_treat + X3_treat + X4_treat + X5_treat + treat")
#glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X1_treat + X2_treat + X3_treat + X4_treat + X5_treat + X6_treat + X7_treat + X8_treat + X9_treat + X10_treat + treat")
#glmmLasso_formula2 <-  list(studyid =~ -1 + treat)
glmmLasso_formula2 <- list(studyid =~ -1 + treat)



lmer1 <- lmer(glmm_full_formula, data = data)


PQL<-glmmPQL(y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat,
              random = ~ -1 + treat|studyid, family=gaussian,data=data)
#PQL<-glmmPQL(y ~ 1, random = ~ 0 + treat|studyid, family=gaussian,data=data) 

#PQL<-glmmPQL(y ~ 1,random = ~ -1 + treat|studyid,family=gaussian,data=data)
#Delta.start<-c(as.numeric(PQL$coef$fixed), rep(0, Ncovariate*3), as.numeric(t(PQL$coef$random$studyid)))


#Delta.start <- c(as.numeric(PQL$coef$fixed)[1], rep(0, 30), as.numeric(t(PQL$coef$random$studyid)))
Q.start<-as.numeric(VarCorr(PQL)[1,1])

Q.start <- 

glm2 <- glmmLasso(glmmLasso_formula, rnd = glmmLasso_formula2,  
                      family = gaussian(link="identity"), 
                      lambda = 0,
                      data = data)
                      control = list(q_start = Q.start, Delta.start = Delta.start))
 #                     )
#                      control = list(index = c(NA, 1:((dim(data)[2] - 3)), NA)))
summary(glm2)

glmm_full_formula <- as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat + (-1 + treat|studyid)")
lmer(glmm_full_formula, data = data)


