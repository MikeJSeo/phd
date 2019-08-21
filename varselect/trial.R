generate.simulation <- function(Nstudies = NULL, Ncovariate = NULL, continuous.cov = NULL, pf = NULL, em = NULL,
                                b1 = NULL, b2 = NULL, sampleSize = c(500, 1000), model = "continuous"){
  
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
  py <- expit(meany)
  
  if(model == "continuous"){
    y <- rnorm(length(studyid), meany, sigmay)
  } else if (model == "binomial"){
    y <- rbinom(length(studyid), 1, py)
  }
  
  data <- cbind(y = y, data = data, studyid = studyid)
  data <- as.data.frame(data)
  data$studyid <- as.factor(data$studyid)
  return(data)
  
}

#glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + treat")
glmmLasso_formula <- as.formula("y ~ as.factor(studyid) + X1 + X2 + X3 + X4 + X5 + X1_treat + X2_treat + X3_treat + X4_treat + X5_treat + treat")
glmmLasso_formula2 <-  list(studyid =~ -1 + treat)

data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2)
colnames(data) <- gsub(":", "_", colnames(data))  


glm2 <- glmmLasso(glmmLasso_formula, rnd = glmmLasso_formula2,  
                      family = gaussian(link="identity"), 
                      lambda = 0,
                      data = data)
              #        control = list(index = c(NA, 1:((dim(data)[2] - 3)), NA)))
summary(glm2)

glmm_full_formula <- as.formula("y ~ studyid + treat + (X1 + X2 + X3 + X4 + X5)*treat + (-1 + treat|studyid)")
lmer(glmm_full_formula, data = data)
