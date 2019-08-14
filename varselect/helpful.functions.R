
cv.glmmLasso <- function(data_glmmLasso, form.fixed = NULL, form.rnd = NULL, lambda = NULL, family = NULL){
  
  N <-dim(data_glmmLasso)[1]
  ind<-sample(N,N)
  
  kk<-5 # 5 fold cross-validation
  nk <- floor(N/kk)
  
  Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
  
  for(j in 1:length(lambda)){
    print(paste("Iteration ", j,sep=""))
    
    for (i in 1:kk)
    {
      if (i < kk){
        indi <- ind[(i-1)*nk+(1:nk)]
      } else{
        indi <- ind[((i-1)*nk+1):N]
      }
    
      data_glmmLasso_train <- data_glmmLasso[-indi,]
      data_glmmLasso_test <- data_glmmLasso[indi,]

      glm2 <- try(glmmLasso(form.fixed, rnd = form.rnd,  
                            family = family, 
                            lambda = lambda[j],
                            data = data_glmmLasso_train,
                            final.re = FALSE,
                            control = list(index = c(NA, 1:((dim(data_glmmLasso)[2] - 3)), NA)))
                  ,silent=TRUE) 
      if(class(glm2)!="try-error")
      {  
        y.hat<-predict(glm2,data_glmmLasso_test)    
        Devianz_ma[j,i]<-sum(family$dev.resids(data_glmmLasso_test$y,y.hat,wt=rep(1,length(y.hat))))
      }
    }
  }
  Devianz_vec<-apply(Devianz_ma,1,sum)
  opt2<-which.min(Devianz_vec)
  print(Devianz_vec)
  print(paste0("optimal lambda value is ", lambda[opt2]))
  
  glm2 <- try(glmmLasso(form.fixed, rnd = form.rnd,  
                        family = family, 
                        lambda = lambda[opt2],
                        data = data_glmmLasso_train,
                        final.re = FALSE,
                        control = list(index = c(NA, 1:((dim(data_glmmLasso)[2] - 3)), NA)))
              ,silent=TRUE) 
  glm2
}


  
generate.simulation <- function(Nstudies = 5, Ncovariate = 10, continuous.cov = c(1,2,4,6,7,8), pf = c(1,2,3,4,5), em = c(4,5),
                                b1 = c(0.1, -0.1, 0.2, 0.2, -0.2), b2 = c(0.2, 0.3), sampleSize = c(30, 200), model = "continuous"){
  
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
  
  meany <- alpha[studyid] + delta[studyid] * treat + X[,pf] %*% b1 + X[,em] %*% b2 * treat  
  sigmay <- 0.5
  py <- expit(meany)
  
  if(model == "continuous"){
    y <- rnorm(length(studyid), meany, sigmay)  
  } else if (model == "binomial"){
    y <- rbinom(length(studyid), 1, py)
  }
  
  data <- model.matrix(~ -1 +  X*treat)
  data <- cbind(y = y, data = data, studyid = studyid)
  data <- as.data.frame(data)
  data$studyid <- as.factor(data$studyid)
  return(data)
  
}


expit <- function(x){
  exp(x)/(1+exp(x))
}

calc_mse <- function(a, b){
  mean((a - b)^2)
}

find_performance2 <- function(mse, correct_values, correct_em){
  
  mse_without_treat <- mse[-length(mse)]
  mse_treat <- mse[length(mse)]
  c(calc_mse(mse_without_treat[correct_em != 1], correct_values[correct_em != 1]),
    calc_mse(mse_without_treat[correct_em == 1], correct_values[correct_em == 1]),
    calc_mse(mse_treat, 1))
}