
## code to do cross validation for glmmLasso to get optimal lambda value

cv.glmmLasso <- function(data_glmmLasso, form.fixed = NULL, form.rnd = NULL, lambda = NULL, family = NULL, q_start = NULL, start = NULL){
  
  N <-dim(data_glmmLasso)[1]
  ind<-sample(N,N)
  
  kk <- 5 # 5 fold cross-validation
  nk <- floor(N/kk)
  
  if(length(lambda) > 1){
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
                            control = list(index = c(NA, 1:((dim(data_glmmLasso)[2] - 3)), NA), q_start = q_start, start = start,
                                           center = FALSE, standardize = FALSE))
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
  #print(Devianz_vec)
  #print(paste0("optimal lambda value is ", lambda[opt2]))
  lambda.min <- lambda[opt2]
  } else{
    lambda.min <- lambda
  }

  
  glm2 <- try(glmmLasso(form.fixed, rnd = form.rnd,  
                        family = family, 
                        lambda = lambda.min,
                        data = data_glmmLasso,
                        control = list(index = c(NA, 1:((dim(data_glmmLasso)[2] - 3)), NA), q_start = q_start, start = start,
                                       center = FALSE, standardize = FALSE))
              ,silent=TRUE)
  list(glm2 = glm2, lambda.min = lambda.min)
}


## Code to generate simulation
  
generate.simulation <- function(Nstudies = NULL, Ncovariate = NULL, continuous.cov = NULL, pf = NULL, em = NULL,
                                b1 = NULL, b2 = NULL, sampleSize = c(100, 150), model.type = "gaussian"){
  
  #treatment effect
  d <- 1
  sd <- 0.2
  delta <- rnorm(Nstudies, d, sd)
  
  studyid <- NULL
  for(i in 1:Nstudies){
    studyid <- c(studyid, rep(i, sample(sampleSize[1]:sampleSize[2], size = 1)))
  }
  
  #study baseline effect
  if(model.type == "gaussian"){
    alpha <- runif(Nstudies, -1, 1)
  } else{
    alpha <- runif(Nstudies, -3, -2)
  }
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
  
  # standardize X: binary and continuous variables in same scale
  X <- apply(X, 2, scale)
  
  meany <- alpha[studyid] + delta[studyid] * treat + X[,pf, drop = FALSE] %*% b1 + X[,em, drop = FALSE] %*% b2 * treat
  sigmay <- 0.5
  py <- expit(meany)

  if(model.type == "gaussian"){
    y <- rnorm(length(studyid), meany, sigmay)
  } else if (model.type == "binary"){
    y <- rbinom(length(studyid), 1, py)
  }

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

find_performance <- function(val, correct_values, correct_em){
  
  val_without_treat <- val[-length(val)]
  val_treat <- val[length(val)]
  c(calc_mse(val_without_treat[correct_em != 1], correct_values[correct_em != 1]),
    calc_mse(val_without_treat[correct_em == 1], correct_values[correct_em == 1]),
    calc_mse(val_treat, 1))
}


find_performance2 <- function(val, correct_em, continuous.cov){
  
  val_treat <- val[length(val)]
  
  continuous.indicator <- rep(0, length(correct_em))
  continuous.indicator[continuous.cov] <- 1
  
  val_without_treat <- val[-length(val)]
  true_em_value_continuous <- val_without_treat[correct_em == 1 & continuous.indicator == 1]
  true_em_value_continuous <- true_em_value_continuous[true_em_value_continuous != 0]
  
  true_em_value_binary <- val_without_treat[correct_em == 1 & continuous.indicator == 0]
  true_em_value_binary <- true_em_value_binary[true_em_value_binary != 0]
  
  c(ifelse(length(true_em_value_continuous) == 0, NA, mean(true_em_value_continuous)),
    ifelse(length(true_em_value_binary) == 0, NA, mean(true_em_value_binary)),
    val_treat)
}


bootstrap_function_LASSO  <- function(model_data, ndraws, p.fac, family) {
  
  coeff_mtx <- matrix(0, nrow = ndraws, ncol = length(col_labels))
  
  for (ii in 1:ndraws) {

    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, standardize = FALSE)  
    aa <- coef(bootstrap_model, s = "lambda.min")
    coeff_mtx[ii,]   <- sapply(col_labels, function(x) ifelse(x %in% rownames(aa)[aa[,1] != 0], aa[x,1], 0))  
  }
  
  se <- apply(coeff_mtx, 2, sd, na.rm = TRUE)
  return(se)
}


bootstrap_function_glmmLasso  <- function(model_data, ndraws, lambda = NULL, model.type = NULL, form.fixed, form.rnd, q_start = NULL, start = NULL) {
  
  if(is.null(model.type)) stop("model type missing")

  coeff_mtx <- matrix(0, nrow = ndraws, ncol = length(col_labels))
  
  for (ii in 1:ndraws) {

    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    if(model.type == "gaussian"){
      bootstrap_model <- cv.glmmLasso(bootstrap_data, form.fixed = form.fixed, form.rnd = form.rnd, lambda = lambda, family = gaussian(link="identity"), q_start = q_start, start = start)
    }
    aa <- summary(bootstrap_model[[1]])$coefficients
    aa <- rownames(aa[aa[,"Estimate"] != 0,])
      
    coeff_mtx[ii,] <- sapply(col_labels_glmmLasso, function(x) ifelse(x %in% aa, summary(bootstrap_model[[1]])$coefficients[x,"Estimate"], 0))
  }
  
  se <- apply(coeff_mtx, 2, sd, na.rm = TRUE)
  return(se)
}


