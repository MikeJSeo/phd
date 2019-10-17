## Code to generate simulation
  
generate.simulation <- function(Nstudies = NULL, Ncovariate = NULL, continuous.cov = NULL, pf = NULL, em = NULL,
                                b1 = NULL, b2 = NULL, sampleSize = c(50, 100), model.type = "gaussian", tau = 0.2, phi = 0.2){
  
  #treatment effect
  d <- 1
  delta <- rnorm(Nstudies, d, tau)
  
  #effect modification
  b <- matrix(NA, nrow = Nstudies, ncol = length(em))
  for(i in 2:length(b2)){
    b[,i] <- rnorm(Nstudies, b2[i], phi)
  }
  
  studyid <- NULL
  for(i in 1:Nstudies){
    studyid <- c(studyid, rep(i, sample(sampleSize[1]:sampleSize[2], size = 1)))
  }
  
  #study baseline effect
  if(model.type == "gaussian"){
    alpha <- runif(Nstudies, -1, 1)
  } else{
    alpha <- runif(Nstudies, -1, 0) 
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
  data <- model.matrix(~ -1 + X*treat)
  
  meany <- alpha[studyid] + delta[studyid] * treat + X[,pf, drop = FALSE] %*% b1 + apply(X[,em, drop = FALSE] * b[studyid,] * treat,1,sum)
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



find_performance <- function(val, correct_values, correct_em, data_subset){
  
  val_without_treat <- val[-length(val)]
  val_treat <- val[length(val)]
  c(calc_mse(val_without_treat[correct_em != 1], correct_values[correct_em != 1]),
    calc_mse(val_without_treat[correct_em == 1], correct_values[correct_em == 1]),
    calc_mse(val_treat, 1),
    mean((data_subset %*% val - data_subset %*% c(correct_values, 1))^2)
    )
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

# calculates patient specific treatment effect MSE
find_performance3 <- function(data_subset, val, correct_values){
  #treatment effect is assumed to be always 1 in the data generating mechanism
  mean((data_subset %*% val - data_subset %*% c(correct_values, 1))^2) 
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


