
generate_simulation_data <- function(Ncov = 5, sys_missing_prob = 0.3, signal = "small", nonlinear = "none", effect_modifiers = "none"){
  
  Nstudies <- 10
  Npatients <- sample(150:500, Nstudies, replace = TRUE)
  Npatients.tot <- sum(Npatients)
  study <- rep(1:Nstudies, times = Npatients)
  
  a <- runif(Nstudies, 0.5, 1.5)
  a <- rep(a, times = Npatients)
  
  ## generate covariates
  rho <- 0.2
  Omega <- diag(1, Ncov)
  for(i in 1:Ncov){
    for(j in 1:Ncov){
      Omega[i,j] <- rho^abs(i - j) 
    }
  }
  sigma2 <- 1
  
  X <- NULL
  for(i in 1:Nstudies){
    mu <- runif(Ncov, -0.5, 0.5)
    X <- rbind(X, rmvnorm(Npatients[i], mu, Omega * sigma2))
  }
  
  #categorize predictors
  X[,2] <- ifelse(X[,2] > 0, 1, 0)
  X[,3] <- ifelse(X[,3] > 1, 1, 0)
  
  #nonlinearity
  if(nonlinear != "none"){
    X <- cbind(X, X[,1]^2, X[,1]^3, X[,4]^2, X[,4]^3, X[,5]^2, X[,5]^3)  
  }
  
  b <- rep(0.1, Ncov)
  if(nonlinear == "small"){
    b <- c(b, rep(0.05, 6))
  } else if(nonlinear == "large"){
    b <- c(b, rep(0.2, 6))
  }
  
  if(signal == "small"){
    e_vec <- rnorm(Npatients.tot, 0, 1) # R squared around 0.1
  } else if(signal == "large"){
    e_vec <- rnorm(Npatients.tot, 0, 0.1) #R squared around 0.6
  }
  
  if(effect_modifiers == "yes"){
    treat <- rbinom(Npatients.tot, 1, 0.4)
    Xinteraction <- X[,1:Ncov] * treat
    cvec <- rep(0.05, Ncov)
    
    d <- rnorm(Nstudies, 1, 0.5)
    d <- rep(d, times = Npatients)
  }
  
  if(effect_modifiers == "none"){
    y <- a + X %*% b + e_vec    
  } else if(effect_modifiers == "yes"){
    y <- a + X %*% b + e_vec + d *treat + Xinteraction %*% cvec
  }
  
  summary(lm(y ~ X + treat + Xinteraction))
  
  # introduce systematically missing; first two predictors are always observed
  for(i in 3:Ncov){
    systematic_missing_study_dummy <- rbinom(Nstudies, 1, sys_missing_prob)
    #systematic_missing_study <- (1:Nstudies)[systematic_missing_study_dummy == 1]  
    study_missing <- as.logical(rep(systematic_missing_study_dummy, times = Npatients))  
    X[,i][study_missing] <- NA
  }    
    
  
  #X[,1:Ncov] 
  #dataset <- as_tibble(cbind(y, X, study))

  return(list(y = y , X = X, study = study, systematic_missing_study = systematic_missing_study, dataset = dataset))
}

findPrediction1 <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()

  sys_studies <- crossdata$systematic_missing_study
  
  dataset <- crossdata$dataset
  
  for(studyid in 1:nstudy){
    training_set <- dataset[dataset$study != studyid,]
    testing_set <- dataset[dataset$study == studyid,]
    
    if(method == "naive"){
      
      training_set <- training_set %>% select(-x2)
      testing_set <- testing_set %>% select(-x2)
      
      trained_model <- lmer(y ~ 1 + x1 + x3 + (1| study), data = training_set)
      bb <- model.matrix(y ~ x1 + x3, data = testing_set)
      predictions[[studyid]] <- bb %*% fixef(trained_model)
      
    } else if(method == "imputation"){
      
      meth <- make.method(training_set)
      meth[c("x2")] <- "2l.2stage.norm"
      
      pred <- make.predictorMatrix(training_set)
      pred[,] <- 0
      pred[, "study"] <- -2
      
      codes <- c(1, 1, 1)
      pred["x2", c("y", "x1", "x3")] <- codes

      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      if(studyid %in% sys_studies){
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x3 + (1| study), data = imp.dummy)
          bb <- model.matrix(y ~ x1 + x3, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)  
        }
        
      } else{
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x2 + x3 + (1| study), data = imp.dummy)
          bb <- model.matrix(y ~ x1 + x2 + x3, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)
        }
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)

    } else if(method == "separate"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        if(studyname2[i] %in% sys_studies || studyid %in% sys_studies){
          
          training_set_dummy <- training_set %>% select(-x2) 
          training_set_dummy <- training_set_dummy %>% filter(study == studyname2[i])
          
          trained_model <- lm(y ~ 1 + x1 + x3, data = training_set_dummy)
          bb <- model.matrix(y ~ x1 + x3, data = testing_set)
          prediction_store[,i] <- bb %*% coef(trained_model)
     
          print(paste0("1st one; ", studyname2[i]))
          
        } else{
          
          training_set_dummy <- training_set %>% filter(study == studyname2[i])

          trained_model <- lm(y ~ 1 + x1 + x2 + x3, data = training_set_dummy)
          bb <- model.matrix(y ~ x1 + x2 + x3, data = testing_set)
          prediction_store[,i] <- bb %*% coef(trained_model)
          
          print(paste0("2nd one; ", studyname2[i]))
        }
      }
      predictions[[studyid]] <- apply(prediction_store, 1, mean)
    } 
    
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}


####################################### Functions for SIMULATION 2

# generate simulation data
generate_data2 <- function(){
  
  set.seed(1)
  Nstudies <- 20
  Npatients <- sample(300:1500, Nstudies, replace = TRUE)
  Npatients.tot <- sum(Npatients)
  study <- rep(1:Nstudies, times = Npatients)
  treat <- rbinom(Npatients.tot, 1, 0.4)
  
  a <- 1
  b <- c(1, 0.5, 0.5, 1)
  c <- c(0.5, 0.5, 0.2, 0.2)
  d <- 0.5
  Sigma <-  matrix(c(0.1^2, -0.1*0.1*0.1, -0.1*0.1*0.1, 0.1^2), nrow = 2)
  
  e_vec <- rnorm(Npatients.tot, 0, 0.1)
  u <- rmvnorm(Nstudies, rep(0, 2), Sigma)
  
  #generate x1
  gamma1 <- rnorm(Nstudies, 0, 0.1)
  epsilon1 <- rnorm(Npatients.tot, 0, 0.1)
  x1 <- rep(gamma1, times = Npatients)+ epsilon1
  
  #generate x2
  gamma2 <- rnorm(Nstudies, 0, 0.1)
  epsilon2 <- rnorm(Npatients.tot, 0, 0.1)
  x2 <- rep(gamma2, times = Npatients)+ epsilon2
  
  #generate x3
  p3 <- runif(Nstudies, 0.05, 0.15)
  x3 <- rbinom(Npatients.tot, 1, rep(p3, times = Npatients))
  
  #generate x4
  p4 <- runif(Nstudies, 0.15, 0.25)
  x4 <- rbinom(Npatients.tot, 1, rep(p4, times = Npatients))
  
  #generate y
  y <- rep(a, Npatients.tot) + b[1] * x1 + b[2] * x2 + b[3] * x3 + b[4] * x4 + 
    c[1] * x1 * treat + c[2] * x2 * treat + c[3] * x3 * treat + c[4] * x4 * treat +
    d * treat + rep(u[,1],  times = Npatients) + rep(u[,2],  times = Npatients) *treat + e_vec
  
  #generate systematically missing framework in x2
  pi_sys <- 0.4
  systematic_missing_study_dummy <- rbinom(Nstudies, 1, pi_sys)
  systematic_missing_study <- (1:Nstudies)[systematic_missing_study_dummy == 1]
  
  study_missing <- as.logical(rep(systematic_missing_study_dummy, times = Npatients))
  x2[study_missing] <- NA
  
  X <- cbind(x1, x2, x3, x4)
  
  dataset <- as_tibble(cbind(y, X, study, treat))
  
  return(list(y = y , X = X, study = study, systematic_missing_study = systematic_missing_study, dataset = dataset))
}
  


findPrediction2 <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  
  sys_studies <- crossdata$systematic_missing_study
  
  dataset <- crossdata$dataset
  
  for(studyid in 1:nstudy){
    training_set <- dataset[dataset$study != studyid,]
    testing_set <- dataset[dataset$study == studyid,]
    
    if(method == "naive"){
      
      training_set <- training_set %>% select(-x2)
      testing_set <- testing_set %>% select(-x2)
      
      trained_model <- lmer(y ~ 1 + (x1 + x3 + x4)*treat + (1| study) + (0+treat|study), data = training_set)
      bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% fixef(trained_model)
      
    } else if(method == "imputation"){
      
      training_set <- training_set %>% mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)
      
      meth <- make.method(training_set)
      meth[c("x2")] <- "2l.2stage.norm"
      
      pred <- make.predictorMatrix(training_set)
      pred[,] <- 0
      pred[, "study"] <- -2
      
      codes <- c(1, 1, 1, 1, 2, rep(1, 3))
      pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
      
      # derive interactions
      meth["x2.treat"] <- "~ I(x2 * treat)"

      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      if(studyid %in% sys_studies){
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study) + (0 + treat|study), data = imp.dummy)
          bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)  
        }
      } else{
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat + x3*treat + x4*treat + (1| study) + (0 + treat|study), data = imp.dummy)
          bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)
        }
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)

    } else if(method == "separate"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        if(studyname2[i] %in% sys_studies || studyid %in% sys_studies){
          
          training_set_dummy <- training_set %>% select(-x2) 
          training_set_dummy <- training_set_dummy %>% filter(study == studyname2[i])
          
          trained_model <- lm(y ~ 1 + (x1 + x3 + x4) * treat, data = training_set_dummy)
          bb <- model.matrix(y ~ (x1 + x3 + x4)*treat, data = testing_set)
          prediction_store[,i] <- bb %*% coef(trained_model)
          
          print(paste0("1st one; ", studyname2[i]))
          
        } else{
          
          training_set_dummy <- training_set %>% filter(study == studyname2[i])
          
          trained_model <- lm(y ~ 1 + (x1 + x2 + x3 + x4)*treat, data = training_set_dummy)
          bb <- model.matrix(y ~ (x1 + x2 + x3 + x4)*treat, data = testing_set)
          prediction_store[,i] <- bb %*% coef(trained_model)
          
          print(paste0("2nd one; ", studyname2[i]))
        }
      }
      predictions[[studyid]] <- apply(prediction_store, 1, mean)
    } 
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}





findPrediction <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  testing_set_store <- list()
  
  # find studies that are systematically missing in x2
  sys_studies <- crossdata %>% group_by(study) %>% summarise(na_count = sum(is.na(x2)), .groups = "drop" ) %>% filter(na_count == 1500) %>% pull(study)
  
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
    
    #use only complete cases to measure performance
    if(studyid %in% sys_studies){
      testing_set <- testing_set %>% select(-x2) %>% filter(complete.cases(.))
    } else{
      testing_set <- testing_set %>% filter(complete.cases(.))
    }
    
    if(method == "naive"){
      
      training_set <- training_set %>% select(-x2) %>% mutate(x1.treat = NA, x3.treat = NA, x4.treat = NA)
      if(!studyid %in% sys_studies){
        testing_set <- testing_set %>% select(-x2)
      }

      #### impute sporadically missing variables
      meth <- make.method(training_set)
      meth[c("x1", "x3", "x4")] <-"2l.pmm"
      
      pred <- make.predictorMatrix(training_set)
      pred[,] <- 0
      pred[, "study"] <- -2
      
      codes <- c(1, 1, 1, 2, 1, 1)
      pred["x1", c("y", "x3", "x4", "treat", "x3.treat", "x4.treat")] <- codes
      pred["x3", c("y", "x1", "x4", "treat", "x1.treat", "x4.treat")] <- codes
      pred["x4", c("y", "x1", "x3", "treat", "x1.treat", "x3.treat")] <- codes
      
      # derive interactions
      meth["x1.treat"] <- "~ I(x1 * treat)"
      meth["x3.treat"] <- "~ I(x3 * treat)"
      meth["x4.treat"] <- "~ I(x4 * treat)"
      
      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.model <- lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study) + (0 + treat|study), data = imp.dummy)
        bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      testing_set_store[[studyid]] <- testing_set
      #fit <- with(imp, lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study)))
      #t(sapply(fit$analyses, fixef))
      #coef_fit <- summary(pool(fit))
      #bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
      #predictions[[studyid]] <- bb %*% coef_fit[,"estimate"]
    } else if(method == "imputation"){
      
      training_set <- training_set %>% mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)

      meth <- make.method(training_set)
      meth[c("x1","x2","x3","x4")] <- "2l.2stage.norm"
      
      pred <- make.predictorMatrix(training_set)
      pred[,] <- 0
      pred[, "study"] <- -2

      codes <- c(1, 1, 1, 1, 2, rep(1, 3))
      pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
      pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
      pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
      pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes
      
      # derive interactions
      meth["x1.treat"] <- "~ I(x1 * treat)"
      meth["x2.treat"] <- "~ I(x2 * treat)"
      meth["x3.treat"] <- "~ I(x3 * treat)"
      meth["x4.treat"] <- "~ I(x4 * treat)"
      
      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      if(studyid %in% sys_studies){
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study) + (0 + treat|study), data = imp.dummy)
          bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)  
        }
      } else{
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          imp.model <- lmer(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat + x3*treat + x4*treat + (1| study) + (0 + treat|study), data = imp.dummy)
          bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = testing_set)
          prediction.dummy[,ii] <- bb %*% fixef(imp.model)
        }
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      testing_set_store[[studyid]] <- testing_set
      
    } else if(method == "average_predictions"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        if(studyname2[i] %in% sys_studies || studyid %in% sys_studies){
          
          training_set_dummy <- training_set %>% select(-x2) 
          training_set_dummy <- training_set_dummy %>% filter(study == studyname2[i]) %>% 
            mutate(x1.treat = NA, x3.treat = NA, x4.treat = NA)

          meth <- make.method(training_set_dummy)
          meth[c("x1", "x3", "x4")] <-"pmm"
    
          pred <- make.predictorMatrix(training_set_dummy)
          pred[,] <- 0
          
          codes <- c(1, 1, 1, 2, 1, 1)
          pred["x1", c("y", "x3", "x4", "treat", "x3.treat", "x4.treat")] <- codes
          pred["x3", c("y", "x1", "x4", "treat", "x1.treat", "x4.treat")] <- codes
          pred["x4", c("y", "x1", "x3", "treat", "x1.treat", "x3.treat")] <- codes
          
          # derive interactions
          meth["x1.treat"] <- "~ I(x1 * treat)"
          meth["x3.treat"] <- "~ I(x3 * treat)"
          meth["x4.treat"] <- "~ I(x4 * treat)"
          
          imp <- mice(training_set_dummy, pred = pred, meth = meth, printFlag = FALSE)
          impc <- complete(imp, "long", include = "TRUE")
          
          imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
          prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
          
          for(ii in 1:length(imp.list)){
            imp.dummy <- imp.list[[ii]]
            imp.model <- lm(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat, data = imp.dummy)
            bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
            prediction.dummy[,ii] <- bb %*% coef(imp.model)
          }
          prediction_store[,i] <- apply(prediction.dummy, 1, mean)
          
          print(paste0("1st one; ", studyname2[i]))

        } else{
          
          training_set_dummy <- training_set %>% filter(study == studyname2[i]) %>% 
            mutate(x1.treat = NA, x2.treat = NA, x3.treat = NA, x4.treat = NA)
          
          meth <- make.method(training_set_dummy)
          meth[c("x1", "x2", "x3", "x4")] <-"pmm"
          
          pred <- make.predictorMatrix(training_set_dummy)
          pred[,] <- 0
          
          codes <- c(1, 1, 1, 1, 2, 1, 1, 1)
          pred["x1", c("y", "x2", "x3", "x4", "treat", "x2.treat", "x3.treat", "x4.treat")] <- codes
          pred["x2", c("y", "x1", "x3", "x4", "treat", "x1.treat", "x3.treat", "x4.treat")] <- codes
          pred["x3", c("y", "x1", "x2", "x4", "treat", "x1.treat", "x2.treat", "x4.treat")] <- codes
          pred["x4", c("y", "x1", "x2", "x3", "treat", "x1.treat", "x2.treat", "x3.treat")] <- codes
          
          # derive interactions
          meth["x1.treat"] <- "~ I(x1 * treat)"
          meth["x2.treat"] <- "~ I(x2 * treat)"
          meth["x3.treat"] <- "~ I(x3 * treat)"
          meth["x4.treat"] <- "~ I(x4 * treat)"
          
          imp <- mice(training_set_dummy, pred = pred, meth = meth, printFlag = FALSE)
          impc <- complete(imp, "long", include = "TRUE")
          
          imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
          prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
          
          for(ii in 1:length(imp.list)){
            imp.dummy <- imp.list[[ii]]
            imp.model <- lm(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat+ x3*treat + x4*treat, data = imp.dummy)
            bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = testing_set)
            prediction.dummy[,ii] <- bb %*% coef(imp.model)
          }
          prediction_store[,i] <- apply(prediction.dummy, 1, mean)
          
          print(paste0("2nd one; ", studyname2[i]))
        }
      }
      predictions[[studyid]] <- apply(prediction_store, 1, mean)
      testing_set_store[[studyid]] <- testing_set
    } 
    
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}




###############################################################
####### cross validation for real dataset ###################


findPrediction_realdata <- function(crossdata, method){
    
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  testing_set_store <- list()
    
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
  
    if(method == "naive"){
      
      covariates_naive <- c("baseline", "gender")
      #covariates_naive <- c("baseline", "gender", "age")
      #covariates_naive <- c("baseline", "gender", "age", "relstat")
      
      training_set <- training_set %>% select(study, y, treat, all_of(covariates_naive))
      training_set <- createinteractions(training_set, covariates_naive)
      
      testing_set <- testing_set %>% select(study, y, treat, all_of(covariates_naive)) %>% filter(complete.cases(.))
      
      missingPattern <- findMissingPattern(training_set, covariates_naive)  

      meth <- getCorrectMeth(training_set, missingPattern, method)
      pred <- getCorrectPred(training_set, missingPattern, method)
      
      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.model <- lmer(y ~ (baseline + gender) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
        bb <- model.matrix(y ~ (baseline + gender) * treat, data = testing_set)
       
        #imp.model <- lmer(y ~ (baseline + gender + age) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
        #bb <- model.matrix(y ~ (baseline + gender + age) * treat, data = testing_set)
        
        #imp.model <- lmer(y ~ (baseline + gender + age + relstat) * treat + (1| study) + (0 + treat|study), data = imp.dummy)
        #bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      testing_set_store[[studyid]] <- testing_set
    } else if(method == "imputation"){
      
      covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
      
      training_set <- training_set %>% select(study, y, treat, all_of(covariates_all))
      training_set <- createinteractions(training_set, covariates_all)
      #training_set$study <- relabel.vec(training_set$study, order = unique(training_set$study))
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all)  
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      missingPattern <- findMissingPattern(training_set, covariates_all)  
      
      meth <- getCorrectMeth(training_set, missingPattern, method)
      pred <- getCorrectPred(training_set, missingPattern, method)
      
      imp <- mice(training_set, pred = pred, meth = meth)
      impc <- complete(imp, "long", include = "TRUE")
      
      imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
      prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        form <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat + (1|study) + (0 + treat|study)" ))
        imp.model <- lmer(form, data = imp.dummy)

        form2 <- as.formula(paste0("y ~ ", "(", paste(missingPatternTest$without_sys_covariates, collapse= "+"), ") * treat" ))
        bb <- model.matrix(form2, data = testing_set)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      testing_set_store[[studyid]] <- testing_set
    } else if(method == "average_prediction"){
      
      covariates_all <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      missingPatternTest <- findMissingPattern(testing_set, covariates_all)  
      testing_set <- testing_set %>% select(study, y, treat, all_of(missingPatternTest$without_sys_covariates)) %>% filter(complete.cases(.))
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        
        training_set_dummy <- training_set %>% filter(study == studyname2[i])
        missingPattern <- findMissingPattern(training_set_dummy, covariates_all)  
        
        training_set_dummy <- training_set_dummy %>% select(study, y, treat, all_of(missingPattern$without_sys_covariates))
        training_set_dummy <- createinteractions(training_set_dummy, missingPattern$without_sys_covariates)
        
        meth <- getCorrectMeth(training_set_dummy, missingPattern, "average_prediction")
        pred <- getCorrectPred(training_set_dummy, missingPattern, "average_prediction")
        
        imp <- mice(training_set_dummy, pred = pred, meth = meth)
        impc <- complete(imp, "long", include = "TRUE")
        imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
        
        prediction.dummy <- matrix(NA, nrow = dim(testing_set)[1], ncol = length(imp.list))
        
        #use covariates that are not systematically missing in both training and testing dataset
        without_sys_cov <- intersect(missingPatternTest$without_sys_covariates, missingPattern$without_sys_covariates)
        
        for(ii in 1:length(imp.list)){
          imp.dummy <- imp.list[[ii]]
          form <- as.formula(paste0("y ~ ", "(", paste(without_sys_cov, collapse= "+"), ") * treat" ))
          imp.model <- lm(form, data = imp.dummy)
          
          bb <- model.matrix(form, data = testing_set)
          prediction.dummy[,ii] <- bb %*% coef(imp.model)
        }
        prediction_store[,i] <- apply(prediction.dummy, 1, mean)
        
        print(paste0("2nd one; ", studyname2[i]))
      }
      predictions[[studyid]] <- apply(prediction_store, 1, mean)
      testing_set_store[[studyid]] <- testing_set
    }
  }
  
  list(predictions = predictions, testing_set_store = testing_set_store)

}


#### imputation related tools

getCorrectPred <- function(dummydata, missingPattern, method, outcome_name = "y", cluster_name = "study"){
  

  if(length(unique(dummydata$study)) == 1){
    
    with(missingPattern, {
      pred <- make.predictorMatrix(dummydata)
      pred[,] <- 0
      
      if(length(spor_covariates) != 0){
        pred[c(outcome_name, spor_covariates),] <- 1
        diag(pred) <- 0
        
        for(i in 1:length(spor_covariates)){
          pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
        }
        pred[c(outcome_name, spor_covariates), cluster_name] <- 0
      } else{
        
        pred[outcome_name,] <- 1
        diag(pred) <- 0
        pred[outcome_name, cluster_name] <- 0
        
      }
      pred
    })
  } else{
    
    with(missingPattern, {
      pred <- make.predictorMatrix(dummydata)
      pred[,] <- 0
      
      if(method == "naive"){
        if(length(spor_covariates) != 0){
          pred[c(outcome_name, spor_covariates),] <- 1
          pred[c(outcome_name, spor_covariates), "treat"] <- 2 
          diag(pred) <- 0
          
          for(i in 1:length(spor_covariates)){
            pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
          }
          pred[c(outcome_name, spor_covariates), cluster_name] <- -2
        } else{
          
          pred[outcome_name,] <- 1
          pred[outcome_name, "treat"] <- 2
          diag(pred) <- 0
          pred[outcome_name, cluster_name] <- -2
        }  
      } else if(method == "imputation"){
        
        if(length(spor_covariates) != 0){
          pred[c(outcome_name, spor_covariates),] <- 1
          pred[c(outcome_name, spor_covariates), "treat"] <- 2 
          
          for(i in 1:length(spor_covariates)){
            pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
          }
          pred[c(outcome_name, spor_covariates), cluster_name] <- -2
        }
        
        if(length(sys_covariates) != 0){
          pred[c(outcome_name, sys_covariates),] <- 1
          pred[c(outcome_name, sys_covariates), "treat"] <- 2 
          
          for(i in 1:length(sys_covariates)){
            pred[sys_covariates[i], paste0(sys_covariates[i], "treat")] <- 0
          }
          pred[c(outcome_name, sys_covariates), cluster_name] <- -2
        }
        diag(pred) <- 0
      }
      
      pred
    })
  }
}

getCorrectMeth <- function(dummydata, missingP, method, outcome_name = "y", multilevel.meth = "2l.pmm"){
  
  meth <- make.method(dummydata)
  if(length(unique(dummydata$study)) == 1){
    
    meth[paste0(missingP$without_sys_covariates, "treat")] <- paste0("~ I(", missingP$without_sys_covariates, " * treat)")

  } else{
    
    meth[outcome_name] <- multilevel.meth
    
    if(length(missingP$without_sys_covariates) != 0){
      meth[missingP$without_sys_covariates] <- "2l.2stage.norm"
    }
    
    if(length(missingP$without_sys_covariates) != 0){
      meth[missingP$without_sys_covariates] <- multilevel.meth
    }
    meth[paste0(missingP$without_sys_covariates, "treat")] <- paste0("~ I(", missingP$without_sys_covariates, " * treat)")
    
    if(method == "imputation"){
      if(length(missingP$without_sys_covariates) != 0){
        meth[paste0(missingP$without_sys_covariates, "treat")] <- paste0("~ I(", missingP$without_sys_covariates, " * treat)")  
      }
    }
  }
  return(meth)
}

findMissingPattern <- function(dummydata, covariates){
  
  if(length(unique(dummydata$study)) == 1){
    
    numberofNAs <- dummydata %>% select(all_of(covariates)) %>% summarize_all(~sum(is.na(.)))
    studysize = dim(dummydata)[1]
    sys_missing <- numberofNAs == studysize
    spor_missing <- numberofNAs < studysize & numberofNAs > 0
    sys_covariates <- colnames(sys_missing)[which(sys_missing == TRUE)]
    spor_covariates <- colnames(spor_missing)[which(spor_missing == TRUE)]
    without_sys_covariates <- colnames(sys_missing)[which(sys_missing == FALSE)]
  } else{
    
    numberofNAs <- dummydata %>% select(study, all_of(covariates)) %>% group_by(study) %>% summarize_all(~sum(is.na(.)))
    studysize <- dummydata %>% select(study, all_of(covariates)) %>% group_by(study) %>% summarize_all(~length(.))
    
    sys_missing <- apply(numberofNAs[,covariates] == studysize[,covariates], 2, any)
    sys_covariates <- names(sys_missing)[which(sys_missing == TRUE)]
    
    spor_missing <- apply(numberofNAs[,covariates], 2, sum) > 0 & !sys_missing
    spor_covariates <- names(spor_missing)[which(spor_missing == TRUE)]
    without_sys_covariates <- names(sys_missing)[which(sys_missing == FALSE)]
  }
  
  return(list(sys_missing = sys_missing, spor_missing = spor_missing, sys_covariates = sys_covariates, spor_covariates = spor_covariates, without_sys_covariates = without_sys_covariates))
}

createinteractions <- function(df, cov) {
  for(i in 1:length(cov)){
    varname <- paste0(cov[i], "treat")
    df[[varname]] <- NA  
  }
  df
}


     
  


################################functions to calculate performance #############
################################################################################

findPerformance <- function(testdata, predictions){
  
  nstudy <- length(testdata)
  performances <- matrix(NA, nrow = 3, ncol = nstudy)
  
  for(studyid in 1:nstudy){

    testing_set <- testdata[[studyid]]

    performances[1,studyid] <- findMSE(testing_set$y, predictions[[studyid]])
    performances[2,studyid] <- findMAE(testing_set$y, predictions[[studyid]])
    performances[3,studyid] <- findRsquared(testing_set$y, predictions[[studyid]])
    rownames(performances) <- c("MSE", "MAE", "Rsquared")
  }
  return(performances)
}


findRsquared <- function(y, pred){
  total <- (y - mean(y, na.rm = TRUE))^2
  tss <- sum(total[!is.na(total)])
  residual <- (y - pred)^2
  rss <- sum(residual[!is.na(residual)])
  rsquared <- 1 - rss/tss
  rsquared
}

findMAE <- function(y, pred){
  err_MAE <- abs(pred - y)
  err_MAE <- err_MAE[!is.na(err_MAE)]
  mean(err_MAE)
}

findMSE <- function(y, pred){
  err_mse <- (pred-y)^2
  err_mse <- err_mse[!is.na(err_mse)]
  mean(err_mse)
}


relabel.vec <- function(x, order)
{
  old.x <- x
  x <- rep(NA, length(old.x))
  for (i in seq(length(order))) x[old.x == order[i]] <- i #relabel studies in numerical order starting with one
  return(x)
}

findTestData <- function(simulated){
  
  testdata <- list()
  for(i in 1:length(unique(simulated$study))){
    testdata[[i]] <- simulated$dataset[simulated$study == i,]
  }
  return(testdata)
}
