
# generate simulation data
generate_data <- function(){
  
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
  

###############################################################
####### cross validation for simulated data ###################

findPrediction <- function(crossdata, method){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  
  # find studies that are systematically missing in x2
  sys_studies <- crossdata %>% group_by(study) %>% summarise(na_count = sum(is.na(x2)), .groups = "drop" ) %>% filter(na_count == 1500) %>% pull(study)
  
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
    
    if(method == "naive"){
      
      training_set <- training_set %>% select(-x2) %>% mutate(x1.treat = NA, x3.treat = NA, x4.treat = NA)
      testing_set <- testing_set %>% select(-x2)
      
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
      prediction.dummy <- matrix(NA, nrow = dim(imp.list[[1]])[1], ncol = length(imp.list))
      
      for(ii in 1:length(imp.list)){
        imp.dummy <- imp.list[[ii]]
        imp.model <- lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study), data = imp.dummy)
        bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = imp.dummy)
        prediction.dummy[,ii] <- bb %*% fixef(imp.model)
      }
      predictions[[studyid]] <- apply(prediction.dummy, 1, mean)
      
      #fit <- with(imp, lmer(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat + (1| study)))
      #t(sapply(fit$analyses, fixef))
      #coef_fit <- summary(pool(fit))
      #bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = testing_set)
      #predictions[[studyid]] <- bb %*% coef_fit[,"estimate"]
    } else if(method == "average_predictions"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      prediction_store <- matrix(NA, dim(testing_set)[1], nstudy2)
      
      for(i in 1:nstudy2){
        if(studyname2[i] %in% sys_studies || studyname[i] %in% sys_studies){
          
          training_set_dummy <- training_set %>% select(-x2) 
          testing_set_dummy <- testing_set %>% select(-x2)
          
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
          prediction.dummy <- matrix(NA, nrow = dim(imp.list[[1]])[1], ncol = length(imp.list))
          
          for(ii in 1:length(imp.list)){
            imp.dummy <- imp.list[[ii]]
            imp.model <- lm(y ~ 1 + x1 + x3 + x4 + treat + x1*treat + x3*treat + x4*treat, data = imp.dummy)
            bb <- model.matrix(y ~ (x1 + x3 + x4) * treat, data = imp.dummy)
            prediction.dummy[,ii] <- bb %*% coef(imp.model)
          }
          prediction_store[,i] <- apply(prediction.dummy, 1, mean)
          
          print(paste0("1st one; ", studyname[i]))

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
          prediction.dummy <- matrix(NA, nrow = dim(imp.list[[1]])[1], ncol = length(imp.list))
          
          for(ii in 1:length(imp.list)){
            imp.dummy <- imp.list[[ii]]
            imp.model <- lm(y ~ 1 + x1 + x2 + x3 + x4 + treat + x1*treat + x2*treat+ x3*treat + x4*treat, data = imp.dummy)
            bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = imp.dummy)
            prediction.dummy[,ii] <- bb %*% coef(imp.model)
          }
          prediction_store[,i] <- apply(prediction.dummy, 1, mean)
          
          print(paste0("2nd one; ", studyname2[i]))
        }
      }
      predictions[[studyid]] <- apply(prediction_store, 1, mean)
    } 
    
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}


     
  


################################functions to calculate performance #############
################################################################################

findPerformance <- function(crossdata, predictions){
  
  crossdata <- crossdata[complete.cases(crossdata),] #calculate performance only on complete data
  nstudy <- length(unique(crossdata$study))
  performances <- matrix(NA, nrow = 3, ncol = nstudy)
  
  for(studyid in 1:nstudy){

    testing_set <- crossdata[crossdata$study == studyid,]

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