
findPredictionCBT <- function(crossdata, modelname){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  predictions <- list()
  
  for(studyid in 1:nstudy){
    training_set <- crossdata %>% filter(study != studyname[studyid])
    testing_set <- crossdata %>% filter(study == studyname[studyid])

    if(modelname == "lm"){
      lm.mod <- lm(y ~ (baseline + gender + age + relstat) * treat, data = training_set)
      bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% coef(lm.mod)
    } else if(modelname == "lmer"){
      lmer.mod <- lmer(y ~ (baseline + gender + age + relstat) * treat + (1 |study), data = training_set)
      bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% fixef(lmer.mod)
    } else if(modelname == "randomforest_nostudy"){
      
      training_set <- select(training_set, -c("study"))
      testing_set <- select(testing_set, -c("study"))
      
      rf1 <- ranger(
        y ~ .,
        data = training_set,
        mtry = floor(n_features/3),
        respect.unordered.factors = TRUE,
        seed = 123
      )
      predictions[[studyid]] <- predict(rf1, data = testing_set)$prediction
    } else if(modelname == "randomforest_withstudy"){
      
      rf1 <- ranger(
        y ~ .,
        data = training_set,
        mtry = floor(n_features/3),
        respect.unordered.factors = TRUE,
        seed = 123
      )
      
      predictions_all <- list()
      for(i in 1:nstudy){
        if(studyid != studyname[i]){
          testing_set <- testing_set %>% mutate(study = studyname[i])
          predictions_all[[i]] <- predict(rf1, data = testing_set)$prediction
        }
      }
      predictions_all[studyid] <- NULL
      predictions[[studyid]] <- colMeans(do.call(rbind, predictions_all))
    } else if(modelname == "gbm_nostudy"){
      
      training_set <- select(training_set, -c("study"))
      testing_set <- select(testing_set, -c("study"))
      
      gbm1 <- gbm(
        formula = y ~ .,
        data = training_set,
        distribution = "gaussian",
        n.trees = 5000,
        shrinkage = 0.1, 
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
      )
      predictions[[studyid]] <- predict(gbm1, newdata = testing_set)
    } else if(modelname == "gbm_withstudy"){
      
      gbm1 <- gbm(
        formula = y ~ .,
        data = training_set,
        distribution = "gaussian",
        n.trees = 5000,
        shrinkage = 0.1, 
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
      )
      
      predictions_all <- list()
      for(i in 1:nstudy){
        if(studyid != studyname[i]){
          testing_set <- testing_set %>% mutate(study = studyname[i])
          predictions_all[[i]] <- predict(gbm1, newdata = testing_set)
        }
      }
      predictions[studyid] <- NULL
      predictions[[studyid]] <- colMeans(do.call(rbind, predictions_all))
    } else if(modelname == "keras"){
      model <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = dim(data)[2] - 2) %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_dense(units = 1) %>%
        
        compile(
          loss = "mse",
          optimizer = optimizer_rmsprop(),
          metrics = list("mean_squared_error")
        )
      
      x_train <- as.matrix(training_set %>% select(-study, -y) %>% mutate_if(is.factor, as.numeric)) 
      y_train <- as.numeric(training_set$y)
      
      fit1 <- model %>% 
        fit(
          x = x_train,
          y = y_train
        )
      
      predictions[[studyid]] <- predict(model, x = as.matrix(testing_set %>% select(-study, -y) %>% mutate_if(is.factor, as.numeric)) )
      
    }
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}

findPerformanceCBT <- function(crossdata, predictions){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  performances <- matrix(NA, nrow = 3, ncol = nstudy)
  
  for(studyid in 1:nstudy){
    
    testing_set <- crossdata[crossdata$study == studyname[studyid],]
    
    performances[1,studyid] <- findMSE(testing_set$y, predictions[[studyid]])
    performances[2,studyid] <- findMAE(testing_set$y, predictions[[studyid]])
    performances[3,studyid] <- findRsquared(testing_set$y, predictions[[studyid]])
    rownames(performances) <- c("MSE", "MAE", "Rsquared")
  }
  return(performances)
}



findPrediction <- function(crossdata, modelname){

  nstudy <- length(unique(crossdata$study))
  predictions <- list()
  
  for(studyid in 1:nstudy){
    training_set <- crossdata[crossdata$study != studyid,]
    testing_set <- crossdata[crossdata$study == studyid,]
    
    if(modelname == "randomforest_nostudy"){
      
      training_set <- select(training_set, -c("study"))
      testing_set <- select(testing_set, -c("study"))
      
      rf1 <- ranger(
         y ~ .,
         data = training_set,
         mtry = floor(n_features/3),
         respect.unordered.factors = TRUE,
         seed = 123
      )
      predictions[[studyid]] <- predict(rf1, data = testing_set)$prediction
    } else if(modelname == "randomforest_withstudy"){
      
      rf1 <- ranger(
        y ~ .,
        data = training_set,
        mtry = floor(n_features/3),
        respect.unordered.factors = TRUE,
        seed = 123
      )
      
      predictions_all <- list()
      for(i in 1:nstudy){
        if(studyid != i){
          testing_set <- testing_set %>% mutate(study = i)
          predictions_all[[i]] <- predict(rf1, data = testing_set)$prediction
        }
      }
      predictions_all[studyid] <- NULL
      predictions[[studyid]] <- colMeans(do.call(rbind, predictions_all))
    } else if(modelname == "lm"){
      lm.mod <- lm(y ~ (x1 + x2 + x3 + x4) * treat, data = training_set)
      bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% coef(lm.mod)
      
    } else if(modelname == "lm2"){
      lm.mod <- lm(y ~ (x1 + x2 + x3 + x4) * treat, data = training_set)
      
      predictions_all <- list()
      for(i in 1:nstudy){
        if(studyid != i){
          testing_set <- testing_set %>% mutate(study = i)
          predictions_all[[i]] <- predict(lm.mod, newdata = testing_set)
        }
      }
      predictions_all[studyid] <- NULL
      predictions[[studyid]] <- colMeans(do.call(rbind, predictions_all))
      
    } else if(modelname == "lmer"){
      lmer.mod <- lmer(y ~ (x1 + x2 + x3 + x4) * treat + (1 |study), data = training_set)
      bb <- model.matrix(y ~ (x1 + x2 + x3 + x4) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% fixef(lmer.mod)
    }
    
    print(paste0("finished: ", studyid))
  }
  return(predictions)
}

findPerformance <- function(crossdata, predictions){
  
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