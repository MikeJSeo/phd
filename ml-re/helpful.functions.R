
findPredictionCBT <- function(crossdata, modelname){
  
  studyname <- unique(crossdata$study)
  nstudy <- length(studyname)
  predictions0 <- list() #training prediction
  predictions <- list() #testing prediction
  
  if(modelname == "randomforest_h2o"){
    h2o.init()
  }
  
  for(studyid in 1:nstudy){
    training_set <- crossdata %>% filter(study != studyname[studyid])
    testing_set <- crossdata %>% filter(study == studyname[studyid])

    if(modelname == "two-stage"){
      
      studyname2 <- unique(training_set$study)
      nstudy2 <- length(studyname2)
      
      varcov_store <- list()
      coefs_store <- matrix(NA, nrow = length(unique(training_set$study)), ncol = 10)
      
      for(i in 1:nstudy2){
        if(!studyname2[i] %in% c("Sheeber, 2012", "Pugh, 2016" )){
          dsi <- training_set %>% filter(study == studyname2[i])
          fit <- lm(y ~ (baseline + gender + age + relstat) * treat, data = dsi)
          coefs_store[i,] <- coefficients(fit)
          varcov_store[[i]] <- vcov(fit)  
        }
      }
      coefs_store <- coefs_store[complete.cases(coefs_store),]
      varcov_store <- plyr::compact(varcov_store)
      fit2 <- mvmeta(coefs_store ~ 1, S = varcov_store)
      
      bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% summary(fit2)$coefficients[,1]
    } else if(modelname == "average_prediction"){
      
    } else if(modelname == "lm"){
      lm.mod <- lm(y ~ (baseline + gender + age + relstat) * treat, data = training_set)
      bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% coef(lm.mod)
      
      bb0 <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = training_set)
      predictions0[[studyid]] <- bb0 %*% coef(lm.mod)
    } else if(modelname == "lmer"){
      lmer.mod <- lmer(y ~ (baseline + gender + age + relstat) * treat + (1 |study), data = training_set)
      bb <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = testing_set)
      predictions[[studyid]] <- bb %*% fixef(lmer.mod)
      
      bb0 <- model.matrix(y ~ (baseline + gender + age + relstat) * treat, data = training_set)
      predictions0[[studyid]] <- bb0 %*% fixef(lmer.mod)
    } else if(modelname == "randomforest_h2o"){
      
      training_set <-  as.matrix(training_set %>% select(-study) %>% mutate_if(is.factor, as.numeric))
      testing_set <-  as.matrix(testing_set %>% select(-study) %>% mutate_if(is.factor, as.numeric))  
      #  select(training_set, -c("study"))
      #testing_set <- select(testing_set, -c("study"))
      
      training_set <- as.h2o(training_set)
      testing_set <- as.h2o(testing_set)
      
      # best_rf <- h2o.randomForest(
      #   x = c("treat", "baseline", "gender", "age", "relstat"), 
      #   y = "y", training_frame = training_set, ntrees = 2000,
      #   max_depth = 20, min_rows = 1, sample_rate = 0.8, nfolds = 10,
      #   fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
      #   seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
      #   stopping_tolerance = 0
      # )
      # best_rf <- h2o.randomForest(
      #   x = c("treat", "baseline", "gender", "age", "relstat"), 
      #   y = "y", training_frame = training_set, nfolds = 10,
      #   fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
      #   seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
      #   stopping_tolerance = 0
      # )
      # best_rf <- h2o.randomForest(
      #   x = c("treat", "baseline", "gender", "age", "relstat"), 
      #   y = "y", training_frame = training_set, stopping_rounds = 50, stopping_metric = "RMSE",
      #   stopping_tolerance = 0
      # )
      
      best_glm <- h2o.glm(
        x = c("treat", "baseline", "gender", "age", "relstat"), 
        y = "y", training_frame = training_set,
        interaction_pairs = list(
          c("treat", "baseline"),
          c("treat", "gender"),
          c("treat", "age"),
          c("treat", "relstat")
        )
      )
      predictions[[studyid]] <- as.vector(h2o.predict(best_glm, newdata = testing_set))
      
      h2o.removeAll()

    } else if(modelname == "randomforest_nostudy"){
      
      training_set <- select(training_set, -c("study"))
      testing_set <- select(testing_set, -c("study"))
      
      rf1 <- ranger(
        y ~ .,
        data = training_set,
        mtry = floor(5/3),
        respect.unordered.factors = TRUE,
        seed = 123
      )
      predictions[[studyid]] <- predict(rf1, data = testing_set)$prediction
      predictions0[[studyid]] <- predict(rf1, data = training_set)$prediction
      
    } else if(modelname == "randomforest_withstudy"){
      
      rf1 <- ranger(
        y ~ .,
        data = training_set,
        mtry = floor(5 /3),
        respect.unordered.factors = TRUE,
        seed = 123
      )
      
      predictions_all <- list()
      predictions0_all <- list()
      for(i in 1:nstudy){
        if(studyid != studyname[i]){
          testing_set_changed <- testing_set %>% mutate(study = studyname[i])
          predictions_all[[i]] <- predict(rf1, data = testing_set_changed)$prediction
          
          training_set_changed <- training_set %>% mutate(study = studyname[i])
          predictions0_all[[i]] <- predict(rf1, data = training_set_changed)$prediction
        }
      }
      predictions_all[studyid] <- NULL
      predictions0_all[studyid] <- NULL
      
      predictions0[[studyid]] <- colMeans(do.call(rbind, predictions0_all))
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
      predictions_all[studyid] <- NULL
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
      
    } else if(modelname == "stacking"){
      
      #change study label
      
      #training_set <-  as.matrix(training_set %>% select(-study) %>% mutate_if(is.factor, as.numeric))
      training_set <-  as.matrix(training_set %>% mutate(study = factor(study)) %>% mutate_if(is.factor, as.numeric))
      #testing_set <-  as.matrix(testing_set %>% select(-study) %>% mutate_if (is.factor, as.numeric))
      testing_set <-  as.matrix(testing_set %>% mutate(study = factor(study)) %>% mutate_if(is.factor, as.numeric))

      training_set <- as.h2o(training_set)
      testing_set <- as.h2o(testing_set)
      
      best_glm <- h2o.glm(
        x = c("treat", "baseline", "gender", "age", "relstat"), 
        y = "y", training_frame = training_set,
        interaction_pairs = list(
          c("treat", "baseline"),
          c("treat", "gender"),
          c("treat", "age"),
          c("treat", "relstat")
        ), fold_column = "study",
        keep_cross_validation_predictions = TRUE
      )
      
      best_rf <- h2o.randomForest(
        x = c("treat", "baseline", "gender", "age", "relstat"),
        y = "y", training_frame = training_set, ntrees = 1000,
        max_depth = 20, min_rows = 1, sample_rate = 0.632, 
        fold_column = "study", keep_cross_validation_predictions = TRUE,
        seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
        stopping_tolerance = 0
      )
      
      best_gbm <- h2o.gbm(
        x = c("treat", "baseline", "gender", "age", "relstat"),
        y = "y", training_frame = training_set, ntrees = 5000, learn_rate = 0.01,
        max_depth = 7, min_rows = 5, sample_rate = 0.8,
        fold_column = "study", keep_cross_validation_predictions = TRUE,
        seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
        stopping_tolerance = 0  
      )
      
      ensemble_tree <- h2o.stackedEnsemble(
        x = c("treat", "baseline", "gender", "age", "relstat"),
        y = "y", training_frame = training_set, model_id = "my_tree_ensemble",
        base_models = list(best_glm, best_rf, best_gbm),
        metalearner_algorithm = "drf"
      )
      
      # ensemble_tree <- h2o.stackedEnsemble(
      #   x = X, y = Y, training_frame = train_h2o, model_id = "my_tree_ensemble",
      #   base_models = list(best_glm, best_rf, best_gbm, best_xgb),
      #   metalearner_algorithm = "drf"
      # )
      
      predictions[[studyid]] <- as.data.frame(h2o.predict(object = ensemble_tree, newdata = testing_set))
      h2o.removeAll()
    }
    print(paste0("finished: ", studyid))
  }
  return(list(predictions = predictions, predictions0 = predictions0))
}

find_stacked_result <- function(crossdata, Z_train, y_train, Z_test){

  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  
  for(studyid in 1:nstudy){
    Z <- Z_train[[studyid]]
    colnames(Z) <- 1:dim(Z)[2]
    y <- y_train[[studyid]]
    
    Z_t <- Z_test[[studyid]]
    colnames(Z_t) <- 1:dim(Z_t)[2]
    
    data_new <- as.data.frame(cbind(y = y, Z = Z))
    print(data_new)
    
    gbm1 <- gbm(
      formula = y ~ .,
      data = data_new,
      distribution = "gaussian",
      n.trees = 5000,
      shrinkage = 0.1, 
      interaction.depth = 3,
      n.minobsinnode = 10,
      cv.folds = 10
    )
    predictions[[studyid]] <- predict(gbm1, newdata = as.data.frame(Z_t))
  }
  return(predictions)
}

stacked_model_y <- function(crossdata){
  
  ystore <- list()
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  
  for(studyid in 1:nstudy){
    training_set <- crossdata %>% filter(study != studyname[studyid])
    ystore[[studyid]] <- training_set$y
  }
  return(ystore)
}


stacked_model_y_test <- function(crossdata){
  
  ystore <- list()
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  
  for(studyid in 1:nstudy){
    testing_set <- crossdata %>% filter(study == studyname[studyid])
    ystore[[studyid]] <- testing_set$y
  }
  return(ystore)
}



stacked_model <- function(pred){
  
  #matrix(unlist(pred0), ncol = length(pred0))
  prednew <- pred[[1]]
  for(i in 2:length(pred)){
    prednew <- mapply(cbind, prednew, pred[[i]])
  }
  return(prednew)
}

findPerformanceCBT <- function(crossdata, pred){
  
  nstudy <- length(unique(crossdata$study))
  studyname <- unique(crossdata$study)
  performances <- matrix(NA, nrow = 3, ncol = nstudy)
  
  for(studyid in 1:nstudy){
    
    testing_set <- crossdata[crossdata$study == studyname[studyid],]
    
    performances[1,studyid] <- findMSE(testing_set$y, pred[[studyid]])
    performances[2,studyid] <- findMAE(testing_set$y, pred[[studyid]])
    performances[3,studyid] <- findRsquared(testing_set$y, pred[[studyid]])
    rownames(performances) <- c("MSE", "MAE", "Rsquared")
  }
  return(performances)
}

########################################

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