############ Some helpful functions

# function to name treatment-covariate terms
createinteractions <- function(df, cov, treatmentname) {
  for(i in 1:length(cov)){
    varname <- paste0(cov[i], treatmentname)
    df[[varname]] <- NA  
  }
  df
}

# Use Rubin's rule to calculate the variance
findVarianceUsingRubinsRule <- function(prediction.dummy, variance.dummy){
  
  avg.prediction <- apply(prediction.dummy, 1, mean)
  summ <- 0
  
  mm <- dim(prediction.dummy)[2]
  for(iii in 1:mm){
    summ <- summ + (prediction.dummy[,iii] - avg.prediction)^2
  }
  return(apply(variance.dummy, 1, mean) + (mm + 1)/ (mm * (mm - 1)) * summ)
}

# output testing output in a list format divided by each study; used to calculate performance measures
findTestingOutcome <- function(dataset){
  
  nstudy <- length(unique(dataset$study))
  
  testingdata <- list()
  
  for(studyid in 1:nstudy){
    
    testing_set <- dataset[dataset$study == studyid,]
    testingdata[[studyid]] <- testing_set$y
  }
  return(testingdata)
}

# Find bootstrap samples index accounting for study level 
findBootstrapSample <- function(dataset){
  
  rownames_data <- rownames(dataset)
  nstudy <- length(unique(dataset$study))
  final_vector <- c()
  
  for(i in 1:nstudy){
    
    studylevel_rownames <- rownames_data[dataset$study == i]
    index <- sample(1:length(studylevel_rownames), replace = TRUE)
    index <- index[order(index)]
    
    final_vector <- c(final_vector, studylevel_rownames[index])
  }
  return(final_vector)
}




findPerformance <- function(testoutcome = NULL, predictions = NULL, aggregation = "weighted"){
  
  if(is.null(testoutcome) | is.null(predictions)){
    stop("testoutcome or predictions is not specified")
  }
  
  if(!aggregation %in% c("weighted", "simple", "ignore")){
    stop("aggregation should be either weighted, simple or ignore")
  }
  
  if(aggregation == "ignore"){
    
    testoutcome_unlisted <- unlist(testoutcome)
    predictions_unlisted <- unlist(predictions)
    
    performancemetrics <- rep(NA, 3)
    performancemetrics[1] <- findMSE(testoutcome_unlisted, predictions_unlisted)
    performancemetrics[2] <- findMAE(testoutcome_unlisted, predictions_unlisted)
    performancemetrics[3] <- findRsquared(testoutcome_unlisted, predictions_unlisted)
    
    names(performancemetrics) <- c("MSE", "MAE","Rsquared")
    
  } else if(aggregation %in% c("weighted", "simple")){
    
    performances <- matrix(NA, nrow = 3, ncol = length(predictions))
    samplesize <- rep(NA, length(predictions))
    
    for(ii in 1:length(predictions)){
      
      performances[1, ii] <- findMSE(testoutcome[[ii]], predictions[[ii]])
      performances[2, ii] <- findMAE(testoutcome[[ii]], predictions[[ii]])
      performances[3, ii] <- findRsquared(testoutcome[[ii]], predictions[[ii]])
      
      samplesize[ii] <- length(testoutcome[[ii]])
    }
    
    if(aggregation == "weighted"){
      product_store <- performances * samplesize
      samplesize_sum <- sum(samplesize)
      
      final_store <- product_store/samplesize_sum
      performancemetrics <- apply(final_store, 1, sum)  
    } else if(aggregation == "simple"){
      performancemetrics <- apply(performances, 1, mean)
    }
    names(performancemetrics) <- c("MSE", "MAE", "Rsquared")
  }
  
  return(performancemetrics)
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


