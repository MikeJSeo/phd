############ Some helpful functions

createinteractions <- function(df, cov, treatmentname) {
  for(i in 1:length(cov)){
    varname <- paste0(cov[i], treatmentname)
    df[[varname]] <- NA  
  }
  df
}


findVarianceUsingRubinsRule <- function(prediction.dummy, variance.dummy){
  
  avg.prediction <- apply(prediction.dummy, 1, mean)
  summ <- 0
  for(iii in 1:5){
    summ <- summ + (prediction.dummy[,iii] - avg.prediction)^2
  }
  return(apply(variance.dummy, 1, mean) + (5 + 1)/ (5^2 - 5) * summ)
}


################################################################################
################################functions to calculate performance #############

findPerformance <- function(testdata_y, predictions, aggregation = ""){
  
  performances <- matrix(NA, nrow = 3, ncol = length(predictions))
  samplesize <- rep(NA, length(predictions))
  
  for(ii in 1:length(predictions)){
    
    performances[1, ii] <- findMSE(testdata_y[[ii]], predictions[[ii]])
    performances[2, ii] <- findMAE(testdata_y[[ii]], predictions[[ii]])
    performances[3, ii] <- findRsquared(testdata_y[[ii]], predictions[[ii]])    
    
    samplesize[ii] <- length(testdata_y[[ii]])
  }
  
  product_store <- prediction_store * precision_store
  precision_vec <- apply(samplesize, 1, sum)
  
  final_store <- sweep(product_store, 1, precision_vec, `/`)
  
  

  
  names(performances) <- c("MSE", "MAE","Rsquared")
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
