# General functions shared for both simulations and real data example

##################################################################
#### some helpful functions
createinteractions <- function(df, cov) {
  for(i in 1:length(cov)){
    varname <- paste0(cov[i], "treat")
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

relabel.vec <- function(x, order)
{
  old.x <- x
  x <- rep(NA, length(old.x))
  for (i in seq(length(order))) x[old.x == order[i]] <- i #relabel studies in numerical order starting with one
  return(x)
}

################################################################################
################################functions to calculate performance #############

findPerformance <- function(testdata_y, predictions){
  
  performances <- rep(NA, 3)
  
  performances[1] <- findMSE(testdata_y, predictions)
  performances[2] <- findMAE(testdata_y, predictions)
  performances[3] <- findRsquared(testdata_y, predictions)
  
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
