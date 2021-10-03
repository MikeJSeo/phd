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


