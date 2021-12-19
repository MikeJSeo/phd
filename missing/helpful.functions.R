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
