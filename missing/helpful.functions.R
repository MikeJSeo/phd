# General functions shared for both simulations and real data example

##################################################################
#### imputation related functions

# Find missing data pattern in a given data i.e. whether they are systematically missing or sporadically missing
findMissingPattern <- function(dataset, covariates, studyname = "study"){
  
  if(length(unique(dataset[,studyname])) == 1){
    
    missingcount <- dataset %>% select(all_of(covariates))%>% summarize_all(~sum(is.na(.)))
    missingn <- dataset %>% select(all_of(covariates))  %>% summarise_all(~length(.))
    missingpercent <- dataset %>% select(all_of(covariates)) %>% summarise_all(~round(sum(is.na(.)/length(.))*100))
    
    numberofNAs <- dataset %>% select(all_of(covariates)) %>% summarize_all(~sum(is.na(.)))
    studysize = dim(dataset)[1]
    
    sys_missing <- numberofNAs == studysize
    spor_missing <- numberofNAs < studysize & numberofNAs > 0
    sys_covariates <- colnames(sys_missing)[which(sys_missing == TRUE)]
    spor_covariates <- colnames(spor_missing)[which(spor_missing == TRUE)]
    without_sys_covariates <- colnames(sys_missing)[which(sys_missing == FALSE)]
    
  } else{
    
    missingcount <- dataset %>% select(all_of(c(studyname, covariates))) %>% group_by(across(all_of(studyname))) %>% summarize_all(~sum(is.na(.)))
    missingn <- dataset %>% select(all_of(c(studyname, covariates))) %>% group_by(across(all_of(studyname))) %>% summarise_all(~length(.))
    missingpercent <- dataset %>% select(all_of(c(studyname, covariates))) %>% group_by(across(all_of(studyname))) %>% summarise_all(~round(sum(is.na(.)/length(.))*100))
    
    numberofNAs <- dataset %>% select(all_of(c(studyname, covariates))) %>% group_by(across(all_of(studyname))) %>% summarize_all(~sum(is.na(.)))
    studysize <- dataset %>% select(all_of(c(studyname, covariates))) %>% group_by(across(all_of(studyname))) %>% summarize_all(~length(.))
    
    sys_missing <- apply(numberofNAs[,covariates] == studysize[,covariates], 2, any)
    sys_covariates <- names(sys_missing)[which(sys_missing == TRUE)]
    spor_missing <- apply(numberofNAs[,covariates], 2, sum) > 0 & !sys_missing
    spor_covariates <- names(spor_missing)[which(spor_missing == TRUE)]
    without_sys_covariates <- names(sys_missing)[which(sys_missing == FALSE)]
  }
  
  return(list(missingcount = missingcount, missingpercent = missingpercent, sys_missing = sys_missing, spor_missing = spor_missing, sys_covariates = sys_covariates, spor_covariates = spor_covariates, without_sys_covariates = without_sys_covariates, covariates = covariates))
}


#Find correct imputation method to be used in the mice package

getCorrectMeth <- function(dataset, missingPattern, studyname = "study", treatmentname = "treat", outcomename = "y", interaction = TRUE, typeofvar = NULL){
  
  
  meth <- make.method(dataset)
  if(length(unique(dataset[,studyname])) == 1){
    
    meth[paste0(missingPattern$without_sys_covariates, treatmentname)] <- paste0("~ I(as.numeric(as.character(", missingPattern$without_sys_covariates, ")) *", treatmentname, ")")
    
    if(meth[outcomename] != ""){
      meth[outcomename] <- "pmm"
    }
    
    if(length(missingPattern$spor_covariates) != 0){
      meth[missingPattern$spor_covariates] <- "pmm"
    }
    
  } else{
    
    if(length(missingPattern$sys_covariates) != 0 & is.null(typeofvar)){
      stop("typeofvar needs to be specified to denote the type of the systematically missing variables")
    } 
    
    if(meth[outcomename] != ""){
      meth[outcomename] <- "2l.pmm" #assume outcome data is not systematically missing
    }
    
    if(length(missingPattern$spor_covariates) != 0){
      meth[missingPattern$spor_covariates] <- "2l.pmm"
    }
    
    if(length(missingPattern$sys_covariates) != 0){
      
      for(i in 1:length(missingPattern$sys_covariates)){
        if(typeofvar[missingPattern$sys_covariates[i]] == "continuous"){
          meth[missingPattern$sys_covariates[i]] <- "2l.2stage.norm"
        } else if(typeofvar[missingPattern$sys_covariates[i]] == "binary"){
          meth[missingPattern$sys_covariates[i]] <- "2l.2stage.bin"
        } else if(typeofvar[missingPattern$sys_covariates[i]] == "count"){
          meth[missingPattern$sys_covariates[i]] <- "2l.2stage.pois"
        }
      }
    }
    
    if(interaction == TRUE){
      meth[paste0(missingPattern$covariates, "treat")] <- paste0("~ I(as.numeric(as.character(", missingPattern$covariates, ")) *", treatmentname,  ")")
    }
  }
  return(meth)
}


# Find correct imputation prediction matrix to be used in the mice package

getCorrectPred <- function(dataset, missingPattern, studyname = "study", treatmentname = "treat", outcomename = "y", interaction = TRUE){
  
  if(length(unique(dataset[,studyname])) == 1){
    
    # Case when there are only one cluster/study
    with(missingPattern, {
      pred <- make.predictorMatrix(dataset)
      pred[,] <- 0
      
      #outcome imputation
      pred[outcomename,] <- 1
      pred[outcomename, studyname] <- 0
      
      # sporadically missing predictors imputation
      if(length(spor_covariates) != 0){
        pred[spor_covariates,] <- 1
        
        for(i in 1:length(spor_covariates)){
          pred[spor_covariates[i], paste0(spor_covariates[i], treatmentname)] <- 0
        }
        pred[spor_covariates, studyname] <- 0
      }
      diag(pred) <- 0
      pred
    })
  } else{
    
    # Case when there are multiple clusters/studies
    with(missingPattern, {
      pred <- make.predictorMatrix(dataset)
      pred[,] <- 0
      
      # outcome imputation
      pred[outcomename,] <- 1
      
      if(treatmentname %in% colnames(pred)){
        pred[outcomename, treatmentname] <- 2
      }
      pred[outcomename, studyname] <- -2
      
      # sporadically missing predictors imputation
      if(length(spor_covariates) != 0){
        pred[spor_covariates,] <- 1
        
        if(treatmentname %in% colnames(pred)){
          pred[spor_covariates, treatmentname] <- 2
        }
        
        if(interaction == TRUE){
          for(i in 1:length(spor_covariates)){
            pred[spor_covariates[i], paste0(spor_covariates[i], treatmentname)] <- 0
          }
        }
        pred[spor_covariates, studyname] <- -2
      }
      
      # systematically missing predictors imputation
      if(length(sys_covariates) != 0){
        pred[c(outcomename, sys_covariates),] <- 1
        pred[sys_covariates, outcomename] <- 1
        
        if(treatmentname %in% colnames(pred)){
          pred[c(outcomename, sys_covariates), treatmentname] <- 2
        }
        
        if(interaction == TRUE){
          for(i in 1:length(sys_covariates)){
            pred[sys_covariates[i], paste0(sys_covariates[i], treatmentname)] <- 0
          }
        }
        pred[c(outcomename, sys_covariates), studyname] <- -2
      }
      diag(pred) <- 0
      pred
    })
  }
}



#################### some other helpful functions
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
