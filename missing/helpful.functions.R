

###############################################################333
#### imputation related tools

getCorrectPred <- function(dummydata, missingPattern, method){
  
  interaction <- "treat" %in% colnames(dummydata)

  if(length(unique(dummydata$study)) == 1){
    
    with(missingPattern, {
      pred <- make.predictorMatrix(dummydata)
      pred[,] <- 0
      
      if(length(spor_covariates) != 0){
        pred[c("y", spor_covariates),] <- 1
        diag(pred) <- 0
        
        for(i in 1:length(spor_covariates)){
          pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
        }
        pred[c("y", spor_covariates), "study"] <- 0
      } else{
        
        pred["y",] <- 1
        diag(pred) <- 0
        pred["y", "study"] <- 0
        
      }
      pred
    })
  } else{
    
    with(missingPattern, {
      pred <- make.predictorMatrix(dummydata)
      pred[,] <- 0
      
      if(method == "naive"){
        if(length(spor_covariates) != 0){
          pred[c("y", spor_covariates),] <- 1
          pred[c("y", spor_covariates), "treat"] <- 2 
          diag(pred) <- 0
          
          for(i in 1:length(spor_covariates)){
            pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
          }
          pred[c("y", spor_covariates), "study"] <- -2
        } else{
          
          pred["y",] <- 1
          pred["y", "treat"] <- 2
          diag(pred) <- 0
          pred["y", "study"] <- -2
        }  
      } else if(method == "imputation"){
        
        if(length(spor_covariates) != 0){
          pred[c("y", spor_covariates),] <- 2
          pred[spor_covariates, "y"] <- 1
          
          if(interaction == TRUE){
            pred[c("y", spor_covariates), "treat"] <- 2   
            for(i in 1:length(spor_covariates)){
              pred[spor_covariates[i], paste0(spor_covariates[i], "treat")] <- 0
            }
          }

          pred[c("y", spor_covariates), "study"] <- -2
        }
        
        if(length(sys_covariates) != 0){
          pred[c("y", sys_covariates),] <- 2
          pred[sys_covariates, "y"] <- 1
          
          if(interaction == TRUE){
            pred[c("y", sys_covariates), "treat"] <- 2 
            for(i in 1:length(sys_covariates)){
              pred[sys_covariates[i], paste0(sys_covariates[i], "treat")] <- 0
            }
          }
          pred[c("y", sys_covariates), "study"] <- -2
        }
        diag(pred) <- 0
      }
      
      pred
    })
  }
}

getCorrectMeth <- function(dummydata, missingP, method, type_of_var = NULL){
  
  interactionData <- "treat" %in% colnames(dummydata)
  
  meth <- make.method(dummydata)
  if(length(unique(dummydata$study)) == 1){
    
    meth[paste0(missingP$without_sys_covariates, "treat")] <- paste0("~ I(as.numeric(as.character(", missingP$without_sys_covariates, ")) * treat)")

    if(meth["y"] != ""){
      meth["y"] <- "pmm"
    }
    
    if(length(missingP$spor_covariates) != 0){
      meth[missingP$spor_covariates] <- "pmm"
    }
    
  } else{
    
    if(meth["y"] != ""){
      meth["y"] <- "2l.pmm" #assume outcome data is not systematically missing
    }

    if(length(missingP$spor_covariates) != 0){
      
      for(i in 1:length(missingP$spor_covariates)){
        meth[missingP$spor_covariates[i]] <- "2l.pmm"
      }
    }
    
    if(length(missingP$sys_covariates) != 0){
      
      for(i in 1:length(missingP$sys_covariates)){
        if(type_of_var[missingP$sys_covariates[i]] == "continuous"){
          meth[missingP$sys_covariates[i]] <- "2l.2stage.norm"
          #meth[missingP$sys_covariates[i]] <- "2l.glm.norm"
        } else if(type_of_var[missingP$sys_covariates[i]] == "binary"){
          meth[missingP$sys_covariates[i]] <- "2l.2stage.bin" 
          #meth[missingP$sys_covariates[i]] <- "2l.glm.bin"
        } else if(type_of_var[missingP$sys_covariates[i]] == "count"){
          meth[missingP$sys_covariates[i]] <- "2l.2stage.pois"
          #meth[missingP$sys_covariates[i]] <- "2l.glm.pois"
        }
      }
    }
    
    if(length(missingP$spor_covariates) != 0){
      meth[missingP$spor_covariates] <- "2l.pmm"
    }
    
    if(interactionData == TRUE){
      meth[paste0(missingP$covariates, "treat")] <- paste0("~ I(as.numeric(as.character(", missingP$covariates, ")) * treat)")
    }
  }
  return(meth)
}


createinteractions <- function(df, cov) {
  for(i in 1:length(cov)){
    varname <- paste0(cov[i], "treat")
    df[[varname]] <- NA  
  }
  df
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
  
  return(list(sys_missing = sys_missing, spor_missing = spor_missing, sys_covariates = sys_covariates, spor_covariates = spor_covariates, without_sys_covariates = without_sys_covariates, covariates = covariates))
}






################################functions to calculate performance #############
################################################################################

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

##########################################################################
###################### additional functions ##############################

relabel.vec <- function(x, order)
{
  old.x <- x
  x <- rep(NA, length(old.x))
  for (i in seq(length(order))) x[old.x == order[i]] <- i #relabel studies in numerical order starting with one
  return(x)
}
