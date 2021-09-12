library(caret)
library(kernlab)
library(gridExtra)
library(dplyr)
library(tidyr)
library(lme4)
library(recipes)

setwd("C:/Users/ms19g661/Desktop")
load("sund data")

categorical.variables <- c("sex", "work_condition", "marriage_condition", "physical_illness", paste0("primemd_q", 1:9), paste0("phq9_q", 1:9, "_1"),
                           paste0("w1_fibser_q", 1:4), paste0("bdi_q", 1:21, "_1"), paste0("phq9_q", 1:9,"_3"), paste0("w3_fibser_q", 1:4), paste0("bdi_q", 1:21, "_3")                         )
continuous.variables <- c("age", "educatenumber", "depression_age", "depression_episode_number", "episode_months")
dat.final <- dat1[, c(categorical.variables, continuous.variables)]
dat.final <- cbind( dat[, c("medicalid", "allocation_resultc")],  dat1[, c(categorical.variables, continuous.variables)])
dat.final$y <- dat$w9_phq9
dat.final[, c("medicalid","allocation_resultc", categorical.variables)] <- lapply(dat.final[, c("medicalid","allocation_resultc", categorical.variables)], as.factor)

#dat.final %>% summarize_all(~sum(is.na(.)))
dat.final <- dat.final %>% drop_na()

dat.final2 <- dat.final %>% recipe(y ~.) %>%
  step_dummy("allocation_resultc", all_of(categorical.variables)) %>%
  prep() %>%
  bake(dat.final)

dat.final2 <- dat.final2 %>% rename(clinic = medicalid) %>% mutate(treatment = dat.final$allocation_resultc)


###############################################


crossvalidate <- function(crossdata, modelname){
  
  nclinic <- length(unique(crossdata$clinic))
  performances <- matrix(NA, nrow = 3, ncol = nclinic)
  medicalid_names <- unique(crossdata$clinic)
  
  for(ii in 1:nclinic){
    
    training_set <- crossdata %>% filter(clinic != medicalid_names[ii])
    testing_set <- crossdata %>% filter(clinic == medicalid_names[ii])
    
    if(modelname == "lmer"){
      
      training_set <- training_set %>%  select(- c("treatment"))
      testing_set <- testing_set %>%  select(- c("treatment"))
      
      lmerfit <- lmer(y ~ . - clinic + (1|clinic), data = training_set)
      bb <- model.matrix(y ~ . - clinic, data = testing_set)
      prediction <- bb %*% fixef(lmerfit)
    } else if(modelname == "svm seperate"){
      
      treatment_dummy <- training_set$treatment
      training_set <- training_set %>% select(-c("treatment", grep("allocation_resultc", colnames(training_set), value = TRUE )))
      training_set.t1 <- training_set[treatment_dummy == 1,]
      training_set.t2 <- training_set[treatment_dummy == 2,]
      training_set.t3 <- training_set[treatment_dummy == 3,]
      
      treatment_test_dummy <- testing_set$treatment
      testing_set <- testing_set %>% select(-c("treatment", grep("allocation_resultc", colnames(training_set), value = TRUE )))
      testing_set.t1 <- testing_set[treatment_test_dummy == 1,]
      testing_set.t2 <- testing_set[treatment_test_dummy == 2,]
      testing_set.t3 <- testing_set[treatment_test_dummy == 3,]
      
      internalexternal <- list()
      for(i in 1:length(unique(training_set.t1$clinic))){
        ind <- 1:length(training_set.t1$clinic)
        ind <- ind[training_set.t1$clinic != unique(training_set.t1$clinic)[i]]
        internalexternal[[i]] <- ind
      }
      trctrl <- trainControl(index = internalexternal, method = "cv")
      
      training_set.t1 <- training_set.t1 %>%  select(- c("clinic"))
      svm_Radial.t1 <- train(y ~., data = training_set.t1, method = "svmRadial", trControl=trctrl, scale = FALSE, tuneLength = 20)
      
      internalexternal <- list()
      for(i in 1:length(unique(training_set.t2$clinic))){
        ind <- 1:length(training_set.t2$clinic)
        ind <- ind[training_set.t2$clinic != unique(training_set.t2$clinic)[i]]
        internalexternal[[i]] <- ind
      }
      trctrl <- trainControl(index = internalexternal, method = "cv")
      
      training_set.t2 <- training_set.t2 %>%  select(- c("clinic"))
      svm_Radial.t2<- train(y ~., data = training_set.t2, method = "svmRadial",trControl=trctrl, scale = FALSE, tuneLength = 20)

      internalexternal <- list()
      for(i in 1:length(unique(training_set.t3$clinic))){
        ind <- 1:length(training_set.t3$clinic)
        ind <- ind[training_set.t3$clinic != unique(training_set.t3$clinic)[i]]
        internalexternal[[i]] <- ind
      }
      trctrl <- trainControl(index = internalexternal, method = "cv")
      
      training_set.t3 <- training_set.t3 %>%  select(- c("clinic"))
      svm_Radial.t3<- train(y ~., data = training_set.t3, method = "svmRadial",trControl=trctrl, scale = FALSE, tuneLength = 20)

      y.in.1<- predict(svm_Radial.t1, newdata = testing_set[treatment_test_dummy == 1,])
      y.in.2<- predict(svm_Radial.t2, newdata = testing_set[treatment_test_dummy == 2,])
      y.in.3<- predict(svm_Radial.t3, newdata = testing_set[treatment_test_dummy == 3,])
      
      prediction <- rep(NA, dim(testing_set)[1])
      prediction[treatment_test_dummy == 1] <- y.in.1
      prediction[treatment_test_dummy == 2] <- y.in.2
      prediction[treatment_test_dummy == 3] <- y.in.3
    } else if(modelname == "svm together"){
      
      training_set <- training_set %>% select(-c("treatment"))
      testing_set <- testing_set %>% select(-c("treatment"))
      
      internalexternal <- list()
      for(i in 1:length(unique(training_set$clinic))){
        ind <- 1:length(training_set$clinic)
        ind <- ind[training_set$clinic != unique(training_set$clinic)[i]]
        internalexternal[[i]] <- ind
      }
      trctrl <- trainControl(index = internalexternal, method = "cv")
      
      training_set <- training_set %>%  select(- c("clinic"))
      svm_Radial <- train(y ~., data = training_set, method = "svmRadial", trControl=trctrl, scale = FALSE, tuneLength = 20)
      
      prediction <- predict(svm_Radial, newdata = testing_set)

    }
    
    performances[1,ii] <- findMSE(testing_set$y, prediction)
    performances[2,ii] <- findMAE(testing_set$y, prediction)
    performances[3,ii] <- findRsquared(testing_set$y, prediction)
  }
  
  return(performances)
}

findRsquared <- function(y, pred){
  
  total <- (y - mean(y, na.rm = TRUE))^2
  tss <- sum(total[!is.na(total)])
  residual <- (y - pred)^2
  rss<- sum(residual[!is.na(residual)])
  rsquared <- 1 - rss/tss
  rsquared
}

findMAE <- function(y, pred){
  
  err_MAE<- abs(pred - y)
  err_MAE <- err_MAE[!is.na(err_MAE)]
  mean(err_MAE)
}

findMSE <- function(y, pred){
  
  err_mse <- (pred - y)^2
  err_mse <- err_mse[!is.na(err_mse)]
  mean(err_mse)
}

######################################
#cross-validation

svm_together <- crossvalidate(dat.final2, "svm together")
lmer_cross <- crossvalidate(dat.final2, "lmer")
svm_seperate_cross <- crossvalidate(dat.final2, "svm seperate")

apply(lmer_cross, 1, mean)
apply(svm_seperate_cross, 1, mean)
apply(svm_together, 1, mean)

#############################################
#internal-validation
#lmer
dat.final3 <- dat.final2 %>% select(- c("treatment"))

lmerfit <- lmer(y ~ . - clinic + (1|clinic), data = dat.final3)
bb <- model.matrix(y ~ . - clinic, data = dat.final3)
prediction <- bb %*% fixef(lmerfit)

findMSE(dat.final3$y, prediction)
findMAE(dat.final3$y, prediction)
findRsquared(dat.final3$y, prediction)

#fit the SVMs
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

treatment_dummy <- dat.final2$treatment
dat.final3.t1 <- dat.final3[treatment_dummy == 1,]
dat.final3.t2 <- dat.final3[treatment_dummy == 2,]
dat.final3.t3 <- dat.final3[treatment_dummy == 3,]

svm_Radial.t1 <- train(y ~. , data = dat.final3.t1, method = "svmRadial", trControl=trctrl, tuneLength = 20, selectionFunction ="min", scale = FALSE)
svm_Radial.t2 <- train(y ~. , data = dat.final3.t2, method = "svmRadial", trControl=trctrl, tuneLength = 20, selectionFunction ="min", scale = FALSE)
svm_Radial.t3 <- train(y ~. , data = dat.final3.t3, method = "svmRadial", trControl=trctrl, tuneLength = 20, selectionFunction ="min", scale = FALSE)

y.in.1<- predict(svm_Radial.t1, newdata = dat.final3.t1)
y.in.2<- predict(svm_Radial.t2, newdata = dat.final3.t2)
y.in.3<- predict(svm_Radial.t3, newdata = dat.final3.t3)

prediction <- rep(NA, dim(dat.final3)[1])
prediction[treatment_dummy == 1] <- y.in.1
prediction[treatment_dummy == 2] <- y.in.2
prediction[treatment_dummy == 3] <- y.in.3

findMSE(dat.final3$y, prediction)
findMAE(dat.final3$y, prediction)
findRsquared(dat.final3$y, prediction)

y1 <- predict(svm_Radial.t1, newdata = dat.final3)
y2 <- predict(svm_Radial.t2, newdata = dat.final3)
y3 <- predict(svm_Radial.t3, newdata = dat.final3)

best=1*(y1< y2& y1<y3)+ 2*(y2 < y1 & y2 < y3)+ 3*(y3 < y1 & y3< y2)


# fit the SVM together
svm_Radial <- train(y ~., data = dat.final3, method = "svmRadial", trControl=trctrl, scale = FALSE, tuneLength = 20)
prediction <- predict(svm_Radial, newdata = dat.final3)

dat.final3.t1 <- dat.final3
dat.final3.t1$allocation_resultc_X2 <- rep(0, length(dat.final3.t1$allocation_resultc_X2))
dat.final3.t1$allocation_resultc_X3 <- rep(0, length(dat.final3.t1$allocation_resultc_X3))

dat.final3.t2 <- dat.final3
dat.final3.t2$allocation_resultc_X2 <- rep(1, length(dat.final3.t2$allocation_resultc_X2))
dat.final3.t2$allocation_resultc_X3 <- rep(0, length(dat.final3.t2$allocation_resultc_X3))

dat.final3.t3 <- dat.final3
dat.final3.t3$allocation_resultc_X2 <- rep(0, length(dat.final3.t3$allocation_resultc_X2))
dat.final3.t3$allocation_resultc_X3 <- rep(1, length(dat.final3.t3$allocation_resultc_X3))

y1 <- predict(svm_Radial, newdata = dat.final3.t1)
y2 <- predict(svm_Radial, newdata = dat.final3.t2)
y3 <- predict(svm_Radial, newdata = dat.final3.t3)

best=1*(y1< y2& y1<y3)+ 2*(y2 < y1 & y2 < y3)+ 3*(y3 < y1 & y3< y2)


