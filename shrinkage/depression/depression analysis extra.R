# load packages needed
library(lme4) #for fitting glmm
library(glmnet) #for lasso

setwd("C:/Rworkspace")
data <- read.csv("database.csv")

################data cleaning
# exclude trials that are not one of these 4 trials
unique_studyid = c("LY248686A203C", "MLD55-11MDD21", "MLD5511M31", "Org3770-1")
data <- data[data$stduid %in% unique_studyid,] 

# Rename the study into numeric sequence
# 1 - LY248686A203C, 2 - MLD55-11MDD21, 3 - MLD5511M31, 4 - Org3770-1
studyid <- factor(data$stduid, levels = unique_studyid) 
studyid <- as.numeric(studyid)

# Define outcome measure
# The end point is 6wk for the study LY248686A203C and Org3770-1 and 8wk for other two studies.
y <- rep(NA, length(studyid))
y[studyid %in% c(1, 4)] <- data$hamd_total_wk6[studyid %in% c(1, 4)]
y[studyid %in% c(2, 3)] <- data$hamd_total_wk8[studyid %in% c(2, 3)]

# Define treatment
treat <- ifelse(data$allocation == 0, 0, 1)

# Covariates
# continuous variables: "baseseverity.hamd_total_start.", "age", "onset", "episode_duration_wk", "gilty_agitationHAM10_11_12_15_17", "body_symptoms_HAM9_13_14", "sleep_problems", "anhedonia_redardationHAM1_2_3_16"
# dichotomous variables: "sex_m1_f2", "episode_frequency_1_2_3over"

X <- data[,c("baseseverity.hamd_total_start.", "age", "sex_m1_f2", "onset",  "episode_frequency_1_2_3over", "episode_duration_wk", "gilty_agitationHAM10_11_12_15_17", "body_symptoms_HAM9_13_14", "sleep_problems", "anhedonia_redardationHAM1_2_3_16")]

# recode episode_frequency 3over - 1, under3 - 0
X$episode_frequency_1_2_3over <- ifelse(X$episode_frequency_1_2_3over == 3, 1, 0)

# recode sex variable m0_f1
X$sex <- ifelse(X$sex_m1_f2 == 1, 0, 1)

# scale variable
#scale_vars <- c("baseseverity.hamd_total_start.", "age", "onset", "episode_duration_wk", "gilty_agitationHAM10_11_12_15_17", "body_symptoms_HAM9_13_14",  "sleep_problems", "anhedonia_redardationHAM1_2_3_16")

#scale_vars <- c("baseseverity.hamd_total_start.", "age", "onset", "episode_duration_wk")
#X[,scale_vars] <- apply(X[,scale_vars], 2, scale)

X$onset <- as.numeric(paste0(X$onset))

data2 <- cbind(y = y, X, treat = treat, studyid = as.factor(studyid))
mydata <- na.omit(data2)
mydata2 <- data2[!complete.cases(data2),]

scale_vars <- c("baseseverity.hamd_total_start.", "age", "sex", "onset",  "episode_frequency_1_2_3over", "episode_duration_wk", "gilty_agitationHAM10_11_12_15_17", "body_symptoms_HAM9_13_14", "sleep_problems", "anhedonia_redardationHAM1_2_3_16")

mean_val <- apply(mydata[,scale_vars], 2, mean)
sd_val <- apply(mydata[,scale_vars], 2, sd)

mydata[,scale_vars] <- apply(mydata[,scale_vars], 2, scale)

X <- mydata[,scale_vars]

#characteristics
mean_val
sd_val

dim(mydata)
dim(mydata2)

apply(mydata2[,scale_vars], 2, mean, na.rm = TRUE)
apply(mydata2[,scale_vars], 2, sd, na.rm = TRUE)

mean(mydata$treat)
sd(mydata$treat)
mean(mydata$y)
sd(mydata$y)

mean(mydata2$treat, na.rm = TRUE)
sd(mydata2$treat, na.rm = TRUE)
mean(mydata2$y, na.rm = TRUE)
sd(mydata2$y, na.rm = TRUE)

################## models
bootstrap_function  <- function(model_data, ndraws, p.fac, family, alpha = 1, col_labels) {
  
  coeff_mtx <- matrix(0, nrow = ndraws, ncol = length(col_labels))
  
  for (ii in 1:ndraws) {
    
    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha)  
    aa <- coef(bootstrap_model, s = "lambda.min")
    coeff_mtx[ii,] <- sapply(col_labels, function(x) ifelse(x %in% rownames(aa)[aa[,1] != 0], aa[x,1], 0))  
  }
  se <- apply(coeff_mtx, 2, sd, na.rm = TRUE)
  return(se)
}


bootstrap_function2  <- function(model_data, ndraws, p.fac, family, alpha = 1, contr_vec) {
  
  predictions <- rep(NA, ndraws)
  
  for (ii in 1:ndraws) {
    
    bootstrap_ids <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data <- model_data[bootstrap_ids,]
    
    bootstrap_model <- cv.glmnet(as.matrix(bootstrap_data[,-1]), as.matrix(bootstrap_data[,1]), penalty.factor = p.fac, family = family, type.measure = "deviance", standardize = FALSE, alpha = alpha)  
    
    predictions_raw <- contr_vec %*% coef(bootstrap_model, s = "lambda.min")
    predictions[ii] <- ifelse(family == "gaussian", predictions_raw, exp(predictions_raw))
  }
  return(predictions)
}

group1 <- c(15, 25, 1, 20, 1, 42, 5, 5, 5, 7)
group1_std <- (group1 - mean_val)/sd_val

group2 <- c(25, 45, 0, 45, 0, 42, 5, 2, 1, 7)
group2_std <- (group2 - mean_val)/sd_val

set.seed(1)
data_glmnet <- model.matrix(y~ studyid + (baseseverity.hamd_total_start. + age + sex + onset + episode_frequency_1_2_3over + episode_duration_wk + gilty_agitationHAM10_11_12_15_17 + body_symptoms_HAM9_13_14 + sleep_problems + anhedonia_redardationHAM1_2_3_16) * treat, data = mydata)
data_glmnet <- data_glmnet[,-1]
data_glmnet <- cbind(y = mydata$y, data_glmnet = data_glmnet)

p.fac.depression = c(rep(0, 3), rep(0, 10), 0, rep(1, 10)) #No shrinkage for treatment effect and prognostic effect

cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.depression, family = "gaussian", type.measure = "deviance", standardize = FALSE, alpha = 0)
ridge_result <- coef(cvfit, s = "lambda.min")[-1]
p.fac.depression2 <- p.fac.depression / abs(ridge_result)


set.seed(1)
cvfit = cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac.depression2, family = "gaussian", type.measure = "deviance", standardize = FALSE, alpha = 1)
coef(cvfit, s = "lambda.min")

set.seed(1)
col_labels <- c("baseseverity.hamd_total_start.", "age", "sex", "onset", "episode_frequency_1_2_3over", "episode_duration_wk","gilty_agitationHAM10_11_12_15_17", "body_symptoms_HAM9_13_14", "sleep_problems", "anhedonia_redardationHAM1_2_3_16", "baseseverity.hamd_total_start.:treat", "age:treat", "sex:treat", "onset:treat", "episode_frequency_1_2_3over:treat", "episode_duration_wk:treat", "gilty_agitationHAM10_11_12_15_17:treat", "body_symptoms_HAM9_13_14:treat", "sleep_problems:treat", "anhedonia_redardationHAM1_2_3_16:treat", "treat")
sd_values <- bootstrap_function(model_data = data_glmnet, ndraws = 50, p.fac = p.fac.depression2, family = "gaussian", alpha = 1, col_labels)
sd_values

set.seed(1)
contr <- c(rep(0, 4), rep(0, 10), 1, group1_std)

predictions_group1 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.depression2, family = "gaussian", alpha = 1, contr_vec = contr)
quantile(predictions_group1, probs = c(0.025, 0.5, 0.975))

set.seed(1)
contr <- c(rep(0, 4), rep(0, 10), 1, group2_std)

predictions_group2 <- bootstrap_function2(model_data = data_glmnet, ndraws = 200, p.fac = p.fac.depression2, family = "gaussian", alpha = 1, contr_vec = contr)
quantile(predictions_group2, probs = c(0.025, 0.5, 0.975))


