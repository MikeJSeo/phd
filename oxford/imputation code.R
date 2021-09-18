library(jomo) # imputation package
library(fastDummies) #to create dummy columns for treatment

setwd("C:/Users/ms19g661/Desktop") #change directory
data <- read.csv("data_for_new_imputation.csv")

covariates <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
                "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
                "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                paste0("HAMD_WEEK_", 1:10))

# Merge two studies
data$STUDYID[data$STUDYID %in% c("29060/073", "29060/059")] <- "29060/059_073"

# Change treatment to dummy variables
data <- dummy_cols(data, select_columns = "TREATMENT_GROUP", remove_selected_columns = TRUE)
covariates <- c(covariates, grep("TREATMENT_GROUP", colnames(data), value = TRUE))

#convert cluster variable STUDYID to integer
newdata <- data[, c("STUDYID", covariates)]
newdata$STUDYID <- as.numeric(as.factor(newdata$STUDYID))
newdata[, c("STUDYID", covariates)] <- lapply(newdata[, c("STUDYID", covariates)], as.numeric)

# The imputation model requires an intercept variable (simply a column of 1's)
newdata$cons <- 1

y_imputation <- newdata[, c(paste0("HAMD_WEEK_", 1:10), "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17")]
X_imputation <- newdata[, c("cons", "AGE", "SEX", "HAMD_BASELINE", "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA", "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION")]
clus <- newdata$STUDYID

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "oxford-imputations.RData")

library(mitools)
imp.list <- imputationList(split(imp, imp$Imputation)[-1])


# Define variable type
typeofvar <- c("continuous", "binary", rep("continuous", 7), "continuous",
               rep("binary", 6), rep("continuous", 10), rep("binary", 13))
names(typeofvar) <- covariates               



