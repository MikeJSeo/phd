library(jomo) # imputation package
library(recipes) # https://recipes.tidymodels.org/reference/step_interact.html

setwd("C:/Users/mike/Desktop") #change directory
data <- read.csv("data_for_new_imputation.csv")

covariates <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
                "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
                "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                paste0("HAMD_WEEK_", 1:10), "TREATMENT_GROUP")

# Merge two studies
data$STUDYID[data$STUDYID %in% c("29060/073", "29060/059")] <- "29060/059_073"

data2 <- data[, c("STUDYID", covariates)]

rec <- recipe(HAMD_WEEK_8 ~ . , data = data2)
mod1 <- rec %>% 
  step_dummy(TREATMENT_GROUP) %>%
  step_interact(terms = ~ c(AGE, SEX, HAMD_BASELINE, HAMD_3, HAMD_4, HAMD_6, HAMD_10, HAMD_11, HAMD_13, HAMD_17):starts_with("TREATMENT_GROUP")) #+ SEX:starts_with("TREATMENT_GROUP"))
mod1 <- prep(mod1, training = data2)
newdata <- bake(mod1, data2)

# covariates_full <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
#                      "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
#                      "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
#                      paste0("HAMD_WEEK_", 1:10),  grep("TREATMENT_GROUP", colnames(newdata), value = TRUE))

# Convert cluster variable STUDYID and SEX to integer
newdata$STUDYID <- as.numeric(as.factor(newdata$STUDYID))
newdata$SEX <- as.numeric(newdata$SEX)
newdata <- as.data.frame(newdata)

# The imputation model requires an intercept variable (simply a column of 1's)
newdata$cons <- 1

y_imputation <- newdata[, c(paste0("HAMD_WEEK_", 1:10), "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17",
                            grep("HAMD_3_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_4_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_6_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_10_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_11_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_13_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_17_x_TREATMENT_GROUP", colnames(newdata), value = TRUE)
                            )]
X_imputation <- newdata[, c("cons", "AGE", "SEX", "HAMD_BASELINE", "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA", "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                            "TREATMENT_GROUP_bupropion", "TREATMENT_GROUP_citalopram", "TREATMENT_GROUP_clomipramine",
                            "TREATMENT_GROUP_duloxetine", "TREATMENT_GROUP_escitalopram", "TREATMENT_GROUP_fluoxetine",                  
                            "TREATMENT_GROUP_imipramine", "TREATMENT_GROUP_paroxetine", "TREATMENT_GROUP_placebo",                     
                            "TREATMENT_GROUP_sertraline", "TREATMENT_GROUP_trazodone", "TREATMENT_GROUP_venlafaxine",
                            grep("AGE_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("SEX_x_TREATMENT_GROUP", colnames(newdata), value = TRUE),
                            grep("HAMD_BASELINE_x_TREATMENT_GROUP", colnames(newdata), value = TRUE))]
clus <- newdata$STUDYID

imp <- jomo(Y = y_imputation, X = X_imputation, clus = clus, meth = "common", nburn = 1000, nbetween = 1000, nimp = 5)
#save(imp, file = "oxford-imputations.RData")

load("oxford-imputations.RData")
library(mitools)
#imp.list <- imputationList(split(imp, imp$Imputation)[-1])
imp.list <- imputationList(split(imp, imp$Imputation))
View(imp.list$imputations$`0`) #original data
View(imp.list$imputations$`1`) #imputed dataset 1




