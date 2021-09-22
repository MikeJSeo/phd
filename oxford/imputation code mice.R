library(mice) # imputation package
library(recipes) # https://recipes.tidymodels.org/reference/step_interact.html

library(devtools)
source("https://raw.githubusercontent.com/MikeJSeo/phd/master/missing/helpful.functions.R") # some helpful functions from my github

#please change the directory to where the data is stored
setwd("C:/Users/mike/Desktop") 
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


findMissingPattern(newdata, )

