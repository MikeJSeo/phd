library(mice) # imputation package
library(recipes) # https://recipes.tidymodels.org/reference/step_interact.html
library(fastDummies) #to create dummy columns for treatment

#please change the directory to where the data is stored
setwd("C:/Users/mike/Desktop") 
data <- read.csv("data_20210927.csv")

covariates <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
                "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
                "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                paste0("HAMD_WEEK_", 1:10), "TREATMENT_GROUP")

# Merge two studies
# data$STUDYID[data$STUDYID %in% c("29060/073", "29060/059")] <- "29060/059_073"
data2 <- data[, c("STUDYID", "PID", covariates)]

######### deal with categorized age problem
index1 <- data2[,"AGE"] %in% c("<25", "25-45", "45-65", "65-75", ">=75")
data2$AGE_CATEGORIZED <- NA
data2$AGE_CATEGORIZED[index1] <- data2[index1,"AGE"]
data2$AGE[index1] <- NA
data2$AGE <- as.numeric(as.character(data2$AGE))

data2$AGE_CATEGORIZED[data2$AGE <25 & !is.na(data2$AGE)] <- "<25"
data2$AGE_CATEGORIZED[data2$AGE >= 25 & data2$AGE < 45 & !is.na(data2$AGE)] <- "25-45"
data2$AGE_CATEGORIZED[data2$AGE >= 45 & data2$AGE < 65 & !is.na(data2$AGE)] <- "45-65"
data2$AGE_CATEGORIZED[data2$AGE >= 65 & data2$AGE < 75 & !is.na(data2$AGE)] <- "65-75"
data2$AGE_CATEGORIZED[data2$AGE >= 75 & !is.na(data2$AGE)] <- ">=75"

#rename age categorized
data2$AGE_CATEGORIZED[data2$AGE_CATEGORIZED == "<25"] <- "25_less"
data2$AGE_CATEGORIZED[data2$AGE_CATEGORIZED == "25-45"] <- "25_over_45_less"
data2$AGE_CATEGORIZED[data2$AGE_CATEGORIZED == "45-65"] <- "45_over_65_less"
data2$AGE_CATEGORIZED[data2$AGE_CATEGORIZED == "65-75"] <- "65_over_75_less"
data2$AGE_CATEGORIZED[data2$AGE_CATEGORIZED == ">=75"] <- "75_over"
data2$AGE_CATEGORIZED <- as.factor(data2$AGE_CATEGORIZED)

data2 <- dummy_cols(data2, select_columns = "AGE_CATEGORIZED", remove_selected_columns = TRUE, remove_first_dummy = TRUE) #removes first dummy 25_less

rec <- recipe(HAMD_WEEK_8 ~ . , data = data2)
mod1 <- rec %>% 
  step_dummy(TREATMENT_GROUP) %>%
  step_interact(terms = ~ c(AGE, SEX, HAMD_BASELINE, HAMD_3, HAMD_4, HAMD_6, HAMD_10, HAMD_11, HAMD_13, HAMD_17):starts_with("TREATMENT_GROUP"))
mod1 <- prep(mod1, training = data2)
newdata <- bake(mod1, data2)

covariates_full <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
                     "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
                     "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                     paste0("HAMD_WEEK_", 1:10),  grep("TREATMENT_GROUP", colnames(newdata), value = TRUE))

interaction_covariate_names <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE")

#baseline treatment: agomelatine
treatment_names <- c("TREATMENT_GROUP_amitriptyline",
                     "TREATMENT_GROUP_bupropion", "TREATMENT_GROUP_citalopram", "TREATMENT_GROUP_clomipramine",
                     "TREATMENT_GROUP_duloxetine", "TREATMENT_GROUP_escitalopram", "TREATMENT_GROUP_fluoxetine",                  
                     "TREATMENT_GROUP_imipramine", "TREATMENT_GROUP_paroxetine", "TREATMENT_GROUP_placebo",                     
                     "TREATMENT_GROUP_sertraline", "TREATMENT_GROUP_trazodone", "TREATMENT_GROUP_venlafaxine")
meth <- make.method(newdata)

for(i in interaction_covariate_names){
  for(j in treatment_names){
    meth[paste0(i, "_x_", j)] <- paste0("~ I(as.numeric(as.character(", i, ")) *", j,  ")")
  }
}

pred <- make.predictorMatrix(newdata)
pred[,] <- 0

# Specifing missing covariates that needs to be imputed
missing_covariates <- c("HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", paste0("HAMD_WEEK_", 1:10))
pred[missing_covariates, ] <- 1
pred[missing_covariates, c("STUDYID", "PID")] <- 0

# Delete interactions not needed
for(i in 1:length(interaction_covariate_names)){
  pred[interaction_covariate_names[i], paste0(interaction_covariate_names[i], "_x_", treatment_names)] <- 0
}
diag(pred) <- 0

imp <- mice(newdata, pred = pred, meth = meth)
impc <- complete(imp, "long", include = "TRUE")

impc.store <- impc[, c(".imp", "STUDYID", "PID", covariates[covariates != "TREATMENT_GROUP"], treatment_names, grep("AGE_CATEGORIZED", colnames(newdata), value = TRUE  ))]
save(impc.store, file = "oxford-imputations.RData")


load("oxford-imputations.RData")
library(mitools)
imp.list <- imputationList(split(impc.store, impc.store[,1]))$imputations
View(imp.list$`0`) #original data
View(imp.list$`1`) #imputed dataset 1




