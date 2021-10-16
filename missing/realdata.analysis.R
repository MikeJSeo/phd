#devtools::install_github("MikeJSeo/bipd") # personal github with some imputation related functions
library(bipd)
library(dplyr)

#load imputation methods
library(mice) #pmm
library(miceadds)#2l.pmm
library(micemd) #2l.2stage.norm

library(lme4) #running lmer
library(broom.mixed) #for summarizing lmer results from multiply imputed dataset
library(mitools) #for function imputationList

setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")

setwd("C:/Users/mike/Desktop/Github/phd/missing")
source("helpful.functions.R")
source("realdata.crossvalidation.R")

############################################################################
################ Data cleaning

# pick which treatment to compare
mydata <- data %>% filter(treat.m2 %in% c("TAU", "Guided", "Unguided"))

# merge Guided and Unguided as treatment
mydata <- mydata %>% mutate(treat.m2 = ifelse(treat.m2 %in% c("Guided", "Unguided"), 1, 0)) %>%
  rename(treat = treat.m2)

# Delete 2 studies that have same gender(female) for all participants
mydata <- mydata %>% filter(!study %in% c("Forsell, 2017", "Milgrom, 2016")) %>%
  mutate(gender = na_if(gender, "")) %>%
  mutate(gender = ifelse(gender == "Female", 1, 0))

# Delete a study where ComorbidAnxiety is only observed for treated patients
mydata <- mydata %>% filter(!study %in% c("Christensen, 2004"))

# leave studies two treatment (delete the ones that have only 1)
mydata2 <- mydata %>% group_by(study) %>% filter(length(unique(treat)) == 2)
mydata <- mydata %>% filter(study %in% unique(mydata2$study))
mydata$study <- as.numeric(as.factor(mydata$study))

# visualizing variables
# library(summarytools)
# view(dfSummary(mydata), method = "browser")

# Specify the covariates of interest and type of variable
covariates <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
typeofvar <- c("continuous", "binary", "continuous", "binary", "binary", "binary", "binary", "binary")
mydata <- mydata %>% select(all_of(c("study", "treat", "y", covariates)))


###########################################################################
############ Find regression estimates using full data

# Naive approach
set.seed(1)
naiveapproach <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                             studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
fit <- with(naiveapproach$imp, lmer(y ~ (baseline + gender) * treat + (1|study) + (0+treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit

# Imputation approach - ignoring clustering
set.seed(1)
imputationapproach.nocluster <- ipdma.impute(mydata, covariates = covariates, typeofvar = typeofvar, sys_impute_method = "pmm",
                                   interaction = TRUE, studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
fit <- with(imputationapproach.nocluster$imp, lmer(y ~ (baseline + gender + age + relstat + ComorbidAnxiety + prevep + Medication + alcohol) * treat + (1|study) + (0 + treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit

# Imputation approach - accounting clustering
set.seed(1)
imputationapproach <- ipdma.impute(mydata, covariates = covariates, typeofvar = typeofvar, interaction = TRUE,
                                  studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
fit <- with(imputationapproach$imp, lmer(y ~ (baseline + gender + age + relstat + ComorbidAnxiety + prevep + Medication + alcohol) * treat + (1|study) + (0 + treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit

# Separate prediction approach
set.seed(1)
coef_fit_store <- list()

for(i in 1:length(unique(mydata$study))){
  
  newdata <- mydata %>% filter(study == i) 
  
  # Need to remove systematically missing covariates for each study
  missingPattern <- findMissingPattern(newdata, covariates, typeofvar, studyname = "study", treatmentname = "treat", outcomename = "y")
  newdata <- newdata %>% select(-all_of(missingPattern$sys_covariates))
  
  imputationapproach <- ipdma.impute(newdata, covariates = missingPattern$without_sys_covariates, typeofvar = typeofvar[which(covariates %in% missingPattern$without_sys_covariates)], interaction = TRUE,
                                     studyname = "study", treatmentname = "treat", outcomename = "y", m = 20)
  imp.list <- imputationapproach$imp.list
  fit <- list()
  for(ii in 1:length(imp.list)){
    imp.dummy <- imp.list[[ii]]
    imp.dummy <- imp.dummy %>% select(-".imp", -".id", -"study")
    imp.model <- lm(y ~ ., data = imp.dummy)
    fit[[ii]] <- imp.model
  }
  coef_fit_store[[i]] <- summary(pool(fit))
}
coef_fit_store


########################################################################
########### Cross validation
# Cross-validation functions are stored in separate R file: realdata.crossvalidation.R

# naive approach
set.seed(1)
naive_crossvalidation <- crossvalidation_realdata(mydata, method = "naive")
naivepred <- naive_crossvalidation$predictions
testingoutcome <- naive_crossvalidation$testingoutcome
naiveperf <- findPerformance(testingoutcome, naivepred, aggregation = "weighted")
naiveperf

# imputation approach ignoring clustering
set.seed(1)
imputation_nocluster_crossvalidation <- crossvalidation_realdata(mydata, method = "imputation_nocluster")
imputationpred <- imputation_nocluster_crossvalidation$predictions
testingoutcome <- imputation_nocluster_crossvalidation$testingoutcome
imputation_noclusterperf <- findPerformance(testingoutcome, imputationpred, aggregation = "weighted")
imputation_noclusterperf

# imputation approach accounting for clustering
set.seed(1)
imputation_crossvalidation <- crossvalidation_realdata(mydata, method = "imputation")
imputationpred <- imputation_crossvalidation$predictions
testingoutcome <- imputation_crossvalidation$testingoutcome
imputationperf <- findPerformance(testingoutcome, imputationpred, aggregation = "weighted")
imputationperf

# separate prediction approach
set.seed(1)
separate_crossvalidation <- crossvalidation_realdata(mydata, method = "separate")
separatepred <- separate_crossvalidation$predictions
testingoutcome <- separate_crossvalidation$testingoutcome
separateperf <- findPerformance(testingoutcome, separatepred, aggregation = "weighted")
separateperf

rbind(naiveperf, imputation_noclusterperf, imputationperf, separateperf)



### Just checking different imputation method: 2l.glm.norm
# imputation approach accounting for clustering
set.seed(1)
imputation_crossvalidation <- crossvalidation_realdata(mydata, method = "imputation_2lglm")
imputationpred <- imputation_crossvalidation$predictions
testingoutcome <- imputation_crossvalidation$testingoutcome
imputationperf <- findPerformance(testingoutcome, imputationpred, aggregation = "weighted")
imputationperf