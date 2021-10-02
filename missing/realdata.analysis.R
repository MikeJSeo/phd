#devtools::install_github("MikeJSeo/bipd") # included some imputation functions
library(bipd)
library(dplyr)

#load imputation methods
library(mice) #pmm
library(miceadds)#2l.pmm
library(micemd) #2l.2stage.norm

library(lme4) #running lmer
#library(broom.mixed)
# library(mitools)

setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")

#setwd("C:/Users/mike/Desktop/Github/phd/missing")
#source("helpful.functions.R")
#source("realdata.related.functions.R")

################################################### Data cleaning

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

# pick the covariates of interest
covariates <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
typeofvar <- c("continuous", "binary", "continuous", "binary", "binary", "binary", "binary", "binary")
mydata <- mydata %>% select(study, y, treat, all_of(covariates))
mydata[,covariates] <- mydata[,covariates] %>% mutate_if(typeofvar == "binary", as.factor)


####################################################
# Finding Imputations

# Naive method
imputations_naivemethod <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                             studyname = "study", treatmentname = "treat", outcomename = "y")

# If above doesn't work we can try by first find correct method and prediction matrix and
# then using ipdma.impute using these obtained objects. Through this process, we can check if there is any errors
# in setting up these objects.
missP <- findMissingPattern(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), 
                            studyname = "study",  treatmentname = "treat", outcomename = "y")
correctMeth <- getCorrectMeth(mydata, missP)
correctPred <- getCorrectPred(mydata, missP)
imputations_test <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"),
                             interaction = TRUE, meth = correctMeth, pred = correctPred,
                             studyname = "study", treatmentname = "treat", outcomename = "y")

fit <- with(imputations_naivemethod$imp, lmer(y ~ (baseline + gender) * treat + (1|study) + (0+treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit





# Imputation method
imputations_impmethod <- ipdma.impute(mydata, covariates = covariates, typeofvar = typeofvar, interaction = TRUE,
                                      studyname = "study", treatmentname = "treat", outcomename = "y")







imputations2 <- ipdma.impute(mydata, covariates = covariates, typeofvar = typeofvar, interaction = TRUE,
                             studyname = "study", treatmentname = "treat", outcomename = "y")








##################################
##############Cross validation
####################################

testing_subset <- findTestData(mydata)

# naive approach
naivepred <- crossvalidation_realdata(mydata, method = "naive")
naiveperf <- findPerformance(testing_subset, naivepred)
naiveperf

# imputation approach
imputationpred <- crossvalidation_realdata(mydata, method = "imputation")
imputationperf <- findPerformance(testing_subset, imputationpred)
imputationperf

# separate prediction approach
separatepred <- crossvalidation_realdata(mydata, method = "separate")
separateperf <- findPerformance(testing_subset, separatepred)  
separateperf
  


###########################################################################
######Analysis using full data

#Naive approach
newdata <- mydata %>% select(study, y, treat, all_of(c("baseline", "gender")))
newdata <- newdata %>% mutate(baselinetreat = NA, gendertreat = NA)

missingPattern <- findMissingPattern(newdata, c("baseline", "gender"))
meth <- getCorrectMeth(newdata, missingPattern)
pred <- getCorrectPred(newdata, missingPattern)

set.seed(1)
imp <- mice(newdata, pred = pred, meth = meth, m = 20)

fit <- with(imp, lmer(y ~ (baseline + gender) * treat + (1|study) + (0+treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit

#Imputation approach
newdata <- mydata
newdata <- newdata %>% mutate(baselinetreat = NA, gendertreat = NA, agetreat = NA, relstattreat = NA, ComorbidAnxietytreat = NA, preveptreat = NA, Medicationtreat = NA,  alcoholtreat = NA)

missingPattern <- findMissingPattern(newdata, covariates)
meth <- getCorrectMeth(newdata, missingPattern, typeofvar = typeofvar)
pred <- getCorrectPred(newdata, missingPattern)

set.seed(1)
imp <- mice(newdata, pred = pred, meth = meth, m = 20)

fit <- with(imp, lmer(y ~ (baseline + gender + age + relstat + ComorbidAnxiety + prevep + Medication + alcohol) * treat + (1|study) + (0 + treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit

# Separate predictions approach
coef_fit_store <- list()

for(i in 1:length(unique(mydata$study))){
  
  newdata <- mydata %>% filter(study == i) 
  
  missingPattern <- findMissingPattern(newdata, covariates)
  newdata <- newdata %>% select(-all_of(missingPattern$sys_covariates))

  newdata <- createinteractions(newdata, missingPattern$without_sys_covariates)
  
  meth <- getCorrectMeth(newdata, missingPattern)
  pred <- getCorrectPred(newdata, missingPattern)
  print(meth)
  print(pred)
  
  set.seed(1)
  imp <- mice(newdata, pred = pred, meth = meth, m = 20)
  impc <- complete(imp, "long", include = "TRUE")
  imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
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
