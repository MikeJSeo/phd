#devtools::install_github("MikeJSeo/bipd") # included some imputation functions
library(bipd)
library(dplyr)

setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")

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

# mydata <- mydata %>% mutate(gender = as.factor(gender),
#                             relstat = as.factor(relstat),
#                             ComorbidAnxiety = as.factor(ComorbidAnxiety),
#                             prevep = as.factor(prevep),
#                             Medication = as.factor(Medication),
#                             alcohol = as.factor(alcohol))

mydata <- mydata %>% select(all_of(covariates)) %>% mutate_if(typeofvar == "binary", as.factor)

# Centering the covariates
# mean_val <- apply(mydata[,covariates], 2, mean, na.rm = TRUE)
# sd_val <- apply(mydata[,covariates], 2, sd, na.rm = TRUE)
# mydata[,covariates] <- apply(mydata[,covariates], 2, scale)



###################################################################
# Need to load mice packages
library(mice) #for method = "pmm"
library(miceadds) #for method = "2l.pmm"
library(micemd) #for method = "2l.2stage.norm", "2l.2stage.bin", and "2l.2stage.pois"


#Naive approach
# There are two ways to impute using functions in ipdma
# (1) Try if ipdma.impute works
imputations_naive <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                            studyname = "study", treatmentname = "treat", outcomename = "y")

# (2) If above doesn't work we can try by first find correct method and prediction matrix and then using ipdma.impute using these obtained objects.
missP <- findMissingPattern(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), 
                            studyname = "study",  treatmentname = "treat", outcomename = "y")
correctMeth <- getCorrectMeth(mydata, missP)
correctPred <- getCorrectPred(mydata, missP)
imputations_naive <- ipdma.impute(mydata, covariates = covariates, typeofvar = c("continuous", "binary"),
                            interaction = TRUE, meth = correctMeth, pred = correctPred,
                            studyname = "study", treatmentname = "treat", outcomename = "y")


#Imputation approach
imputations_naive <- ipdma.impute(mydata, covariates = c("baseline", "gender"), typeofvar = c("continuous", "binary"), interaction = TRUE,
                                  studyname = "study", treatmentname = "treat", outcomename = "y")




#imputated datasets are stored
str(imputations$imp.list)











missingPattern <- findMissingPattern(newdata, c("baseline", "gender"))
meth <- getCorrectMeth(newdata, missingPattern)
pred <- getCorrectPred(newdata, missingPattern)

set.seed(1)
imp <- mice(newdata, pred = pred, meth = meth, m = 20)

fit <- with(imp, lmer(y ~ (baseline + gender) * treat + (1|study) + (0+treat|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))
coef_fit












# library(dplyr)
# library(miceadds)#2l.pmm
# library(micemd) #2l.2stage.norm
# library(mice)
# library(lme4)
# library(broom.mixed)
# library(mitools)



setwd("C:/Users/ms19g661/Documents/GitHub/phd/missing")
source("helpful.functions.R")
source("realdata.related.functions.R")






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
