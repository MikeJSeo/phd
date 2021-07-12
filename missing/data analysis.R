library(dplyr)
library(miceadds)#2l.pmm
library(micemd) #2l.2stage.norm
library(mice)
library(lme4)
library(broom.mixed)
library(mitools)

setwd("C:/Users/ms19g661/Documents/GitHub/phd/missing")
source("helpful.functions.R")

setwd("C:/Users/ms19g661/Desktop")
#setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")

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
mydata$study <- relabel.vec(mydata$study, unique(mydata$study))

# Delete study that have age missing, relstat missing
#mydata <- mydata %>% filter(!study %in% c("Gilbody, 2015"))
#mydata <- mydata %>% filter(!study %in% c("Gilbody, 2015", "Rosso, 2016"))

# visualizing variables
# library(summarytools)
# view(dfSummary(mydata), method = "browser")

# pick the covariates of interest
covariates <- c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol")
mydata <- mydata %>% select(study, y, treat, all_of(covariates))

mean_val <- apply(mydata[,covariates], 2, mean, na.rm = TRUE)
sd_val <- apply(mydata[,covariates], 2, sd, na.rm = TRUE)
mydata[,covariates] <- apply(mydata[,covariates], 2, scale)



##################################
##############Cross validation

# naive approach
naivepred <- findPrediction3(mydata, method = "naive")
save(naivepred, file = "realdata_naivepred.RData")
load("realdata_naivepred.RData")
naiveperf <- findPerformance(naivepred$testing_set_store, naivepred$predictions)
apply(naiveperf, 1, mean, na.rm = TRUE)

# imputation approach
imputationpred <- findPrediction3(mydata, method = "imputation")
save(imputationpred, file = "realdata_imputation.RData")
load("realdata_imputation.RData")
imputationperf <- findPerformance(imputationpred$testing_set_store, imputationpred$predictions)
apply(imputationperf, 1, mean, na.rm = TRUE)

# average prediction approach
averagepred <- findPrediction3(mydata, method = "average_prediction")
save(averagepred, file = "realdata_average_prediction.RData")
load("realdata_average_prediction.RData")
averageperf <- findPerformance(averagepred$testing_set_store, averagepred$predictions)
apply(averageperf, 1, mean, na.rm = TRUE)



##########################################
####Analysis using full data

#Naive approach - dropping systematically missing variables
newdata <- mydata %>% select(study, y, treat, baseline, gender, age, relstat)
newdata <- newdata %>% mutate(baselinetreat = NA, gendertreat = NA, agetreat = NA, relstattreat = NA)

meth <- make.method(newdata)
meth[c("y", "gender", "age", "relstat")] <- "2l.pmm"
meth["baselinetreat"] <- "~ I(baseline * treat)"
meth["gendertreat"] <- "~ I(gender * treat)"
meth["agetreat"] <- "~ I(age * treat)"
meth["relstattreat"] <- "~ I(relstat * treat)"

pred <- make.predictorMatrix(newdata)
pred[,] <- 0

codes <- c(rep(1, 4), 2, rep(1, 4))
codes2 <- c(rep(1, 4), 2, rep(1, 3))

pred["y", c("baseline", "gender", "age", "relstat", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat")] <- codes
pred["gender", c("y", "baseline", "age", "relstat", "treat", "baselinetreat", "agetreat", "relstattreat")] <- codes2
pred["age", c("y", "baseline", "gender", "relstat", "treat", "baselinetreat", "gendertreat", "relstattreat")] <- codes2
pred["relstat", c("y", "baseline", "gender", "age", "treat", "baselinetreat", "gendertreat", "agetreat")] <- codes2

pred[c("y", "gender", "age", "relstat"), "study"] <- -2
imp <- mice(newdata, pred = pred, meth = meth)

fit <- with(imp, lmer(y ~ (baseline + gender + age + relstat) * treat + (1|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))


#Imputation approach
newdata <- mydata
newdata <- newdata %>% mutate(baselinetreat = NA, gendertreat = NA, agetreat = NA, relstattreat = NA, ComorbidAnxietytreat = NA, preveptreat = NA, Medicationtreat = NA,  alcoholtreat = NA)
meth <- make.method(newdata)

meth[c("y", "gender", "age", "relstat")] <- "2l.pmm"
meth[c("ComorbidAnxiety", "prevep", "Medication", "alcohol")] <- "2l.2stage.norm"
meth["baselinetreat"] <- "~ I(baseline * treat)"
meth["gendertreat"] <- "~ I(gender * treat)"
meth["agetreat"] <- "~ I(age * treat)"
meth["relstattreat"] <- "~ I(relstat * treat)"
meth["ComorbidAnxietytreat"] <- "~ I(ComorbidAnxiety * treat)"
meth["preveptreat"] <- "~ I(prevep * treat)"
meth["Medicationtreat"] <- "~ I(Medication * treat)"
meth["alcoholtreat"] <- "~ I(alcohol * treat)"

pred <- make.predictorMatrix(newdata)
pred[,] <- 0

codes <- c(rep(1, 8), 2, rep(1, 8))
codes2 <- c(rep(1, 8), 2, rep(1, 7))

pred["y", c("baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat", "ComorbidAnxietytreat", "preveptreat", "Medicationtreat", "alcoholtreat")] <- codes
pred["gender", c("y", "baseline", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol", "treat", "baselinetreat", "agetreat", "relstattreat", "ComorbidAnxietytreat", "preveptreat", "Medicationtreat", "alcoholtreat")] <- codes2
pred["age", c("y", "baseline", "gender", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol", "treat", "baselinetreat", "gendertreat", "relstattreat", "ComorbidAnxietytreat", "preveptreat", "Medicationtreat", "alcoholtreat")] <- codes2
pred["relstat", c("y", "baseline", "gender", "age", "ComorbidAnxiety", "prevep", "Medication", "alcohol", "treat", "baselinetreat", "gendertreat", "agetreat", "ComorbidAnxietytreat", "preveptreat", "Medicationtreat", "alcoholtreat")] <- codes2
pred["ComorbidAnxiety",  c("y", "baseline", "gender", "age", "relstat", "prevep", "Medication", "alcohol", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat", "preveptreat", "Medicationtreat", "alcoholtreat")] <- codes2
pred["prevep", c("y", "baseline", "gender", "age", "relstat", "ComorbidAnxiety", "Medication", "alcohol", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat", "ComorbidAnxietytreat", "Medicationtreat", "alcoholtreat")] <- codes2
pred["Medication", c("y", "baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "alcohol", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat", "ComorbidAnxietytreat", "preveptreat", "alcoholtreat")] <- codes2
pred["alcohol", c("y", "baseline", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "treat", "baselinetreat", "gendertreat", "agetreat", "relstattreat", "ComorbidAnxietytreat", "preveptreat", "Medicationtreat")] <- codes2

pred[c("y", "gender", "age", "relstat", "ComorbidAnxiety", "prevep", "Medication", "alcohol"), "study"] <- -2
imp <- mice(newdata, pred = pred, meth = meth)

fit <- with(imp, lmer(y ~ (baseline + gender + age + relstat + ComorbidAnxiety + prevep + Medication + alcohol) * treat + (1|study)))
t(sapply(fit$analyses, fixef))
coef_fit <- summary(pool(fit))


# average predictions

for(i in 1:length(unique(mydata$study))){
  
  newdata <- mydata %>% filter(study == i) 
  
  missingPattern <- findMissingPattern(newdata, covariates)
  newdata <- newdata %>% select(-all_of(missingPattern$sys_covariates))

  newdata <- createinteractions(newdata, missingPattern$without_sys_covariates)
  
  meth <- getCorrectMeth(newdata, missingPattern)
  pred <- getCorrectPred(newdata, missingPattern)
  
  imp <- mice(newdata, pred = pred, meth = meth)
  impc <- complete(imp, "long", include = "TRUE")
  imp.list <- imputationList(split(impc, impc[,1])[-1])$imputations
  fit <- list()
  
  for(ii in 1:length(imp.list)){
    imp.dummy <- imp.list[[ii]]
    imp.dummy <- imp.dummy %>% select(-".imp", -".id", -"study")
    imp.model <- lm(y ~ ., data = imp.dummy)
    fit[[ii]] <- imp.model
    #summary(imp.model)
  }
  coef_fit <- summary(pool(fit))
  print(coef_fit)
}



#newdata %>% group_by(study) %>% summarize_all(~sum(is.na(.)))