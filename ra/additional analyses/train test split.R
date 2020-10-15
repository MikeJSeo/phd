#train test split

setwd("~/GitHub/phd/ra/R files")
source("helpful.functions.R")

setwd("C:/Users/ms19g661/Desktop")

# xlsx files
mydata <- read_excel("ra_dataset.xlsx")
BSRBR <- mydata %>% filter(study == "BSRBR")
SCQM <- mydata %>% filter(study == "SCQM")

####################### train test split test
setwd("C:/Users/ms19g661/Documents/GitHub/phd/ra/JAGS files")
SCQM2 <- SCQM %>% mutate(id = row_number())

#Create training set
train <- SCQM2 %>% sample_frac(.70)
#Create test set
test  <- anti_join(SCQM2, train, by = 'id')

train$id <- NULL
test$id <- NULL

result_SCQM <- firstStage(train, "first stage.txt",mm =1)
result <- result_SCQM[,c(1:10,39,40,20:37)]

prediction_train <- findPrediction(train, result)
performance_train <- findPerformance(prediction_train)
lapply(performance_train, mean)

prediction_test <- findPrediction(test, result)
performance_test <- findPerformance(prediction_test)
lapply(performance_test, mean)


#############################
BSRBR2 <- BSRBR %>% mutate(id = row_number())

#Create training set
train <- BSRBR2 %>% sample_frac(.70)
#Create test set
test  <- anti_join(BSRBR2, train, by = 'id')

train$id <- NULL
test$id <- NULL

result_BSRBR <- firstStage(train, "first stage.txt",mm =1)
result <- result_BSRBR[,c(1:10,39,40,20:37)]

prediction_train <- findPrediction(train, result)
performance_train <- findPerformance(prediction_train)
lapply(performance_train, mean)

prediction_test <- findPrediction(test, result)
performance_test <- findPerformance(prediction_test)
lapply(performance_test, mean)