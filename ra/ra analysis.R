setwd("C:/Users/ms19g661/Desktop")

library("readxl")
# xls files
mydata <- read_excel("ra_dataset.xlsx")
mydata <- as.data.frame(mydata)

## data with non-missing outcome
mydata2 <- mydata[!is.na(mydata$change_das_bsr),]


## data with complete data
#data <- mydata[complete.cases(mydata),]
#data2 <- mydata[, -which(colnames(mydata) %in% c("base_bmi"))]


## variables to include
# gender
# age
# base_bmi: BMI
# disease_duration: time from RA diagnosis to baseline visit, in years
# n_prev_dmards: No. of previous cDMARDs
# n_prev_antiTNF: No. of previous anti-TNF agents
# base_rf : rheumatoid factor
# conc_stero: On steroids
# base_haq: HAQ questionnaire (another disease activity score)
# comorbidities
# base_bsr: baseline ESR, not to be confused with the DAS28
# base_das_bsr: baseline DAS28 score



## four studies; BSRBR, REFLEX, SCQM, TOWARD

# BSRBR

BSRBR <- mydata[mydata$study == "BSRBR",]
table(BSRBR$treatment)

apply(BSRBR, 2, function(x) sum(is.na(x)))

# REFLEX

REFLEX <- mydata2[mydata2$study == "REFLEX",]
table(REFLEX$treatment)

apply(REFLEX, 2, function(x) sum(is.na(x)))

# SCQM

SCQM <- mydata2[mydata2$study == "SCQM",]
table(SCQM$treatment)

apply(SCQM, 2, function(x) sum(is.na(x)))


# TOWARD

TOWARD <- mydata[mydata$study == "TOWARD",]
table(TOWARD$treatment)

apply(TOWARD, 2, function(x) sum(is.na(x)))
