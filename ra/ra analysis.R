setwd("C:/Users/ms19g661/Desktop")

library("readxl")
# xls files
mydata <- read_excel("ra_dataset.xlsx")
mydata <- as.data.frame(mydata)

## data with non-missing outcome
mydata <- mydata[!is.na(mydata$change_das_bsr),]
#apply(mydata, 2, function(x) sum(is.na(x)))

## data with complete data: three covariates removed
data <- mydata[, -which(colnames(mydata) %in% c("base_bmi", "base_haq", "comorbidities"))]
data <- data[complete.cases(data),]

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

BSRBR <- data[data$study == "BSRBR",]
table(BSRBR$treatment)

# REFLEX

REFLEX <- data[data$study == "REFLEX",]
table(REFLEX$treatment)

# SCQM

SCQM <- data[data$study == "SCQM",]
table(SCQM$treatment)

# TOWARD

TOWARD <- data[data$study == "TOWARD",]
table(TOWARD$treatment)


