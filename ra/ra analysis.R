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
## not included: base_bmi: BMI
# disease_duration: time from RA diagnosis to baseline visit, in years
# n_prev_dmards: No. of previous cDMARDs
# n_prev_antiTNF: No. of previous anti-TNF agents
# base_rf : rheumatoid factor
# conc_stero: On steroids
## not included: base_haq: HAQ questionnaire (another disease activity score)
## not included: comorbidities
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



########## First stage (individual analysis)


data <- BSRBR
y <- data$change_das_bsr
X <- data[,c("gender", "age", "disease_duration", "n_prev_dmards", "n_prev_antiTNF", "base_rf", "conc_stero", "base_bsr", "base_das_bsr")]
Np <- length(y)
Ncovariate <- ncol(X)

data$treatment[data$treatment == "DMARDs"] <- 1
data$treatment[data$treatment == "RTX + DMARDs"] <- 2
data$treatment[data$treatment == "TCZ + DMARDs"] <- 3
data$treatment <- as.numeric(data$treatment)

data_jags <- with(data,{
  list(y = y,
       X = X,
       Np = Np,
       Ncovariate = Ncovariate,
       treat = treatment)
})


library(rjags)
setwd("~/GitHub/phd/ra")

jags_model <- jags.model("IPD-MA.txt", data_jags, n.chains = 3, n.adapt = 1000)
TOWARD_samples <- coda.samples(jags_model, variable.names = c("gamma", "beta", "delta"), n.iter = 2000)
summary(TOWARD_samples)


##########

