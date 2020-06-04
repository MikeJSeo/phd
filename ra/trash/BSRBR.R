#BSRBR data analysis

setwd("C:/Users/ms19g661/Desktop/ra_data/BSRBR")

library(tidyverse)
library(readxl)

# baseline covariates
base <- read.csv("01 - tbl_baseline.csv", sep = ";")
base <- as_tibble(base)

base1 <- base %>% 
  mutate(DDURN = todyear - yonset,
         AGE = todyear - YearOfBirth,
         BMI = weight1/(height1/100)^2,
         baseDAS = dascore,
         pribio1 = ifelse(is.na(pribio), 0, pribio)) %>%
  rename(FEMALE = pgen, RF_pos = acrrpos, baseESR = daesr) %>%
  filter(pribio1 %in% c(0, 5, 1394)) %>%
  mutate(treat = recode(pribio1, '0' = '1', '5' = '2', '1394' = '3')) %>%
  select(LocalRandID, DDURN, FEMALE, RF_pos, AGE, BMI, baseESR, baseDAS, treat)

# add in baseline HAQ values as covariates
HAQ <- read.csv("13 - tbl_fup1.csv", sep = ";")
HAQ <- as_tibble(HAQ)

baseHAQ <- HAQ %>% filter(fupno == 0) %>%
  rename(baseHAQ = ovmean) %>%
  select(LocalRandID, baseHAQ)

base2 <- base1 %>% left_join(baseHAQ, by = "LocalRandID")

# Date of start of treatment; if missing (i.e. when no treatment) registered date is used
dateStart <- base %>%
  mutate(date0 = ifelse(!is.na(biodate), as.character(biodate), as.character(registdt)),
         date = as.Date(date0, tryFormats = "%d.%m.%Y")) %>%
  select(LocalRandID, date)

# Outcome: DAS
DAS <- read.csv("07 - tbl_fup.csv", sep = ";")
DAS <- as_tibble(DAS)


## limit the dataset for those that have date of DAS (dasdat)
DAS2 <- DAS %>% 
  mutate(dasdat1 = ifelse(dasdat == "NULL", NA, as.character(dasdat)),
         dasdat2 = as.Date(dasdat1, tryFormats = "%d.%m.%Y"),
         dastot = ifelse(dastot %in% c(888,999,"NULL"), NA, as.character(dastot))) %>%
  filter(!is.na(dasdat2)) %>%
  select(LocalRandID, dastot, dasdat2, Fupno)
  
## Find the time difference between baseline to followup measurement
DAS3 <- DAS2 %>% 
  left_join(dateStart, by = "LocalRandID") %>%
  mutate(diff = as.vector(difftime(dasdat2, date, units = "days"))) %>%
  filter(diff < 180 & diff > 0 & dastot != "NULL") 


## Remove any duplicates with same time difference
DAS4 <- DAS3 %>%
  distinct_at(vars(LocalRandID, diff), .keep_all = TRUE) 

## Keep only individuals that have both outcome and covariates
DAS5 <- DAS4 %>%
  filter(DAS4$LocalRandID %in% base2$LocalRandID)

base3 <- base2 %>%
  filter(base2$LocalRandID %in% DAS5$LocalRandID)


##scale except patient id and treatment
base5 <- base3
base5[,c(-1,-9)] <- apply(base5[,c(-1,-9)], 2, scale) 



#### fitting model

library(rjags)
library(mice)
setwd("~/GitHub/phd/ra")
y <- DAS5$dastot

num_obs <- as.vector(table(DAS5$LocalRandID))
patientid <- rep(1:dim(base5)[1], times = num_obs)

X = as.matrix(base5[, c("FEMALE", "AGE", "DDURN", "BMI", "RF_pos", "baseESR", "baseHAQ", "baseDAS")])
XX = mice(X, m = 5)

add.mcmc <- function(x,y){
  n.chains <- length(x)
  n.var <- nvar(x)
  newobjects <- vector("list", length = n.chains)
  
  for(i in 1:n.chains){
    newobjects[[i]] <- matrix(NA, nrow = 0, ncol = n.var, dimnames = list(NULL, dimnames(x[[1]])[[2]]))
    newobjects[[i]] <- rbind(x[[i]], y[[i]])
    newobjects[[i]] <- mcmc(newobjects[[i]])
  }
  mcmc.list(newobjects)
}

jags_data <- list(
  N = length(y),
  patientid = patientid,
  time = DAS5$diff,
  treat = base5$treat,
  Ncovariate = 8,
  Ntreat = 3,
  Npatient = length(unique(DAS5$LocalRandID)),
  X = complete(XX, 1),
  y = y
)

mod <- jags.model(file = "first stage model.txt", data = jags_data, n.chains = 3)
update(mod, 1000)

samples <- coda.samples(mod, variable.names = c("a", "b", "c", "d", "e", "f"), n.iter = 10000)

### add more samples
for(i in 2:5){
  
  jags_data <- list(
    N = length(y),
    patientid = patientid,
    time = DAS5$diff,
    treat = base5$treat,
    Ncovariate = 8,
    Ntreat = 3,
    Npatient = length(unique(DAS5$LocalRandID)),
    X = complete(XX, i),
    y = y
  )
  mod <- jags.model(file = "first stage model.txt", data = jags_data, n.chains = 3)
  update(mod, 1000)
  
  sample_more <- coda.samples(mod, variable.names = c("a", "b", "c", "d", "e", "f"), n.iter = 10000)
  samples <- add.mcmc(samples, sample_more)
}

summary(samples)
