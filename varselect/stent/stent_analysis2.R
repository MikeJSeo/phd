# load packages needed
library(readstata13)

library(lme4) #for fitting glmm
library(glmnet) #for lasso
library(glmmLasso) #for glmmLasso
library(rjags) # for bayesian models
library(dclone) # for parallel processing of JAGS code

library(MASS) # Used for data simulation
library(nlme) # for initial values for glmmLasso

# Setting directory
setwd("C:/Users/ms19g661/Desktop")
#setwd("C:/Users/mike/Desktop")
#stent <- read.dta13("stent.dta")
stent <- read.dta13("1_year_stent_data_21092018.dta")

#load/save everything else from/to github
setwd("~/GitHub/phd/varselect")
#setwd("C:/Users/mike/Desktop/Github/phd/varselect")


# "age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "gpi", "new_p2y12", "num_stent"
# gender=1 is men, gender=0 is female
# dropped gpi, new_p2y12
# dropped study 19: too big
study_inclusion <- c(3, 4, 5, 12, 13, 14, 15, 17)
data0 <- stent[stent$trial_name %in% study_inclusion,]

#rename study
for(i in 1:dim(data0)[1]){
  data0$trial_name[i] <- which(data0$trial_name[i] == study_inclusion)
}
data0$trial_name <- as.factor(data0$trial_name)

X <-  data0[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")]

#recode categorical variables
ii <- sapply(X, is.factor)
X[ii] <- lapply(X[ii], as.character)
X[X == "Yes"] <- 1
X[X == "Male"] <- 1
X[X == "No"] <- 0
X[X == "Female"] <- 0
X[ii] <- lapply(X[ii], as.numeric)

data_stent <- cbind(X, y = data0$a_death_5yr_yn, treat = ifelse(data0$rand == "BMS", 0, 1), studyid = data0$trial_name)
mydata <- na.omit(data_stent)

# scale all variables
mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")] <- apply(mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")], 2, scale)

X <- mydata[,c("age", "gender", "diabetes", "stable_cad", "multivessel","ladtreated", "overlap", "m_dia_above_3", "num_stent")]

# Total data: 11433 -> complete cases X: 11106 

col_labels_stent <- c("age", "gender", "diabetes", "stable_cad", "multivessel", "ladtreated", "overlap", "m_dia_above_3", "num_stent")
col_labels_stent_all <- c("(Intercept)", col_labels_stent, paste0(col_labels_stent, ":treat"), "treat", paste0("as.factor(studyid)", 1:length(unique(mydata$studyid))))
col_labels_stent_glmm <- c("(Intercept)", col_labels_stent, paste0(col_labels_stent, "_treat"), "treat", paste0("as.factor(studyid)", 1:length(unique(mydata$studyid))))

expit <- function(x){
  exp(x)/(1+exp(x))
}


# setup for parallel computing
n.cores <- 2
cl <- makePSOCKcluster(n.cores)
tmp <- clusterEvalQ(cl, library(dclone))

data_jags_stent <- with(mydata, {
  list(Nstudies = length(unique(studyid)),
       Ncovariate = 9,
       X = X,
       Np = dim(X)[1],
       studyid = studyid,
       treat = treat + 1,
       y = y
  )})

model_bayesLASSO <- jags.parfit(cl = cl, data = data_jags_stent, params = c("alpha","beta", "g", "d", "sdDelta"), model = "IPD-MA-bayesLASSO-binomial.txt", n.chains = 2, n.adapt = 100, n.update = 1000, n.iter = 10000)
summary(model_bayesLASSO)

model_SSVS <- jags.parfit(cl = cl, data = data_jags_stent, params = c("alpha", "beta", "g", "d", "sdDelta"), model = "IPD-MA-SSVS-binomial.txt", n.chains = 3, n.adapt = 100, n.update = 1000, n.iter = 10000)
summary(model_SSVS)


