# load packages needed
library(MASS) # Used for data simulation

library(glmnet) #for lasso
library(writexl)

#setwd("C:/Users/mike/Desktop/Github/phd/varselect/")
setwd("~/GitHub/phd/varselect/")

source("helpful.functions.final.R")

run.simulation <- function(){
  
  ridge_store_mse <- matrix(NA, nrow = niter, ncol = 4)
  ridge_store_sd <- matrix(NA, nrow = niter, ncol = 3)
  
  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, beta = beta, gamma = gamma, model.type = model.type, tau = tau)
    
    data_glmnet <- model.matrix(step_full_formula, data = data)
    data_glmnet <- data_glmnet[,-1] 
    data_glmnet <- cbind(y = data$y, data_glmnet = data_glmnet)  
    
    p.fac <- c(rep(0, Nstudies - 1), rep(0, Ncovariate), 0, rep(1, Ncovariate))
    
    family <- ifelse(model.type == "gaussian", "gaussian", "binomial")
    lambdas <- 10^seq(3, -3, by = -.1) #manually specify lambda value to cross validate
    cvfit <- cv.glmnet(as.matrix(data_glmnet[,-1]), as.matrix(data_glmnet[,1]), penalty.factor = p.fac, family = family, standardize = FALSE, type.measure = "deviance", alpha = 0, lambda = lambdas)  
    aa <- coef(cvfit, s = "lambda.min")
    
    mean_values <-  sapply(col_labels, function(x) ifelse(x %in% rownames(aa)[aa[,1] != 0], aa[x,1], 0))
    
    ridge_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em, data_glmnet[,col_labels])
  }
  
  ridge_store_mse_mean <- apply(ridge_store_mse, 2, mean)
  ridge_store_sd_mean <- apply(ridge_store_sd, 2, mean, na.rm = TRUE)

  result_matrix_mse <- matrix(NA, nrow = 1, ncol = 4)
  colnames(result_matrix_mse) <- c("false em mse", "true em mse","treatment mse", "patient specific trt mse")
  rownames(result_matrix_mse) <-  c("ridge")
  result_matrix_mse[1,] <- ridge_store_mse_mean

  round(result_matrix_mse,3)
}

# number of simulation to run
niter <- 1000
model.type = "binary"

### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- NULL
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- NULL
tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation31 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- NULL
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- NULL
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation32 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.1
tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation33 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.1
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation34 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.5
tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation35 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.5
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation36 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
correct_em_values <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4)
  
Nstudies <- 5
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 4, 5)
pf <- c(1,2,3,4,5,6,7,8,9,10)
em <- c(1,2,3,4,5,6,7,8,9,10)
beta <- rep(0.2, 10)
gamma <-  c(0.5, 0.5, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4)
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation37 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1)
tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation38 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)
  
Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1)
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation39 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.4)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation40 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.4)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation41 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1, 0.1)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation42 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1, 0.1)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation43 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.5, 0.4)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation44 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 5
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.5, 0.4)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")

simulation45 <- run.simulation()

### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- NULL
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- NULL

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation46 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- NULL
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- NULL

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation47 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.1

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation48 <- run.simulation()



### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.1

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation49 <- run.simulation()



### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.5

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation50 <- run.simulation()

### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 5, 6, 7, 8)
pf <- c(1,2,3,4,5)
em <- 5
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- 0.5

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")

simulation51 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:10, ":treat"), "treat")

correct_em <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
correct_em_values <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4)

Nstudies <- 10
Ncovariate <- 10
continuous.cov <- c(1, 2, 3, 4, 5)
pf <- c(1,2,3,4,5,6,7,8,9,10)
em <- c(1,2,3,4,5,6,7,8,9,10)
beta <- rep(0.2, 10)
gamma <-  c(0.5, 0.5, 0.5, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4)
tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)*treat")
simulation52 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation53 <- run.simulation()



### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation54 <- run.simulation()



### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.4)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation55 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 3, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.4)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation56 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1, 0.1)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation57 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.1, 0.1, 0.1)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation58 <- run.simulation()



### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.5, 0.4)

tau <- 0.2

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation59 <- run.simulation()


### parameters to change ###
col_labels <- c(paste0("X", 1:15, ":treat"), "treat")

correct_em <- c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
correct_em_values <- c(0, 0, 0, 0, 0.5, 0.5, 0.4, 0, 0, 0, 0, 0, 0, 0, 0)

Nstudies <- 10
Ncovariate <- 15
continuous.cov <- c(1, 2, 5, 6, 8, 9, 10, 11, 12)
pf <- c(1,2,3,4,5,6,7)
em <- c(5,6,7)
beta <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
gamma <- c(0.5, 0.5, 0.4)

tau <- 0.5

step_full_formula <-  as.formula("y ~ studyid + (X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15)*treat")
simulation60 <- run.simulation()
