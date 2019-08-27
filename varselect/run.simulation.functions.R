
run.simulation <- function(){
  
  glmmLasso_store_mse <- glmnet_store_mse <- step_store_mse <- glmm_full_store_mse <- glmm_null_store_mse <- matrix(NA, nrow = niter, ncol = 3)
  glmmLasso_store_sd <- glmnet_store_sd <- step_store_sd <- glmm_full_store_sd <- glmm_null_store_sd <- matrix(NA, nrow = niter, ncol = 3)
  
  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    if(model.type == "gaussian"){
      m1 <- lmer(glmm_null_formula, data = data)
    } else if(model.type == "binary"){
      m1 <- glmer(glmm_null_formula, data = data, family = binomial(link = "logit"))
    }
    
    mean_values <- sapply(col_labels_glmm, function(x) ifelse(x %in% names(fixef(m1)), summary(m1)$coef[x,"Estimate"], 0))
    sd_values <- sapply(col_labels_glmm, function(x) ifelse(x %in% names(fixef(m1)), summary(m1)$coef[x,"Std. Error"], 0))
    
    glmm_null_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    glmm_null_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }

  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    if(model.type == "gaussian"){
      m1 <- lmer(glmm_full_formula, data = data)
    } else if(model.type == "binary"){
      m1 <- glmer(glmm_full_formula, data = data, family = binomial(link = "logit"))
    }

    mean_values <- sapply(col_labels_glmm, function(x) ifelse(x %in% names(fixef(m1)), summary(m1)$coef[x,"Estimate"], 0))
    sd_values <- sapply(col_labels_glmm, function(x) ifelse(x %in% names(fixef(m1)), summary(m1)$coef[x,"Std. Error"], 0))
    
    glmm_full_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    glmm_full_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }
  
  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    
    if(model.type == "gaussian"){
      m1 <- lm(step_full_formula, data = data)  
    } else if(model.type == "binary"){
      m1 <- glm(step_full_formula, family = binomial(link = "logit"), data = data)
    }
    s1 <- step(m1, scope=list(lower=~treat))
    
    mean_values <- sapply(col_labels, function(x) ifelse(x %in% variable.names(s1), summary(s1)$coef[x,"Estimate"], 0))
    sd_values <- sapply(col_labels, function(x) ifelse(x %in% variable.names(s1), summary(s1)$coef[x,"Std. Error"], 0))
    
    step_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    step_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }
  
  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    data$studyid <- NULL
    
    p.fac <- rep(1, length(col_labels)*2 - 1)
    p.fac[length(col_labels)] <- 0
    
    cvfit <- cv.glmnet(as.matrix(data[,-1]), as.matrix(data[,1]), penalty.factor = p.fac, family = model.type, standardize = FALSE)  
    aa <- coef(cvfit, s = "lambda.min")
    
    mean_values <-  sapply(col_labels, function(x) ifelse(x %in% rownames(aa)[aa[,1] != 0], aa[x,1], 0))
    sd_values <- bootstrap_function_LASSO(data, 50, p.fac)
    
    glmnet_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    glmnet_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }
  
  for(i in seq(niter)){
    
    set.seed(i)  
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    
    #This part is used for setting initial values
    if(model.type == "gaussian"){
      PQL <-glmmPQL(pql_formula, random = ~ -1 + treat|studyid, family=gaussian, data=data)
    }
    q_start <- as.numeric(VarCorr(PQL)[1,1])
    start <- as.numeric(c(PQL$coef$fixed[-which(names(PQL$coef$fixed) == "treat")], PQL$coef$fixed[which(names(PQL$coef$fixed) == "treat")], t(PQL$coef$random$studyid)))

    colnames(data) <- gsub(":", "_", colnames(data)) #glmmLasso doesn't allow colon sign
    
    form.fixed <- glmmLasso_formula 
    form.rnd <- list(studyid =~ -1 + treat)
      
    if(model.type == "gaussian") {
      cv.fit <- cv.glmmLasso(data, form.fixed = form.fixed, form.rnd = form.rnd, lambda = seq(50, 0, by = -5), family = gaussian(link = "identity"), q_start = q_start, start = start)
    } else if(model.type == "binary"){
      cv.fit <- cv.glmmLasso(data, form.fixed = form.fixed, form.rnd = form.rnd, lambda = seq(50, 0, by = -5), family = binomial(link = "logit"), q_start = q_start, start = start)
    }
    aa <- summary(cv.fit[[1]])$coefficients
    aa <- rownames(aa[aa[,"Estimate"] != 0,])
    
    mean_values <- sapply(col_labels_glmmLasso, function(x) ifelse(x %in% aa, summary(cv.fit[[1]])$coefficients[x,"Estimate"], 0))
    # sd_values <- bootstrap_function_glmmLasso(data, 50, lambda = cv.fit[[2]], model.type, form.fixed, form.rnd, q_start = q_start , start = start)
    # need to use lambda = seq(50, 0, by = -5), however, takes too long
    
    glmmLasso_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    #glmmLasso_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }
  
  glmm_null_store_mse_mean <- apply(glmm_null_store_mse, 2, mean)
  glmm_null_store_sd_mean <- apply(glmm_null_store_sd, 2, mean, na.rm = TRUE)
  glmm_full_store_mse_mean <- apply(glmm_full_store_mse, 2, mean)
  glmm_full_store_sd_mean <- apply(glmm_full_store_sd, 2, mean, na.rm = TRUE)
  step_store_mse_mean <- apply(step_store_mse, 2, mean)
  step_store_sd_mean <- apply(step_store_sd, 2, mean, na.rm = TRUE)
  glmnet_store_mse_mean <- apply(glmnet_store_mse, 2, mean)
  glmnet_store_sd_mean <- apply(glmnet_store_sd, 2, mean, na.rm = TRUE)
  glmmLasso_store_mse_mean <- apply(glmmLasso_store_mse, 2, mean)
  glmmLasso_store_sd_mean <- apply(glmmLasso_store_sd, 2, mean, na.rm = TRUE)
  
  result_matrix_mse <- matrix(NA, nrow = 5, ncol = 3)
  colnames(result_matrix_mse) <- c("false em mse", "true em mse","treatment mse")
  rownames(result_matrix_mse) <-  c("glmm null", "glmm full","naive step", "naive lasso", "glmmLasso")
  result_matrix_mse[1,] <- glmm_null_store_mse_mean
  result_matrix_mse[2,] <- glmm_full_store_mse_mean
  result_matrix_mse[3,] <- step_store_mse_mean
  result_matrix_mse[4,] <- glmnet_store_mse_mean
  result_matrix_mse[5,] <- glmmLasso_store_mse_mean
  
  result_matrix_sd <- matrix(NA, nrow = 5, ncol = 3)
  colnames(result_matrix_sd) <- c("continuous EM se", "binary EM se","treatment se")
  rownames(result_matrix_sd) <-  c("glmm null", "glmm full","naive step", "naive lasso", "glmmLasso")
  result_matrix_sd[1,] <- glmm_null_store_sd_mean
  result_matrix_sd[2,] <- glmm_full_store_sd_mean
  result_matrix_sd[3,] <- step_store_sd_mean
  result_matrix_sd[4,] <- glmnet_store_sd_mean
  result_matrix_sd[5,] <- glmmLasso_store_sd_mean
  
  cbind(result_matrix_mse, result_matrix_sd)
}

run.simulation2 <- function(){

  SSVS_store_mse <- bayesLASSO_store_mse <- matrix(NA, nrow = niter, ncol = 3)
  SSVS_store_sd <- bayesLASSO_store_sd <- matrix(NA, nrow = niter, ncol = 3)
  
  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    
    data_jags <- with(data,{
      list(Nstudies = length(unique(studyid)),
           X = data[,paste0("X",1:Ncovariate)],
           Np = length(X1),
           Ncovariate = (dim(data)[2] - 3)/2,
           studyid = studyid,
           treat = treat + 1,
           y = y)
    })
    
    if(model.type == "gaussian") {
      samples <- jags.parfit(cl = cl, data = data_jags, params = c("g", "d"), model = "IPD-MA-bayesLASSO.txt", n.chains = 2, n.adapt = 100, n.update = 200, n.iter = 2000)
    } else if(model.type == "binary"){
      samples <- jags.parfit(cl = cl, data = data_jags, params = c("g", "d"), model = "IPD-MA-bayesLASSO-binomial.txt", n.chains = 2, n.adapt = 100, n.update = 200, n.iter = 2000)
    }
    
    a <- summary(samples)
    
    g_mean <-  a$statistics[grep("g\\[", rownames(a$statistics)), "Mean"]
    treat_mean <- a$statistics["d[2]", "Mean"]
    names(treat_mean) <- "treat"
    mean_values <- c(g_mean, treat_mean)
    bayesLASSO_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    
    g_sd <-  a$statistics[grep("g\\[", rownames(a$statistics)), "SD"]
    treat_sd <- a$statistics["d[2]", "SD"]
    names(treat_sd) <- "treat"
    sd_values <- c(g_sd, treat_sd)
    bayesLASSO_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }

  for(i in seq(niter)){
    
    set.seed(i)
    data <-generate.simulation(Nstudies = Nstudies, Ncovariate = Ncovariate, continuous.cov = continuous.cov, pf = pf, em = em, b1 = b1, b2 = b2, model.type = model.type)
    data_jags <- with(data,{
      list(Nstudies = length(unique(studyid)),
           X = data[,paste0("X",1:Ncovariate)],
           Np = length(X1),
           Ncovariate = (dim(data)[2] - 3)/2,
           studyid = studyid,
           treat = treat + 1,
           y = y)
    })
    
    if(model.type == "gaussian") {
      samples <- jags.parfit(cl = cl, data = data_jags, params = c("g", "d"), model = "IPD-MA-SSVS.txt", n.chains = 2, n.adapt = 100, n.update = 200, n.iter = 2000)
    } else if(model.type == "binary"){
      samples <- jags.parfit(cl = cl, data = data_jags, params = c("g", "d"), model = "IPD-MA-SSVS-binomial.txt", n.chains = 2, n.adapt = 100, n.update = 200, n.iter = 2000)
    }
    
    a <- summary(samples)
    
    g_mean <-  a$statistics[grep("g\\[", rownames(a$statistics)), "Mean"]
    treat_mean <- a$statistics[grep("d\\[", rownames(a$statistics)), "Mean"]
    treat_mean <- treat_mean["d[2]"]
    names(treat_mean) <- "treat"
    mean_values <- c(g_mean, treat_mean)
    SSVS_store_mse[i,] <- find_performance(mean_values, correct_em_values, correct_em)
    
    g_sd <-  a$statistics[grep("g\\[", rownames(a$statistics)), "SD"]
    treat_sd <- a$statistics[grep("d\\[", rownames(a$statistics)), "SD"]
    treat_sd <- treat_sd["d[2]"]
    names(treat_sd) <- "treat"
    sd_values <- c(g_sd, treat_sd)
    SSVS_store_sd[i,] <- find_performance2(sd_values, correct_em, continuous.cov)
  }
  
  bayesLASSO_store_mse_mean <- apply(bayesLASSO_store_mse, 2, mean)
  bayesLASSO_store_sd_mean <- apply(bayesLASSO_store_sd, 2, mean, na.rm = TRUE)
  SSVS_store_mse_mean <- apply(SSVS_store_mse, 2, mean)
  SSVS_store_sd_mean <- apply(SSVS_store_sd, 2, mean, na.rm = TRUE)
  
  result_matrix_mse <- matrix(NA, nrow = 2, ncol = 3)
  colnames(result_matrix_mse) <- c("false em mse", "true em mse","treatment mse")
  rownames(result_matrix_mse) <-  c("bayesLASSO", "SSVS")
  result_matrix_mse[1,] <- bayesLASSO_store_mse_mean
  result_matrix_mse[2,] <- SSVS_store_mse_mean
  
  result_matrix_sd <- matrix(NA, nrow = 2, ncol = 3)
  colnames(result_matrix_sd) <- c("continuous EM se", "binary EM se","treatment se")
  rownames(result_matrix_sd) <-  c("bayesLASSO", "SSVS")
  result_matrix_sd[1,] <- bayesLASSO_store_sd_mean
  result_matrix_sd[2,] <- SSVS_store_sd_mean
  
  cbind(result_matrix_mse, result_matrix_sd)
}