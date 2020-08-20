
findPrediction <- function(data, result, ncov = 9) {
  
  options(na.action='na.pass')
  abd <- model.matrix(~ female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS + treat, data)
  abd_scaled <- abd
  abd_scaled[,c(-1,-11,-12)] <- apply(abd[,c(-1,-11,-12)], 2, scale)
  
  g2 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 2),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  g3 <- model.matrix(~ (female + age + duration + baseBMI + baseRF + n_prev_dmards_antiTNF + baseHAQ + baseESR + baseDAS)*I(data$treat == 3),as.data.frame(abd_scaled))[,-c(1:(ncov+2))]
  
  XX <- as.matrix(cbind(abd_scaled, g2, g3))
  X <- XX[complete.cases(XX),]
  
  coefs <- summary(result)[[1]][,"Mean"]
  coefs <- coefs[coefs != 0]
  
  pred <- X %*% coefs
  y <- data$DAS28[complete.cases(XX)]
  treat <- data$treat[complete.cases(XX)]
  
  return(list(y = y, pred = pred, treat = treat, X = X, coefs = coefs))
}


findPerformance <- function(pred, y){
  

  err_mse <- (pred - y)^2
  err_mse <- err_mse[!is.na(err_mse)]
  mse <- mean(err_mse, na.rm = TRUE)
    
  err_bias <- pred - y
  err_bias <- err_bias[!is.na(err_bias)]
  bias <- mean(err_bias, na.rm = TRUE)

  list(mse = mse, bias = bias)
}
