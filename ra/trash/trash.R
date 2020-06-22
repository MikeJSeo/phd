#trash
# Treatment
Treat <- read.csv("09 - tbl_fupbio.csv", sep = ";")
Treat <- as_tibble(Treat)

Treat2 <- Treat %>% 
  filter(fupno == 1) %>%
  select(LocalRandID, drugid) %>%
  distinct(LocalRandID, .keep_all = TRUE)

base3 <- base2 %>% left_join(Treat2, by = "LocalRandID") %>%
  filter(!is.na(drugid)) %>%
  filter(drugid %in% c(0, 5, 1394)) %>% 
  mutate(treat = recode(drugid, '0' = "No Trt", '5' = "RTX", '1394' = "TCZ"),
         treat1 = recode(drugid, '0' = '1', '5' = '2', '1394' = '3'))



#second stage for Approach IIIb

secondStage3 <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL, samples4 = NULL,
                         y5 = NULL, Omega5 = NULL, jags_file = NULL, W = NULL){
  
  r1 <- find_jags_data0(samples1)
  r2 <- find_jags_data0(samples2)
  r3 <- find_jags_data0(samples3)
  r4 <- find_jags_data0(samples4)
  
  
  if(!is.null(W)){
    if(sum(diag(W)) != 0){
      data_jags <-
        list(y1 = r1[[1]],
             y2 = r2[[1]],
             y3 = r3[[1]],
             y4 = r4[[1]],
             y5 = y5,
             Omega1 = r1[[2]],
             Omega2 = r2[[2]],
             Omega3 = r3[[2]],
             Omega4 = r4[[2]],
             Omega5 = Omega5,
             W = W) 
    } else{
      data_jags <-
        list(y1 = r1[[1]][1:10],
             y2 = r2[[1]][1:10],
             y3 = r3[[1]],
             y4 = r4[[1]],
             y5 = y5,
             Omega1 = r1[[2]][1:10,1:10],
             Omega2 = r2[[2]][1:10,1:10],
             Omega3 = r3[[2]],
             Omega4 = r4[[2]],
             Omega5 = Omega5) 
    }
    
  }
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta"), n.iter = 10000)
  
  return(samples)
}

#second stage for Approach IIIb - external

secondStage4 <- function(samples1 = NULL, samples2 = NULL, samples3 = NULL,
                         y4 = NULL, Omega4 = NULL, jags_file = NULL, W = NULL){
  
  r1 <- find_jags_data0(samples1)
  r2 <- find_jags_data0(samples2)
  r3 <- find_jags_data0(samples3)
  
  
  if(!is.null(W)){
    if(sum(diag(W)) != 0){
      data_jags <-
        list(y1 = r1[[1]],
             y2 = r2[[1]],
             y3 = r3[[1]],
             y4 = y4,
             Omega1 = r1[[2]],
             Omega2 = r2[[2]],
             Omega3 = r3[[2]],
             Omega4 = Omega4,
             W = W)    
    } else{
      data_jags <-
        list(y1 = r1[[1]][1:10],
             y2 = r2[[1]],
             y3 = r3[[1]],
             y4 = y4,
             Omega1 = r1[[2]][1:10,1:10],
             Omega2 = r2[[2]],
             Omega3 = r3[[2]],
             Omega4 = Omega4)
    }
    
  }
  mod <- jags.model(jags_file, data_jags, n.chains = 3, n.adapt = 1000)
  samples <- coda.samples(mod, variable.names = c("alpha", "beta", "gamma", "delta"), n.iter = 10000)
  
  return(samples)
}