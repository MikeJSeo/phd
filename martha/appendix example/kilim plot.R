library(ggplot2)
library(netmeta) #for NMA

# generate dataset
generateData <- function(logOR = NULL, p.ref.1, p.ref.2){
  ntreat <- 7 #number of treatments
  
  t1 <- rep(combn(7,2)[1,], each = 2) # arm 1 treatment
  t2 <- rep(combn(7,2)[2,], each = 2) # arm 2 treatment
  nstudy <- length(t1) # number of studies
  
  OR <- exp(logOR)
  
  studlab <- seq(nstudy)
  n1 <- n2 <- round(runif(nstudy,300,600)) #sample size for each study
  
  p.ref <- runif(nstudy,p.ref.1, p.ref.2) # study-specific probability of an event in treatment 1
  odds.ref <- p.ref / (1 - p.ref)
  
  # define probabilities per treatment, per study arm
  odds.t1 <- odds.t2 <- r1 <- r2 <- vector()
  for(j in 1:nstudy){
    odds.t1[j] <- odds.ref[j] * OR[t1[j]]
    odds.t2[j] <- odds.ref[j] * OR[t2[j]]
  }
  p.t1 <- odds.t1/(1+odds.t1)
  p.t2 <- odds.t2/(1+odds.t2)
  
  for(j in 1:nstudy){
    r1[j] <- rbinom(1, n1[j], p.t1[j])
    r2[j] <- rbinom(1, n2[j], p.t2[j])
  }
  
  data01 <- data.frame(studlab = studlab, drug = t1, outcome = r1, n = n1)
  data02 <- data.frame(studlab = studlab, drug = t2, outcome = r2, n = n2)
  
  data <- rbind(data01, data02)
  data <- data[order(data$studlab, data$drug),]
  rownames(data) <- 1:dim(data)[1]
  
  return(data)
}

# last two paramters are range for probability of an event in treatment 1
data1 <- generateData(logOR = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 0.1, 0.15)
data2 <- generateData(logOR = c(0, -0.1, -0.2, -0.3, -0.8, -0.9, -1.2), 0.15, 0.20)
data3 <- generateData(logOR = c(0, -3, 2, -0.5, 0.3, -0.5, 0), 0.10, 0.12)
data4 <- generateData(logOR = c(0, 0.2, 0.3, 0, 0, 0.2, 0.1), 0.05, 0.07)
data5 <- generateData(logOR = c(0, -0.2, -0.3, -0.4, -0.1, 0, 0), 0.10, 0.20)
store0 <- list(data1, data2, data3, data4, data5) #data for each outcome

# Fit network meta analysis (for 5 outcomes)
p1 = pairwise(treat = drug, event = outcome, n = n, studlab = studlab, data = data1, sm = "OR", allstudies=T)
result1 <- netmeta(p1)

p2 = pairwise(treat = drug, event = outcome, n = n, studlab = studlab, data = data2, sm = "OR", allstudies=T)
result2 <- netmeta(p2)

p3 = pairwise(treat = drug, event = outcome, n = n, studlab = studlab, data = data3, sm = "OR", allstudies=T)
result3 <- netmeta(p3)

p4 = pairwise(treat = drug, event = outcome, n = n, studlab = studlab, data = data4, sm = "OR", allstudies=T)
result4 <- netmeta(p4)

p5 = pairwise(treat = drug, event = outcome, n = n, studlab = studlab, data = data5, sm = "OR", allstudies=T)
result5 <- netmeta(p5)

store <- list(result1, result2, result3, result4, result5) #NMA result


############

final <- data.frame() # data frame to store results

for(i in 1:5){

data <- store0[[i]]
net1 <- store[[i]]

# treatment estimate (odds ratio) from netmeta
OR.pla <- data.frame("drug" = colnames(net1$TE.random))
OR.pla$logOR <- net1$TE.random[,1]
OR.pla$seTE <- net1$seTE.random[,1]
OR.pla$OR <- exp(OR.pla$logOR)
OR.pla <- OR.pla[-which(OR.pla$drug == 1),] #exclude comparison with placebo itself which is 0

# meta analysis of event rates in placebo
meta.pla = metaprop(event = round(data$outcome[data$drug==1]), n = data$n[data$drug==1], method = "GLMM")
rate.pla = exp(meta.pla$TE.fixed)/(1+exp(meta.pla$TE.fixed))
odds.pla=rate.pla/(1-rate.pla)

# calculate event rate for treatment
OR.pla$event.rate <- round(OR.pla$OR*odds.pla/(1+OR.pla$OR*odds.pla),digits=3)

# Calculate Zscore accounting for clinically important risk difference
clinically.important.RD.0 <- 0.0  
risk.drugs.0 <- clinically.important.RD.0+rate.pla
OR.import.0 <- risk.drugs.0/(1-risk.drugs.0)/((rate.pla)/(1-rate.pla))
OR.pla$Zscore.0 <- (OR.pla$logOR-log(OR.import.0))/OR.pla$seTE

outcome.result <- data.frame(outcome = paste("outcome", i), drug = OR.pla$drug, Zscore = OR.pla$Zscore.0,
                             event.rate = round(OR.pla$event.rate*100,1), rate.pla = rate.pla, 
                             logOR = OR.pla$logOR, seTE = OR.pla$seTE)
final <- rbind(final, outcome.result)

}

final_data <- final
final_data <- final_data[,c("outcome", "drug", "Zscore", "event.rate")]

#add in control treatment (i.e. treatment = 1)
add_placebo <- data.frame(outcome = paste("outcome", 1:5), drug = rep(1, 5), Zscore = rep(NA, 5), event.rate = round(unique(final$rate.pla)*100,1) )
final_data <- rbind(final_data, add_placebo)

final_data$Zscore2 <- final_data$Zscore #truncated zscore
final_data$Zscore2[final_data$Zscore2 < -2.5] = -2.5
final_data$Zscore2[final_data$Zscore2 > 2.5] = 2.5

add_percent <- function(x){if(!is.na(x)){paste0(x, "%")} else{x}}
final_data$event.rate <- sapply(final_data$event.rate, add_percent)

# order the drugs accordingly
final_data$drug <- paste("treatment", final_data$drug)
final_data$drug <- factor(final_data$drug, level = paste("treatment", 7:1), ordered = TRUE)

############## heatplots

ggplot(final_data, aes(outcome, drug)) + 
  geom_tile(aes(fill = round(Zscore2,2)), colour = "white") + 
  geom_text(aes(label= event.rate), size = 6) +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", na.value = "lightskyblue1", breaks = c(-2.326348, -1.281552, 0, 1.281552, 2.326348), limits = c(-2.5, 2.5), labels = c("p < 0.01", "p = 0.1", "p = 1.00", "p = 0.1", "p < 0.01"))+
  labs(x = "",y = "") +
  theme(legend.title = element_blank(),axis.text.x = element_text(angle=30,hjust=1,vjust=1.0, size = 14),axis.text.y = element_text(size = 14),legend.position = "left", legend.text = element_text(size = 14))


