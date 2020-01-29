############## Martha analysis

#### R package used
library("readxl")
library("netmeta") # for pairwise, netmeta function

#### Data cleaning
#setwd("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Martha/data") #change working directory
#original <- read_excel("For Orestis 20.11.19.xlsx",  col_names = FALSE)

setwd("C:/Users/ms19g661/Desktop")
original <- read_excel("Data Martha 06.10.19.xlsx",  col_names = FALSE)
original <- as.data.frame(original)
original[original[,4] == "Placebo" & !is.na(original[,4]),4] <- "placebo"
original[!is.na(original[,1]) & original[,1] == "Jefferson2000 (29060/785)" & original[,5] == "paroxetine CR", 4] <- "paroxetine"

setwd("~/GitHub/phd/martha")
source("useful functions for martha.R")

treatment.selection <- c("placebo", "amitriptyline", "mirtazapine", "vortioxetine", "duloxetine", "agolematine", "venlafaxine", "duloxetine")
#outcome.selection <- c("NAUSEA", "Headache", "Dry mouth", "insomnia", "sedation", "diahrroea", "hyperhidrosis", "Arrhythmia", "suicidal ideation", "death")  
  

######################### 
outcome <- "NAUSEA"
data1 <- split_data(original, outcome) 
data1 <- data1[data1$Dose_range=="Licensed",]
data2 <- remove.onearm(data1)
data2 <- data2[data2$Drug %in% treatment.selection,]

# remove studies with events > no_randomized
problem_events <- which(data2$No_randomised<data2$martha_outcome)
print(paste("Total number of studies with number randomized being smaller than event rates: ",length(unique(data2$StudyID[problem_events]))))
print(paste("Removed the following studies:", unique(data2$StudyID[problem_events])))
data2 <- data2[data2$No_randomised>=data2$martha_outcome,]
data3 <- data2[,c("StudyID", "Drug", "No_randomised", "martha_outcome")]

data4 <-aggregate(x = data3[,c("No_randomised", "martha_outcome")], by = list(data3[,"StudyID"], data3[,c("Drug")]), FUN = sum)
colnames(data4) <- c("StudyID","Drug", "No_randomised", "martha_outcome")

#p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data4, sm = "OR", allstudies=T)
p1 <- pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data4, sm = "OR")

## if network disconnected
#netconnection(treat1, treat2, studlab, data=p1)
#p1=p1[p1$treat1!="sertraline"&p1$treat1!="trazodone",]

net1 <- netmeta(p1, prediction = T)

# treatment estimate (odds ratio) from netmeta
options(digits=3)
OR.pla <- data.frame("drug"=colnames(net1$TE.random), "logOR"=NA, "seTE"=NA)
OR.pla$logOR <- net1$TE.random[,"placebo"]
OR.pla$seTE <- net1$seTE.predict[,"placebo"]
OR.pla$OR <- exp(OR.pla$logOR)
OR.pla$upperOR <- exp(OR.pla$logOR+1.96*OR.pla$seTE)
OR.pla$lowerOR <- exp(OR.pla$logOR-1.96*OR.pla$seTE)
OR.pla <- OR.pla[-which(OR.pla$drug == "placebo"),] 


# meta analysis of event rates in placebo
#meta.pla = metaprop(event = round(data3$martha_outcome[data3$Drug=="placebo"]), n = data3$No_randomised[data3$Drug=="placebo"], prediction = TRUE)  
meta.pla = metaprop(event = round(data3$martha_outcome[data3$Drug=="placebo"]), n = data3$No_randomised[data3$Drug=="placebo"], prediction = TRUE, method = "GLMM")
rate.pla = exp(meta.pla$TE.random)/(1+exp(meta.pla$TE.random))
lci.pla = exp(meta.pla$lower.random)/(1+exp(meta.pla$lower.random))
uci.pla =  exp(meta.pla$upper.random)/(1+exp(meta.pla$upper.random))
lb.pla = exp(meta.pla$lower.predict)/(1+exp(meta.pla$lower.predict))
ub.pla = exp(meta.pla$upper.predict)/(1+exp(meta.pla$upper.predict))
print(paste("Event rate in placebo equal to ", 
            round(rate.pla, digits = 3)," (95% CI ",round(lci.pla,digits=3), " to ", round(uci.pla,digits=3) ,")." ,sep=""))

print(paste("95% prediction interval (",round(lb.pla,digits=3), " to ", round(ub.pla,digits=3) ,")." ,sep=""))
#rate.pla=sum(data3$martha_outcome[data3$Drug=="placebo"])/sum(data3$No_randomised[data3$Drug=="placebo"])
odds.pla=rate.pla/(1-rate.pla)

OR.pla$event.rate=round(OR.pla$OR*odds.pla/(1+OR.pla$OR*odds.pla),digits=3)
OR.pla$CI.lower=round(OR.pla$lowerOR*odds.pla/(1+OR.pla$lowerOR*odds.pla),digits=3)
OR.pla$CI.upper=round(OR.pla$upperOR*odds.pla/(1+OR.pla$upperOR*odds.pla),digits=3)
event.rates=OR.pla[,which(colnames(OR.pla) %in% c("drug","event.rate","CI.lower","CI.upper"))]
event.rates$rate=paste(event.rates$event.rate," [",event.rates$CI.lower,", ",event.rates$CI.upper,"]",sep = "")

print(paste("For an event rate in placebo equal to the average event rate (i.e. fixed to ",
            round(rate.pla, digits = 3),") the estimated event rates for each drug are as follows", sep=""))
#print(event.rates[,c(1,5)], digits = 2)

write.xlsx(event.rates,file=paste(outcome,".xlsx"), sheetName = "event.rates", append = T)  

placebo.rates=data.frame("rate"=rate.pla, "lowerCI"=lci.pla, "upperCI"=uci.pla, "UpperPre"=lb.pla, "LowerPre"=ub.pla)
write.xlsx(placebo.rates,file=paste(outcome,".xlsx"), sheetName = "placebo.rates", append = T)  


