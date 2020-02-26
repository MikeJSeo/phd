############## Martha analysis
### drawing heatplots: edited by Michael Seo

#### R package used
library("readxl")
library("netmeta") # for pairwise, netmeta function
library("ggplot2")
library("grid") # for layering forest plots..


#### Data cleaning
#setwd("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Martha/data") #change working directory
#original <- read_excel("For Orestis 20.11.19.xlsx",  col_names = FALSE)

setwd("C:/Users/ms19g661/Desktop")
#setwd("C:/Users/mike/Desktop")
original <- read_excel("Data Martha 06.10.19.xlsx",  col_names = FALSE)
original <- as.data.frame(original)
original[original[,4] == "Placebo" & !is.na(original[,4]),4] <- "placebo"
original[!is.na(original[,1]) & original[,1] == "Jefferson2000 (29060/785)" & original[,5] == "paroxetine CR", 4] <- "paroxetine"

#setwd("C:/Users/mike/Desktop/Github/phd/martha")
setwd("~/GitHub/phd/martha")
source("useful functions for martha.R")


treatment.selection <- c("vortioxetine", "venlafaxine", "reboxetine", "mirtazapine", "fluoxetine", "duloxetine", "amitriptyline", "placebo")
outcome.selection <- c("NAUSEA", "HEADACHE", "DRY MOUTH", "INSOMNIA", "SEXUAL DYSFUNCTION","Diarrhoea", "SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")  
severe_ae <- c("SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")

outcome.renamed <- outcome.selection
outcome.renamed[outcome.renamed == "Diarrhoea"] <- "DIARRHOEA"


#########################  preliminary cleaning
final <- data.frame()
placebo.rate.store <- rep(NA, length(outcome.renamed))

studyID_all <- vector(mode = "character")

for(i in 1:length(outcome.selection)){
print(paste("Analyzing", outcome.selection[i]))
outcome <- outcome.selection[i]
data <- split_data(original, outcome) 
data <- data[data$Dose_range=="Licensed",]
data <- remove.onearm(data)
data <- data[data$Drug %in% treatment.selection,]
data <- data[data$No_randomised>=data$martha_outcome,]
data <- data[,c("StudyID", "Drug", "No_randomised", "martha_outcome")]
data <- aggregate(x = data[,c("No_randomised", "martha_outcome")], by = list(data[,"Drug"], data[,c("StudyID")]), FUN = sum)
colnames(data) <- c("Drug", "StudyID", "No_randomised", "martha_outcome")

# studies used
studyID_all <- c(studyID_all, data$StudyID)

# Fit network meta analysis
p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data, sm = "OR", allstudies=T)
net1 <- netmeta(p1)

# treatment estimate (odds ratio) from netmeta
OR.pla <- data.frame("drug" = colnames(net1$TE.random))
OR.pla$logOR <- net1$TE.random[,"placebo"]
OR.pla$seTE <- net1$seTE.random[,"placebo"]
OR.pla$OR=exp(OR.pla$logOR)
OR.pla$upperOR=exp(OR.pla$logOR+1.96*OR.pla$seTE)
OR.pla$lowerOR=exp(OR.pla$logOR-1.96*OR.pla$seTE)
OR.pla <- OR.pla[-which(OR.pla$drug == "placebo"),] #exclude comparison with placebo itself which is 0

# meta analysis of event rates in placebo
meta.pla = metaprop(event = round(data$martha_outcome[data$Drug=="placebo"]), n = data$No_randomised[data$Drug=="placebo"], prediction = TRUE, method = "GLMM")
rate.pla = exp(meta.pla$TE.fixed)/(1+exp(meta.pla$TE.fixed))
odds.pla=rate.pla/(1-rate.pla)

# calculate event rate for treatment using the formula
OR.pla$event.rate=round(OR.pla$OR*odds.pla/(1+OR.pla$OR*odds.pla),digits=3)
#OR.pla$CI.lower=round(OR.pla$lowerOR*odds.pla/(1+OR.pla$lowerOR*odds.pla),digits=3)
#OR.pla$CI.upper=round(OR.pla$upperOR*odds.pla/(1+OR.pla$upperOR*odds.pla),digits=3)

### calculate adjusted Z-scores for heatplot
clinically.important.RD.0=0.0  ##### clinically important risk difference for this outcome
risk.drugs.0=clinically.important.RD.0+rate.pla
OR.import.0=risk.drugs.0/(1-risk.drugs.0)/((rate.pla)/(1-rate.pla))
OR.pla$Zscore.0=(OR.pla$logOR-log(OR.import.0))/OR.pla$seTE

clinically.important.RD.1=0.05  
risk.drugs.1=clinically.important.RD.1+rate.pla
OR.import.1=risk.drugs.1/(1-risk.drugs.1)/((rate.pla)/(1-rate.pla))
OR.pla$Zscore.1=(OR.pla$logOR-log(OR.import.1))/OR.pla$seTE

Zscore.1 <- 
if(outcome %in% severe_ae){
  OR.pla$Zscore.0
} else {
  OR.pla$Zscore.1
}

# no clinical difference
aa <- data.frame(outcome = rep(outcome.renamed[i], length(OR.pla$drug)), drug = OR.pla$drug, OR = OR.pla$OR,
                 Zscore.0 = OR.pla$Zscore.0,  Zscore.1 = Zscore.1, 
                 event.rate = round(OR.pla$event.rate*100), rate.pla = rate.pla, logOR = OR.pla$logOR, seTE = OR.pla$seTE)

placebo.rate.store[i] <- round(rate.pla*100)
final <- rbind(final, aa)

}

# find unique studyID
length(unique(studyID_all))

final_data <- final
final_data$Zscore <- final$Zscore.0 #specify which z score we want: differ on clinical difference
#final_data$Zscore <- final$Zscore.1
final_data <- final_data[,c("outcome", "drug", "Zscore", "event.rate")]

#add in placebo
bb <- data.frame(outcome = outcome.renamed, drug = rep("placebo", length(outcome.renamed)), Zscore = rep(NA, length(outcome.renamed)), event.rate = placebo.rate.store )
final_data <- rbind(final_data, bb)

final_data$drug <- factor(final_data$drug,
       level = treatment.selection,ordered = TRUE)

final_data$Zscore2 <- final_data$Zscore #truncated zscore
final_data$Zscore2[final_data$Zscore2 < -2.5] = -2.5
final_data$Zscore2[final_data$Zscore2 > 2.5] = 2.5

aa <- function(x){
  if(!is.na(x)){
    paste0(x, "%")
  } else{
    x
  }
}
final_data$event.rate <- sapply(final_data$event.rate, aa)

# fill in missing combination
dat2 <- with(final_data, expand.grid(outcome = levels(outcome), drug = levels(drug)))
final_data2 <- merge(final_data, dat2, all.y = TRUE)

final_data2[final_data2$drug != "placebo" & is.na(final_data2$Zscore), "Zscore2"] <- 0
final_data2[final_data2$drug != "placebo" & is.na(final_data2$Zscore), "event.rate"] <- "-"

############## heatplots
ggplot(final_data2, aes(outcome, drug)) + geom_tile(aes(fill = round(Zscore2,1)), colour = "white") + 
  geom_text(aes(label= event.rate), size = 6) +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", na.value = "lightskyblue1", 
                       breaks = c(-2.326348, -1.281552, 0, 1.281552, 2.326348), limits = c(-2.5, 2.5),
                       labels = c("p < 0.01", "p = 0.1", "p = 1.00", "p = 0.1", "p < 0.01")) +
  labs(x = "",y = "") +
  theme(legend.title = element_blank(),
        legend.position = "left",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_discrete(position = "top") 
  


############# forest plots
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 3)))
for(i in 1:9){
  #print(paste("Analyzing", outcome.selection[i]))
  outcome <- outcome.selection[i]
  data <- split_data(original, outcome) 
  data <- data[data$Dose_range=="Licensed",]
  data <- remove.onearm(data)
  data <- data[data$Drug %in% treatment.selection,]
  data <- data[data$No_randomised>=data$martha_outcome,]
  data <- data[,c("StudyID", "Drug", "No_randomised", "martha_outcome")]
  data <- aggregate(x = data[,c("No_randomised", "martha_outcome")], by = list(data[,"Drug"], data[,c("StudyID")]), FUN = sum)
  colnames(data) <- c("Drug", "StudyID", "No_randomised", "martha_outcome")
  
  # Fit network meta analysis
  p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data, sm = "OR", allstudies=T)
  net1 <- netmeta(p1, prediction = T)
  
  sort.order <- colnames(net1$TE.random)
  sort.order <- sort.order[sort.order != "placebo"]
  sort.order <- c("placebo", sort.order)
  
  pospos <- ifelse(i %% 3 == 0, 3, i %%3)
  pushViewport(viewport(layout.pos.col = ceiling(i/3), layout.pos.row = pospos))
  forest(net1, sortvar = TE, reference.group = "placebo", smlab = outcome.renamed[i],
         label.right = "Favours placebo", label.left = "Favours drug", digits=2, xlim=c(0.1,15), new = FALSE)
  popViewport()
} 