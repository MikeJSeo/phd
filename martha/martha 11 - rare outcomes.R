### load packages #######

rm(list=ls())
library("readxl")
library(netmeta)
library(meta)
library(xlsx)
library(ggplot2)
library(ggpmisc)
library(grid)
library(gridExtra)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
### load data ######


setwd("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Martha/data") #change working directory
original <- read_excel("Data Martha 06.10.19.xlsx",  col_names = FALSE)
original <- as.data.frame(original)
original$X__4[original$X__4=="Placebo"]="placebo"

#if(original[837, 4] == "*") original[837,4] <- "paroxetine" #!!!!!!!!!!this is wrong; row number changed in the new dataset
original[!is.na(original[,1]) & original[,1] == "Jefferson2000 (29060/785)" & original[,5] == "paroxetine CR", 4] <- "paroxetine"



#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
### functions that will be used ####
split_data <- function(martha, outcome){
  
  
  # only keep necessary characteristic variables we need for now
  characteristics <- which(martha[1,] %in% c("StudyID", "Drug", "No_randomised", "Age_mean", "Scale", "Mean", "Dose_intended_min","Dose_range" ,"%_Female", "Dosing_schedule"))
  martha_characteristics <- martha[, characteristics]
  colnames(martha_characteristics) <- martha[1, characteristics]
  
  # extract outcome; only keep trials that are not all '*'; also remove the first 6 row headers
  martha_outcome <- as.matrix(martha[,martha[1,] %in% outcome])
  ind <- apply(martha_outcome, 1, function(x) all(x == "*"))
  #ind[1:3] <- TRUE #!!!!!! this is wrong too since they changed the data
  ind[1:6] <- TRUE
  
  martha_outcome <- as.matrix(martha_outcome[!ind, ])
  martha_characteristics <- martha_characteristics[!ind,]
  
  # change No_randomised to numeric
  martha_characteristics["No_randomised"][martha_characteristics["No_randomised"] == "*"] <- 0   #some values are missing - denoted with *
  martha_characteristics["No_randomised"] <- sapply(martha_characteristics[,"No_randomised"], as.numeric)
  
  # change the * to 0 and add the same columns up; change the values to numeric
  martha_outcome[martha_outcome == "*"] <- 0
  martha_outcome <- apply(martha_outcome, 2, function(x){x[grepl('S ', x)] <- NA; x}) #delete some weird values (i.e. "S 0" for Suicidal Ideation
  martha_outcome <- apply(martha_outcome, 2, as.numeric)
  martha_outcome <- apply(martha_outcome, 1, sum, na.rm = TRUE)
  
  # combine the characteristics and outcome
  martha_combined <- cbind(martha_characteristics, martha_outcome)
  # print(sum(martha_combined$martha_outcome, na.rm = TRUE)) #check this number with what we have calculated previously
  
  return(martha_combined)
}

# function to remove one arm trial; think the netmeta automatically gets rid of one arm
remove.onearm <- function(data, warn=TRUE) {
  # Remove 1-arm studies
  sel <- as.logical(sapply(data[,'StudyID'],
                           function(study) {sum(data[,'StudyID'] == study) > 1
                           }))
  
  if (warn && !all(sel)) {
    warning(paste("Removed", sum(!sel), "one-arm studies:",
                  paste(data[!sel,'StudyID'], collapse=", ")))
  }
  data[sel, , drop=FALSE]
}

# function to harmonize baseline
harmonize_baseline <- function(data){
  
  data$Mean=round(as.numeric(data$Mean))
  baseline <- rep(NA, length(data$Mean))
  
  #let's first extract HAMD21
  HAMD21_values = data[data$Scale == "HAMD21",]$Mean 
  HAMD21_values[HAMD21_values == 17] = 16
  HAMD21_values[HAMD21_values == 18] = 17
  HAMD21_values[HAMD21_values == 19] = 17
  HAMD21_values[HAMD21_values == 20] = 18
  HAMD21_values[HAMD21_values == 21] = 19
  HAMD21_values[HAMD21_values == 22] = 19
  HAMD21_values[HAMD21_values == 23] = 20
  HAMD21_values[HAMD21_values == 24] = 21
  HAMD21_values[HAMD21_values == 25] = 22
  HAMD21_values[HAMD21_values == 26] = 23
  HAMD21_values[HAMD21_values == 27] = 24
  HAMD21_values[HAMD21_values == 28] = 25
  HAMD21_values[HAMD21_values == 29] = 26.5
  HAMD21_values[HAMD21_values == 30] = 26.5
  HAMD21_values[HAMD21_values == 31] = 26.5
  HAMD21_values[HAMD21_values == 32] = 28
  HAMD21_values[HAMD21_values == 33] = 29
  HAMD21_values[HAMD21_values == 34] = 29
  HAMD21_values[HAMD21_values == 35] = 30.5
  
  # Then extract HAMD24
  HAMD24_values = data[data$Scale == "HAMD24",]$Mean 
  HAMD24_values[HAMD24_values == 24] = 18
  HAMD24_values[HAMD24_values == 25] = 19
  HAMD24_values[HAMD24_values == 26] = 19
  HAMD24_values[HAMD24_values == 27] = 20
  HAMD24_values[HAMD24_values == 28] = 20
  HAMD24_values[HAMD24_values == 29] = 21
  HAMD24_values[HAMD24_values == 30] = 22
  HAMD24_values[HAMD24_values == 31] = 23
  HAMD24_values[HAMD24_values == 32] = 23
  HAMD24_values[HAMD24_values == 33] = 24
  HAMD24_values[HAMD24_values == 34] = 25
  HAMD24_values[HAMD24_values == 35] = 26.5
  HAMD24_values[HAMD24_values == 36] = 26.5
  HAMD24_values[HAMD24_values == 37] = 26.5
  HAMD24_values[HAMD24_values == 38] = 26.5
  
  # Then extract MADRS
  MADRS_values = data[data$Scale == "MADRS",]$Mean 
  MADRS_values[MADRS_values == 24] = 19
  MADRS_values[MADRS_values == 25] = 19
  MADRS_values[MADRS_values == 26] = 20
  MADRS_values[MADRS_values == 27] = 22
  MADRS_values[MADRS_values == 28] = 22
  MADRS_values[MADRS_values == 29] = 23
  MADRS_values[MADRS_values == 30] = 23
  MADRS_values[MADRS_values == 31] = 24
  MADRS_values[MADRS_values == 32] = 25
  MADRS_values[MADRS_values == 33] = 25
  MADRS_values[MADRS_values == 34] = 26
  MADRS_values[MADRS_values == 35] = 27
  MADRS_values[MADRS_values == 36] = 28
  MADRS_values[MADRS_values == 37] = 29
  MADRS_values[MADRS_values == 38] = 29
  
  baseline[data$Scale == "HAMD17"] = data[data$Scale=="HAMD17",]$Mean
  baseline[data$Scale == "HAMD21"] = HAMD21_values
  baseline[data$Scale == "HAMD24"] = HAMD24_values
  baseline[data$Scale == "MADRS"] = MADRS_values
  data$baseline <- baseline  
  
  # What about other ones?
  
  return(data)
}

# function to transform Dose_intended minimum
transform_dose <- function(data){
  #data=data[!is.na(as.numeric(data$Dose_intended_min)),]
  data$Dose_intended_min[data$Drug=="placebo"]=1
  data$Dose_intended_min[data$Drug=="agomelatine"]=as.numeric(data$Dose_intended_min[data$Drug=="agomelatine"])/25
  data$Dose_intended_min[data$Drug=="amitriptyline"]=as.numeric(data$Dose_intended_min[data$Drug=="amitriptyline"])/75
  data$Dose_intended_min[data$Drug=="bupropion"]=as.numeric(data$Dose_intended_min[data$Drug=="bupropion"])/300
  data$Dose_intended_min[data$Drug=="citalopram"]=as.numeric(data$Dose_intended_min[data$Drug=="citalopram"])/20
  data$Dose_intended_min[data$Drug=="clomipramine"]=as.numeric(data$Dose_intended_min[data$Drug=="clomipramine"])/30
  data$Dose_intended_min[data$Drug=="desvenlafaxine"]=as.numeric(data$Dose_intended_min[data$Drug=="desvenlafaxine"])/50
  data$Dose_intended_min[data$Drug=="duloxetine"]=as.numeric(data$Dose_intended_min[data$Drug=="duloxetine"])/40
  data$Dose_intended_min[data$Drug=="escitalopram"]=as.numeric(data$Dose_intended_min[data$Drug=="escitalopram"])/10
  data$Dose_intended_min[data$Drug=="fluoxetine"]=as.numeric(data$Dose_intended_min[data$Drug=="fluoxetine"])/20
  data$Dose_intended_min[data$Drug=="fluvoxamine"]=as.numeric(data$Dose_intended_min[data$Drug=="fluvoxamine"])/50
  data$Dose_intended_min[data$Drug=="levomilnacipran"]=as.numeric(data$Dose_intended_min[data$Drug=="levomilnacipran"])/40
  data$Dose_intended_min[data$Drug=="milnacipran"]=as.numeric(data$Dose_intended_min[data$Drug=="milnacipran"])/50
  data$Dose_intended_min[data$Drug=="mirtazapine"]=as.numeric(data$Dose_intended_min[data$Drug=="mirtazapine"])/15
  data$Dose_intended_min[data$Drug=="nefazodone"]=as.numeric(data$Dose_intended_min[data$Drug=="nefazodone"])/300
  data$Dose_intended_min[data$Drug=="paroxetine"]=as.numeric(data$Dose_intended_min[data$Drug=="paroxetine"])/20
  data$Dose_intended_min[data$Drug=="reboxetine"]=as.numeric(data$Dose_intended_min[data$Drug=="reboxetine"])/8
  data$Dose_intended_min[data$Drug=="sertraline"]=as.numeric(data$Dose_intended_min[data$Drug=="sertraline"])/50
  data$Dose_intended_min[data$Drug=="trazodone"]=as.numeric(data$Dose_intended_min[data$Drug=="trazodone"])/150
  data$Dose_intended_min[data$Drug=="venlafaxine"]=as.numeric(data$Dose_intended_min[data$Drug=="venlafaxine"])/75
  data$Dose_intended_min[data$Drug=="vilazodone"]=as.numeric(data$Dose_intended_min[data$Drug=="vilazodone"])/20
  data$Dose_intended_min[data$Drug=="vortioxetine"]=as.numeric(data$Dose_intended_min[data$Drug=="vortioxetine"])/5
  data$Dose_intended_min=as.numeric(data$Dose_intended_min)
  return(data)}

# function to make study-level characterististics for age, gender, dose, severity, 
make_study_char <- function(data){
  data$Age_mean=as.numeric(data$Age_mean)
  data$`%_Female`=as.numeric(data$`%_Female`)
  x0 <- aggregate(data[,c("Age_mean", "%_Female", "baseline")], list( StudyID = data[,"StudyID"]), mean) 
  g1=aggregate(data[,c("Dosing_schedule")], list( StudyID = data[,"StudyID"]), unique) 
  g1$dose.fixed.all.arms=NA
  
  for(i in 1:length(g1$StudyID)){   g1$dose.fixed.all.arms[i]=paste(g1$x[[i]], collapse = '')}
  g1$dose.fixed.all.arms[g1$dose.fixed.all.arms=="Fixed"]="1"
  g1$dose.fixed.all.arms[g1$dose.fixed.all.arms=="Flexible"|g1$dose.fixed.all.arms=="FlexibleFixed"|g1$dose.fixed.all.arms=="FixedFlexible"]="0"
  g1$dose.fixed.all.arms[g1$dose.fixed.all.arms!="1"&g1$dose.fixed.all.arms!="0"]=NA
  
  data1=data[,c(1,2,3,6,  11)]
  
  x1=merge(data1,g1, by.x=c("StudyID"), by.y = c("StudyID"))
  x2=merge(x1,x0, by.x=c("StudyID"), by.y = c("StudyID"))
  x2=x2[,-c(6)]
  return(x2)
}

# function to do meta analysis after grouping treatments into one
grouped_meta_analysis <- function(data){
  
  data$Drug <- ifelse(data$Drug == "placebo", "placebo", "treatment")
  # combine identical treatments together
  meta <- aggregate(data[,c("No_randomised", "martha_outcome")], list(Drug = data[,"Drug"], StudyID = data[,"StudyID"]), sum)
  
  p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = meta, sm = "OR")
  model1 = metabin(event.e = martha_outcome2, n.e = No_randomised2, event.c = martha_outcome1, n.c = No_randomised1,
                   data = p1, studlab = StudyID, sm = "OR")
  return.list=list("model1"=model1)
  return(return.list)
}

# function to collapse same drug arms
collapse_arms <- function(data){
  data3.2=data[,1:2]
  data3.3=data[,c(3,5)]
  data3.4=aggregate( x=data3.3, by=list(data3.2$StudyID, data3.2$Drug), FUN=sum)
  colnames(data3.4)=c("StudyID","Drug", "No_randomised", "martha_outcome")
  
  data3.4$age=NA; data3.4$female=NA; data3.4$baseline=NA; data3.4$dose.fixed.all=NA
  for(i in 1:length(data3.4$StudyID)){
    k=which(data$StudyID==data3.4$StudyID[i])
    data3.4$age[i]=data$Age_mean[k[1]]
    data3.4$female[i]=data$`%_Female`[k[1]]
    data3.4$baseline[i]=data$baseline[k[1]]
    data3.4$dose.fixed.all[i]=data$dose.fixed.all[k[1]]
  }
  
  return.list=list("data_collapsed"=data3.4)
  return(return.list)
}

# function to perform meta-analysis in comparisons with more than 10 studies
metan_manystudies <- function(data){
  data3.4=data
  data3.4$Drug[data3.4$Drug=="placebo"]=" placebo"
  data3.4=data3.4[order(data3.4$StudyID,data3.4$Drug),]
  p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data3.4, sm = "OR")
  p1$comparison=paste(p1$Drug1, "-",p1$Drug2)
  comparisons=table(p1$comparison)[table(p1$comparison)>9]
  by.comparison=list()
  for(i in 1:length(comparisons)){
    by.comparison[[i]]=metabin(event.e = martha_outcome2, n.e = No_randomised2, event.c = martha_outcome1, n.c = No_randomised1,
                               data = p1[p1$comparison==names(comparisons)[i],], studlab = StudyID, sm = "OR")
  }
  
  results=data.frame("comparisons"=names(comparisons))
  for(i in 1:length(comparisons)){
    results$Number.of.studies[i]=by.comparison[[i]]$k
    results$OR[i]=round(exp(by.comparison[[i]]$TE.random),2)
    results$lower.CI[i]=round(exp(by.comparison[[i]]$lower.fixed),2)
    results$upper.CI[i]=round(exp(by.comparison[[i]]$upper.fixed),2)
    results$tau.sq[i]=round((by.comparison[[i]]$tau)^2,2)
    results$I.sq[i]=round(by.comparison[[i]]$I2,2)
  }
  
  
  return(results)
}

# function to draw prediction interval
plot.pred=function(net1, outcome, xlow, xhigh){
  options(digits=3)
  drug=colnames(net1$TE.random)
  mean=exp(net1$TE.random[,which(colnames(net1$TE.random)=="placebo")])
  mean=format(round(mean, 2), nsmall = 2)
  CI.upper=exp(net1$upper.random[,which(colnames(net1$TE.random)=="placebo")])
  CI.upper=format(round(CI.upper, 2), nsmall = 2)
  CI.lower=exp(net1$lower.random[,which(colnames(net1$TE.random)=="placebo")])
  CI.lower=format(round(CI.lower, 2), nsmall = 2)
  PI.lower=exp(net1$lower.predict[,which(colnames(net1$TE.random)=="placebo")])
  PI.lower=format(round(PI.lower, 2), nsmall = 2)
  PI.upper=exp(net1$upper.predict[,which(colnames(net1$TE.random)=="placebo")])
  PI.upper=format(round(PI.upper, 2), nsmall = 2)
  prediction=data.frame("drug"=c(rep(0,2*length(drug))), "mean"=c(rep(0,2*length(drug))),"LowerLimit"=c(rep(0,2*length(drug))),
                        "UpperLimit"=c(rep(0,2*length(drug))))
  
  for(k in 1:length(drug)){
    i=order(netrank(net1)$Pscore.random)[k]
    prediction$drug[2*k]=drug[i]
    prediction$drug[2*k-1]=drug[i]
    
    prediction$mean[2*k-1]=(mean[i])
    prediction$mean[2*k]=(mean[i])
    prediction$LowerLimit[2*k]=(CI.lower[i])
    prediction$LowerLimit[2*k-1]=(PI.lower[i])
    prediction$UpperLimit[2*k]=(CI.upper[i])
    prediction$UpperLimit[2*k-1]=(PI.upper[i])
  }
  prediction$mean=as.numeric(prediction$mean)
  prediction$LowerLimit=as.numeric(prediction$LowerLimit)
  prediction$UpperLimit=as.numeric(prediction$UpperLimit)
  
  # remove placebo
  prediction=prediction[prediction$drug!="placebo"&prediction$drug!="placebo PI",]
  # write down the PI
  prediction$`95% CI`=paste(prediction$mean," [",prediction$LowerLimit,", ",prediction$UpperLimit,"]", sep="")
  prediction$`95% PI`=paste(" [",prediction$LowerLimit,", ",prediction$UpperLimit,"]", sep="")
  for(i in 1:(length(prediction$drug)/2)){
    prediction$`95% CI`[2*i-1]=""
    prediction$`95% PI`[2*i]=""
    prediction$type[2*i-1]="CI"
    prediction$type[2*i]="PI"
  }
  
  interval=data.frame(
    "Drug"=prediction$drug[(1:(length(prediction$drug)/2))*2],
    "CI"=prediction$`95% CI`[1:(length(prediction$`95% CI`)/2)*2])
  interval$PI=prediction$`95% PI`[1:(length(prediction$`95% CI`)/2)*2-1]
  interval<- interval[seq(dim(interval)[1],1),]
  
  #Turn your 'drug' column into a character vector
  prediction$drug <- as.character(prediction$drug)
  #Then turn it back into a factor with the levels in the correct order (i.e. the same order as the csv file)
  prediction$drug <- factor(prediction$drug, levels=unique(prediction$drug))
  
  
  prediction_pi = ggplot(data=prediction,
                         aes(x=drug,y=mean, ymin = LowerLimit, ymax = UpperLimit,color=type))+
    scale_color_manual(values=c('red','black'))+ 
    scale_y_continuous(trans = "log10", limits=c(xlow,xhigh))+ 
    geom_point(aes(size=type, color=type))+
    scale_size_manual(values=c(1,0))+
    geom_pointrange() +  ylab('Favors Drug                      Favors Placebo             ')+
    geom_hline(yintercept=1, lty=2) +xlab('')+
    coord_flip()+ggtitle(paste(outcome,' (Odds Ratios)  ',sep=""))+theme_classic()+theme(legend.position='none')
  
  
  
  names(interval) <- c("Drug", "95% CI", "95% PI")
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(interval, rows=NULL, theme=ttheme_default(base_size=8))
  # Plot chart and table into one object
  p2=grid.arrange(prediction_pi, tbl,
                  ncol=2,
                  as.table=TRUE,
                  heights=c(25,0))
  
  
  return(p2)
}


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

##### 1.1 set up the dataset
outcome="SUICIDAL BEHAVIOUR or SELF HARM"
data1 <- split_data(original,outcome ) 
data1=data1[data1$Dose_range=="Licensed",]
data2=remove.onearm(data1)

# remove studies with events > no_randomized
problem_events=which(data2$No_randomised<data2$martha_outcome)
print(paste("Total number of studies with number randomized being smaller than event rates: ",length(unique(data2$StudyID[problem_events]))))
print(paste("Removed the following studies:", unique(data2$StudyID[problem_events])))
data2=data2[data2$No_randomised>=data2$martha_outcome,];
data3 = data2

# harmonized baseline
data3= harmonize_baseline(data3)

# transform dose
data3=transform_dose(data3)

# summarize study-level characteristics
data3=make_study_char(data3)

# descriptives
print(paste("Total number of arms ",dim(data3)[1],". Total number of studies ", length(unique(data3$StudyID)), sep=""))

#print(paste("Total events in placebo ",round(sum(data2$martha_outcome[data2$Drug=="placebo"]), digits=0),", out of a total of ", round(sum(data2$No_randomised[data2$Drug=="placebo"]), digits=0)," patients. Event rate placebo ",round(sum(data2$martha_outcome[data2$Drug=="placebo"])/sum(data2$No_randomised[data2$Drug=="placebo"]), digits=3)  , sep=""))
#print(paste("Total events in drugs ",round(sum(data2$martha_outcome[data2$Drug!="placebo"]), digits=0),", out of a total of ", round(sum(data2$No_randomised[data2$Drug!="placebo"]), digits=0)," patients. Event rate drugs ",round(sum(data2$martha_outcome[data2$Drug!="placebo"])/sum(data2$No_randomised[data2$Drug!="placebo"]), digits=3) ,sep=""))
print(paste("Total events in placebo ",round(sum(data3$martha_outcome[data3$Drug=="placebo"]), digits=0),", out of a total of ", round(sum(data3$No_randomised[data3$Drug=="placebo"]), digits=0)," patients. Event rate placebo ",round(sum(data3$martha_outcome[data3$Drug=="placebo"])/sum(data3$No_randomised[data3$Drug=="placebo"]), digits=3)  , sep=""))
print(paste("Total events in drugs ",round(sum(data3$martha_outcome[data3$Drug!="placebo"]), digits=0),", out of a total of ", round(sum(data3$No_randomised[data3$Drug!="placebo"]), digits=0)," patients. Event rate drugs ",round(sum(data3$martha_outcome[data3$Drug!="placebo"])/sum(data3$No_randomised[data3$Drug!="placebo"]), digits=3) ,sep=""))


#summarize dose across drugs
data3.dose=data3[!is.na(data3$Dose_intended_min),]
dose_info=data.frame("Drug"=unique(data3.dose$Drug),"number arms"=NA, "percent low dose"=NA)
for(i in 1:length(dose_info$Drug)){
  dose_info$number.arms[i]= sum(data3.dose$Drug==unique(dose_info$Drug)[i])
  dose_info$percent.low.dose[i]= sum(data3.dose$Dose_intended_min[data3.dose$Drug==unique(dose_info$Drug)[i]]<=1)/    sum(data3.dose$Drug==unique(dose_info$Drug)[i])
}
print(dose_info[dose_info$Drug!="placebo",],digits=2)

##### 1.2 perform a meta-analysis after combining all drugs
col3=which(colnames(data3) %in% c("StudyID", "Drug", "No_randomised", "martha_outcome"))
data3.1=data3[,col3]
pairwise1= grouped_meta_analysis(data3.1)

print(paste("Number of studies: ", pairwise1$model1$k, sep=""))
print( paste("Fixed effects (Mantel-Haenszel) meta-analysis: OR=",  round(exp(pairwise1$model1$TE.fixed),2), ". 95% CI " ,round(exp(pairwise1$model1$lower.fixed),2), " to " , round(exp(pairwise1$model1$upper.fixed),2), sep=""))

print( paste("Random effects meta-analysis: OR=",  round(exp(pairwise1$model1$TE.random),2), ". 95% CI " ,round(exp(pairwise1$model1$lower.random),2), " to " , round(exp(pairwise1$model1$upper.random),2), sep=""))
print( paste("Prediction interval",  round(exp(pairwise1$model1$lower.predict),2), "to" , round(exp(pairwise1$model1$upper.predict),2)))
print(paste("Heterogeneity (tau squared) was estimated to be", round(pairwise1$model1$tau^2,2) ))

funnel(pairwise1$model1, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c("green", "yellow", "pink"))
legend(1.5, 0, c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),   fill = c("green", "yellow", "pink"), bty = "n")

metabias(pairwise1$model1, method.bias = "linreg")

forest(pairwise1$model1, label.right = "Favours placebo", 
       label.left = "Favours drug", digits=2, xlim=c(0.1,15), leftcols = c("studlab"), 
       rightcols=c("effect", "ci", "w.fixed"), comb.random = F)



##### 1.3 perform meta-analyses in comparisons with more than 10 studies
data3.2= collapse_arms(data3)$data_collapsed
data4=data3.2[order(data3.2$StudyID,data3.2$Drug),]

###
metan_manystudies(data4)

#### data3 includes arms in different dosages in the same study
#### data4 includes has collapsed arms in different dosages in the same study


##### 1.4 assess transitivity

### after collapsing same drugs with different dosages in the same study
p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data4, sm = "OR")
p1$dose.fixed.all=as.numeric(p1$dose.fixed.all)
p1$comparison=paste(p1$Drug1, "-", p1$Drug2)
comparison.list=unique(p1$comparison)
transitivity=data.frame("comparison"=comparison.list,"studies"=NA,"baseline_SD"=NA, "age_SD"=NA, "female"=NA, "dose.fixed.all"=NA)

for(i in 1:length(transitivity$comparison)){
  transitivity$studies[i]=sum(p1$comparison==transitivity$comparison[i])
  transitivity$baseline_SD[i]=paste(round(mean(p1$baseline[p1$comparison==transitivity$comparison[i]], na.rm = T),0)," (",round(sd(p1$baseline[p1$comparison==transitivity$comparison[i]], na.rm = T),1), ")", sep="")
  transitivity$age_SD[i]=paste(round(mean(p1$age[p1$comparison==transitivity$comparison[i]], na.rm = T),0)," (",round(sd(p1$age[p1$comparison==transitivity$comparison[i]], na.rm = T),1), ")", sep="")
  transitivity$female[i]=paste(round(mean(p1$female[p1$comparison==transitivity$comparison[i]], na.rm = T),2)," (",round(sd(p1$female[p1$comparison==transitivity$comparison[i]], na.rm = T),2), ")", sep="")
  transitivity$dose.fixed.all[i]=(round(mean(p1$dose.fixed.all[p1$comparison==transitivity$comparison[i]], na.rm = T),2))
}

transitivity[,1:5]

##### 1.4 perform NMA

# if there are multi-arm studies with 0 events in multiple arms run this instead
#p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data4, sm = "OR", allstudies=T)

netMH=netmetabin(p1)
netNCH=netmetabin(p1, method = "NCH")
#p1 = pairwise(treat = Drug, event = martha_outcome,n = No_randomised, studlab = StudyID, data = data4, sm = "OR", allstudies=T)
net1_FE=netmeta(p1, prediction = T, comb.random = F, comb.fixed = T)
net1_RE=netmeta(p1, prediction = T, comb.random = T, comb.fixed = F)

### netgraph
netgraph(net1_RE, seq = "optimal", col = "black", plastic = FALSE,
         points = TRUE, pch = 21, cex.points = 3, col.points = "black",
         bg.points = "gray", thickness = "equal",lwd=0.1,
         multiarm = FALSE, number.of.studies = F)

#######
print(paste("A total of ",netMH$n," treatments are included in the network.", sep=""))
print(paste("A total of ",netMH$k," studies are included in this analysis.", sep=""))
print(paste("Estimated heterogeneity tau-squared",round(net1_RE$tau^2,2)))
print(paste("Global test for inconsistency, p-value ",round(net1_RE$pval.Q.inconsistency,5)," (Q=",round(net1_RE$Q.inconsistency),",d.o.f.",net1_RE$df.Q.inconsistency, ")",sep=""))

options(digits=3)

### forestplots
forest(netMH, sortvar = -pscore, reference.group = "placebo",
       label.right = "Favours placebo", label.left = "Favours drug", digits=2, xlim=c(0.01,10))

forest(netNCH, sortvar = -pscore, reference.group = "placebo",
       label.right = "Favours placebo", label.left = "Favours drug", digits=2, xlim=c(0.01,10))

forest(net1_FE, sortvar = -pscore, reference.group = "placebo",
       label.right = "Favours placebo", label.left = "Favours drug", digits=2, xlim=c(0.01,10))

forest(net1_RE, sortvar = -pscore, reference.group = "placebo",
       label.right = "Favours placebo", label.left = "Favours drug", digits=2, xlim=c(0.01,10))



### draw prediction interval
#plot.pred(net1, outcome = outcome, xlow=0.1,xhigh=12)


###create league table
league.table=netleague(netMH, digits=2)
l1=as.matrix(league.table$fixed)
diag(l1)=toupper(substr(diag(l1),1,4))
write.xlsx(l1,file=paste(outcome,".xlsx"), sheetName = "league")  
netrank=netrank(netMH)

write.xlsx(netrank$Pscore.fixed,file=paste(outcome,".xlsx"), sheetName = "ranks", append = T)  
print(netrank, digits=2)

options(digits=2)
options(scipen=100)
options(max.print = 3000)


#### inconsistency
nsMH=netsplit(netMH)

split=netsplit(net1_RE)
split1=split$compare.random[!is.na(split$compare.random$p),]
print(paste("Percent of comparisons where there is some evidence (p-value<0.05) of disagreement between direct and indirect sources: ", 
            round(sum(split1$p<0.05)/length(split1$comparison),digits=2),". (",
            sum(split1$p<0.05)," out of ", length(split1$comparison)," comparisons)",sep=""))




# estimate probability of an event in each drug
#ind.pla=which(diag(league.table$random)=="placebo")

ind.pla=which(colnames(netMH$TE.fixed)=="placebo")
options(digits=3)
OR.pla=data.frame("drug"=colnames(netMH$TE.fixed), "logOR"=NA, "seTE"=NA)
OR.pla=OR.pla[OR.pla$drug!="placebo",]

for( i in 1:length(OR.pla$drug)){
  ind=which(colnames(netMH$TE.fixed)==OR.pla$drug[i])
  if(ind<ind.pla){
    OR.pla$logOR[i]=-netMH$TE.fixed[ind.pla,ind]
    OR.pla$seTE[i]=netMH$seTE.fixed[ind.pla,ind]
  }
    if(ind>ind.pla){
      OR.pla$logOR[i]=-netMH$TE.fixed[ind.pla,ind]
      OR.pla$seTE[i]=netMH$seTE.fixed[ind.pla,ind]
          }
}
OR.pla$OR=exp(OR.pla$logOR)
OR.pla$upperOR=exp(OR.pla$logOR+1.96*OR.pla$seTE)
OR.pla$lowerOR=exp(OR.pla$logOR-1.96*OR.pla$seTE)


rate.pla=sum(data3$martha_outcome[data3$Drug=="placebo"])/sum(data3$No_randomised[data3$Drug=="placebo"])
odds.pla=rate.pla/(1-rate.pla)

OR.pla$event.rate=round(OR.pla$OR*odds.pla/(1+OR.pla$OR*odds.pla),digits=2)
OR.pla$CI.lower=round(OR.pla$lowerOR*odds.pla/(1+OR.pla$lowerOR*odds.pla),digits=2)
OR.pla$CI.upper=round(OR.pla$upperOR*odds.pla/(1+OR.pla$upperOR*odds.pla),digits=2)
event.rates=OR.pla[,which(colnames(OR.pla) %in% c("drug","event.rate","CI.lower","CI.upper"))]
event.rates$rate=paste(event.rates$event.rate," [",event.rates$CI.lower,", ",event.rates$CI.upper,"]",sep = "")

print(paste("For an event rate in placebo equal to the average event rate (i.e. fixed to ", 
      round(rate.pla, digits = 2),") the estimated event rates for each drug are as follows", sep=""))
print(event.rates[,c(1,5)], digits = 2)

write.xlsx(event.rates,file=paste(outcome,".xlsx"), sheetName = "event.rates", append = T)  






##### 1.5 Sensitivity analysis, split drugs in high vs low dosage drugs
data3.5=data3
print("Studies with no information on dose are excluded")
data3.5=data3.5[!is.na(data3.5$Dose_intended_min),]
data3.5$Drug[data3.5$Dose_intended_min<=1]=paste(data3.5$Drug[data3.5$Dose_intended_min<=1], "_L",sep ="")
data3.5$Drug[data3.5$Dose_intended_min>1]=paste(data3.5$Drug[data3.5$Dose_intended_min>1], "_H",sep ="")
data3.5$Drug[data3.5$Drug=="placebo_L"]=" placebo"
data3.6=collapse_arms(data3.5)$data_collapsed

p3 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, 
              data = data3.6, sm = "OR", allstudies = F)

## if network disconnected
#netconnection(treat1, treat2, studlab, data=p3)
#p3=p3[p3$treat1!="citalopram_H"&p3$treat1!="escitalopram_H",]

net3=netmeta(p3, prediction = T)
#netgraph(net3, seq = "optimal", col = "black", plastic = FALSE,
#         points = TRUE, pch = 21, cex.points = 3, col.points = "black",
#         bg.points = "gray", thickness = "equal",lwd=0.1,
#         multiarm = FALSE, number.of.studies = F)

forest(net3, sortvar = colnames(net3$TE.random), reference.group = " placebo",
      label.right = "Favours placebo", label.left = "Favours drug", fontsize =12, xlim=c(0.2,8))

league.table3=netleague(net3, digits=2)
l3=as.matrix(league.table3$random)
drug_list1=diag(l3)[2:length(diag(l3))]
drug_list2=substr(drug_list1,1, nchar(drug_list1)-2)
remove=which(table(drug_list2)==1)
drug_list2.1=drug_list2[!(drug_list2 %in% names(remove))]
drug_list3=unique(drug_list2.1)

options(digits=3)
#### low vs. pla
lopla=data.frame("Drug"=unique(drug_list2), "logOR"=NA, "seTE"=NA)
for(i in 1: length(unique(lopla$Drug))){
  L=which(colnames(net3$TE.random)==paste(lopla$Drug[i],"_L",sep=""))
  if(length(L)!=0){
    lopla$logOR[i]=(net3$TE.random[L,1])
    lopla$seTE[i]=(net3$seTE.random[L,1])
  }}
lopla=lopla[!is.na(lopla$logOR),]
mL=metagen(logOR,seTE, studlab = Drug, data=lopla, backtransf=T, sm="OR", comb.random = F)
forest(mL, backtransf=T, overall=F, hetlab = "", print.I2 =F,print.tau2 = F , 
       leftlabs=c("Drug",NA,NA), print.pval.Q =F, leftcols=c("Drug"),
       label.right = "Favours placebo",label.left = "Favours low dose", xlim=c(0.1,20))



#### high vs. pla
hipla=data.frame("Drug"=unique(drug_list2), "logOR"=NA, "seTE"=NA)
for(i in 1: length(unique(hipla$Drug))){
  H=which(colnames(net3$TE.random)==paste(hipla$Drug[i],"_H",sep=""))
  if(length(H)!=0){
    hipla$logOR[i]=(net3$TE.random[H,1])
    hipla$seTE[i]=(net3$seTE.random[H,1])
  }}
hipla=hipla[!is.na(hipla$logOR),]
mH=metagen(logOR,seTE, studlab = Drug, data=hipla, backtransf=T, sm="OR", comb.random = F)
forest(mH, backtransf=T, overall=F, hetlab = "", print.I2 =F,print.tau2 = F , 
       leftlabs=c("Drug",NA,NA), print.pval.Q =F, leftcols=c("Drug"),
       label.right = "Favours placebo",label.left = "Favours high dose", xlim=c(0.1,20))


#### high vs. low
hilo=data.frame("Drug"=drug_list3, "logOR"=NA, "seTE"=NA)
for(i in 1: length(unique(hilo$Drug))){
  H=which((colnames(net3$TE.random)==paste(hilo$Drug[i],"_H",sep="")))
  L=which((colnames(net3$TE.random)==paste(hilo$Drug[i],"_L",sep="")))
    if(length(L)!=0&length(H)!=0){
    hilo$logOR[i]=-(net3$TE.random[L,H])
    hilo$seTE[i]=(net3$seTE.random[L,H])
  }}
mHL=metagen(logOR,seTE, studlab = Drug, data=hilo, backtransf=T, sm="OR", comb.random = F)
forest(mHL, backtransf=T, overall=F, hetlab = "", print.I2 =F,print.tau2 = F , 
       leftlabs=c("Drug",NA,NA), print.pval.Q =F, leftcols=c("Drug"),
       label.right = "Favours low dose",label.left = "Favours high dose", xlim=c(0.2,10))



