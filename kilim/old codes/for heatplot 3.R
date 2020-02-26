rm(list=ls())
library(xlsx)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)
library(heatmaply)

setwd("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Martha/data/RESULTS/")
outcomes=list.files()
#outcomes[outcomes=="arrhythmia-heart .xlsx"]="ARRHYTMIA/HEART RATE DISORDER.xlsx"


N.outcomes=length(outcomes)
treatments=c("placebo", "agomelatine", "amitriptyline", "bupropion", 
             "citalopram", "clomipramine", "desvenlafaxine", "duloxetine", 
             "escitalopram", "fluoxetine", "fluvoxamine", "levomilnacipran", 
             "milnacipran", "mirtazapine", "nefazodone", "paroxetine", 
             "reboxetine", "sertraline", "trazodone", "venlafaxine", "vilazodone", 
             "vortioxetine")
N.treatments=length(treatments)

rankings=matrix(0,nrow=N.treatments,ncol=N.outcomes)
rownames(rankings)=treatments
colnames(rankings)=substr(outcomes,1,nchar(outcomes)-6)
colnames(rankings)[colnames(rankings)=="CARDIAC DISORDERS, SIGNS AND SYMTPOMS NEC"]="CARDIAC DISORDERS, SIGNS AND SYMPTOMS"

#### for RD=0
rankings2=-rankings
for (i in 1:N.outcomes){
  ranks=read.xlsx(outcomes[i],sheetName = "Z.scores")
  for (j in 1:N.treatments){
    ind=which( ranks$drug==treatments[j])
    if(length(ind)!=0){rankings2[j,i]=-ranks$Zscore.0[ind]}
  }
}
range(rankings2) ###check range
is.na(rankings2[rankings2==0])=T
rankings2[rankings2<(-4.25)]=-4.25##### MAKE SURE THAT ZSCORES ARE SYMMETRICAL AROUND ZERO!!!!! Important for colors!!!!
rankings2[rankings2>4.25]=4.25 ##### MAKE SURE THAT ZSCORES ARE SYMMETRICAL AROUND ZERO!!!!! Important for colors!!!!
d3heatmap(rankings2, scale="none", dendrogram = "none", colors = colorRamp(c("red", "white", "green"))( (0:4)/4 ))




#### for RD=1%
rankings3=-rankings
for (i in 1:N.outcomes){
  ranks=read.xlsx(outcomes[i],sheetName = "Z.scores")
  for (j in 1:N.treatments){
    ind=which( ranks$drug==treatments[j])
    if(length(ind)!=0& !(outcomes[i] %in% c("DEATH .xlsx", "SUICIDAL BEHAVIOUR-ATTEMPT .xlsx", 
                                          "COMPLETED SUICIDE .xlsx", "SUICIDAL IDEATION .xlsx" ))
                                          ){rankings3[j,i]=-ranks$Zscore.1[ind]}
 
  if(length(ind)!=0& (outcomes[i] %in% c("DEATH .xlsx", "SUICIDAL BEHAVIOUR-ATTEMPT .xlsx", 
                                        "COMPLETED SUICIDE .xlsx", "SUICIDAL IDEATION .xlsx" ))
  ){rankings3[j,i]=-ranks$Zscore.0[ind]}
} }


range(rankings3)
is.na(rankings3[rankings3==0])=T
rankings3[rankings3<(-5)]=-5
rankings3[rankings3>5]=5
d3heatmap(rankings3, scale="none", dendrogram = "none", colors = colorRamp(c("red", "white", "green"))( (0:4)/4 ))


#### for RD=5%
rankings3=-rankings
for (i in 1:N.outcomes){
  ranks=read.xlsx(outcomes[i],sheetName = "Z.scores")
  for (j in 1:N.treatments){
    ind=which( ranks$drug==treatments[j])
    if(length(ind)!=0& !(outcomes[i] %in% c("DEATH .xlsx", "SUICIDAL BEHAVIOUR-ATTEMPT .xlsx", 
                                            "COMPLETED SUICIDE .xlsx", "SUICIDAL IDEATION .xlsx" ))
    ){rankings3[j,i]=-ranks$Zscore.2[ind]}
    
    if(length(ind)!=0& (outcomes[i] %in% c("DEATH .xlsx", "SUICIDAL BEHAVIOUR-ATTEMPT .xlsx", 
                                           "COMPLETED SUICIDE .xlsx", "SUICIDAL IDEATION .xlsx" ))
    ){rankings3[j,i]=-ranks$Zscore.0[ind]}
  } }


range(rankings3)
is.na(rankings3[rankings3==0])=T
rankings3[rankings3<(-5)]=-5
rankings3[rankings3>5]=5
d3heatmap(rankings3, scale="none", dendrogram = "none", colors = colorRamp(c("red", "white", "green"))( (0:4)/4 ))




#### event rates in placebo
outcomes1=colnames(rankings)

events.pla=data.frame("outcomes"=colnames(rankings), "rate"=NA, "PIL"=NA, 
                      "PIU"=NA,  "CIL"=NA,  "CIU"=NA)
for (i in 1:N.outcomes){
  events=read.xlsx(outcomes[i],sheetName = "placebo.rates")
  events.pla$rate[i]=events$rate
  events.pla$PIL[i]=events$UpperPre
  events.pla$PIU[i]=events$LowerPre
  events.pla$CIL[i]=events$lowerCI
  events.pla$CIU[i]=events$upperCI
}

metafor::forest(events.pla$rate, ci.lb = events.pla$CIL,
                ci.ub = events.pla$CIU, slab = events.pla$outcomes,
                xlab = "Event rates",xlim=c(-0.18,0.25), psize=0.8, digits=2)

