library(rjags)
library(MCMCvis)
##### only 2 arm studies ----------------------------------------
treatments.compared=list()
mean.estimates.2arm=array(0,dim=c(N.2arm, 11) )
var.cov.2arm<-array(0, dim=c(N.2arm, 11, 11))
inv.var.cov.2arm<-array(0, dim=c(N.2arm, 11, 11))
treats.2arm<-array(0, dim=c(N.2arm,2))


for (ii in 1: N.2arm){  
  
samps.all.imputed=list()
for (imputed in 1:Nimputation)
  {
    
    
i<-trials.2arm[ii]
study.data[[i]]$treat<-as.factor(study.data[[i]]$treat)
dat2<-study.data[[i]][study.data[[i]]$imp==imputed ,]
###  standardize covariates
dat2$baseline=(dat2$baseline-20)/4
dat2$age=(dat2$age-45)/15
dat2$y=(dat2$y-6)/5

treat1<-sort(as.character(unique(dat2$treat)))
treats.2arm[ii,1]<-treat1[1]
treats.2arm[ii,2]<-treat1[2]
treatments.compared[[ii]]<-treat1
dat2$treat=as.character(dat2$treat)

for(j in 1:Nt.per.study[i]){
  dat2$treat.coded[dat2$treat==treat1[j]]=j-1 
}


model.study <-  "
model {
for(i in 1:Np)
{
yf[i]~dnorm(mu[i], prec)

mu[i]<-intercept+
ctreat*treat[i]+
cbaseline*baseline[i] +
cage*age[i]+
csex*sex[i]+
h3*HAMD_3[i] +h4*HAMD_4[i] +h6*HAMD_6[i] +h10*HAMD_10[i] +h11*HAMD_11[i] +
h13*HAMD_13[i] +h17*HAMD_17[i] +
ctreat.baseline*baseline[i]*treat[i]+
ctreat.sex*sex[i]*treat[i]+
ctreat.age*age[i]*treat[i]+
th3*HAMD_3[i]*treat[i]+
th4*HAMD_4[i]*treat[i]+
th6*HAMD_6[i]*treat[i]+
th10*HAMD_10[i]*treat[i]+
th11*HAMD_11[i]*treat[i]+
th13*HAMD_13[i]*treat[i]+
th17*HAMD_17[i]*treat[i]

}
prec<-1/pow(sigma,2)
sigma~dunif(0,5)

intercept~dnorm(0,0.01)
cbaseline~dnorm(0,0.01)
cage~dnorm(0,0.01)
csex~dnorm(0,0.01)
h3~dnorm(0,0.01)
h4~dnorm(0,0.01)
h6~dnorm(0,0.01)
h10~dnorm(0,0.01)
h11~dnorm(0,0.01)
h13~dnorm(0,0.01)
h17~dnorm(0,0.01)

ctreat~ dnorm(0,0.01)
ctreat.age~dnorm(0,0.01)
ctreat.sex~ dnorm(0,0.01)
th3~ dnorm(0,0.01)
th4~ dnorm(0,0.01)
th6~dnorm(0,0.01)
th10~ dnorm(0,0.01)
th11~ dnorm(0,0.01)
th13~ dnorm(0,0.01)
th17~ dnorm(0,0.01)
ctreat.baseline~ dnorm(0,0.01)

}
"


model1.spec<-textConnection(model.study) 
data <- list(yf=dat2$y,Np=length(dat2$age),baseline=dat2$baseline, 
             treat=dat2$treat.coded, HAMD_3=dat2$HAMD_3, HAMD_4=dat2$HAMD_4, HAMD_6=dat2$HAMD_6
             , HAMD_10=dat2$HAMD_10, HAMD_11=dat2$HAMD_11, HAMD_13=dat2$HAMD_13, 
             HAMD_17=dat2$HAMD_17, age=dat2$age, sex=dat2$sex)
jags.m=0
jags.m <- jags.model(model1.spec, data = data, n.chains =4, n.adapt = 5000) ### increase!

params <- c("ctreat", "ctreat.baseline", "th3", 
            "th4", "th6", "th10", "th11", "th13", "th17",
            "ctreat.age", "ctreat.sex") 

closeAllConnections()
samps<- coda.samples(jags.m, params, n.iter =5000) ### increase!
#summary(samps)
#round(MCMCsummary(samps)[,1:2], digits=4)
#MCMCtrace(samps,pdf = FALSE, params = "ctreat.baseline") 

samps.all.imputed[[imputed]]=rbind(samps[[1]], samps[[2]], samps[[3]], samps[[4]])
}
samps.all=samps.all.imputed[[1]]
for(kk in 2:Nimputation){samps.all=rbind(samps.all,samps.all.imputed[[kk]] )}

mean.estimates.2arm[ii,]<-colMeans(samps.all)
#colnames(samps.all)
#lm(y~treat.coded*(baseline+age+sex+HAMD_3+HAMD_4+HAMD_6+HAMD_10+HAMD_11+HAMD_13+HAMD_17), data=dat2)

var.cov.2arm[ii, ,]<-cov(samps.all)
inv.var.cov.2arm[ii,,]<-solve(var.cov.2arm[ii, ,])

print(i)
}

# recode treatments 2-armed studies
treats.2arm.coded<-treats.2arm
for(i in 1:Nt){treats.2arm.coded[treats.2arm.coded==treatments[i]]=i}




#  checking whether results make sense
# aggregate(dat2$y, list(dat2$treat), mean)
# a1=mean.estimates.2arm[ii,]
# a1 %*% c(1, mean(dat2$age),mean(dat2$baseline),mean(dat2$sex),
#                                mean(dat2$HAMD_10),mean(dat2$HAMD_11), mean(dat2$HAMD_13),
#                                mean(dat2$HAMD_17), 
#                                mean(dat2$HAMD_3),
#                                mean(dat2$HAMD_4), mean(dat2$HAMD_6))
                               

##### only 3 arm studies --------------------------------------------------
treatments.compared.3a=list()
mean.estimates.3arm=array(0,dim=c(N.3arm, 22) )
var.cov.3a<-array(0, dim=c(N.3arm, 22, 22))
inv.var.cov.3a<-array(0, dim=c(N.3arm, 22, 22))
treats.3arm<-array(0, dim=c(N.3arm,3))


for (ii in 1: N.3arm){ 
  samps.all.imputed=list()
  for (imputed in 1:Nimputation){
    
  i<-trials.3arm[ii]
  
  study.data[[i]]$treat<-as.factor(study.data[[i]]$treat)
  dat2<-study.data[[i]][study.data[[i]]$imp==imputed ,]
  #dim(dat2)
  
  ###  standardize covariates
  dat2$baseline=(dat2$baseline-20)/4
  dat2$age=(dat2$age-45)/15
  dat2$y=(dat2$y-6)/5
  
  treat1<-sort(as.character(unique(dat2$treat)))
  treats.3arm[ii,1]<-treat1[1]
  treats.3arm[ii,2]<-treat1[2]
  treats.3arm[ii,3]<-treat1[3]
  treatments.compared[[i]]<-treat1
  dat2$treat=as.character(dat2$treat)
  
  for(j in 1:Nt.per.study[i]){dat2$treat.coded[dat2$treat==treat1[j]]=j   }
  dat2$treat.coded=as.factor(dat2$treat.coded)

model.study <-  "
model {
for(i in 1:Np)
{
yf[i]~dnorm(mu[i], prec)

mu[i]<-intercept+
ctreat[treat[i]]+
cbaseline*baseline[i] + tbaseline[treat[i]]*baseline[i]+
cage*age[i] + tage[treat[i]]*age[i]+
csex*sex[i] + tsex[treat[i]]*sex[i]+
ch3*HAMD_3[i] + th3[treat[i]]*HAMD_3[i]+
ch4*HAMD_4[i] + th4[treat[i]]*HAMD_4[i]+
ch6*HAMD_6[i] + th6[treat[i]]*HAMD_6[i]+
ch10*HAMD_10[i] + th10[treat[i]]*HAMD_10[i]+
ch11*HAMD_11[i] + th11[treat[i]]*HAMD_11[i]+
ch13*HAMD_13[i] + th13[treat[i]]*HAMD_13[i]+
ch17*HAMD_17[i] + th17[treat[i]]*HAMD_17[i]
}

prec<-1/pow(sigma,2)
sigma~dunif(0,5)
ctreat[1]<-0
ctreat[2]~dnorm(0,0.01)
ctreat[3]~dnorm(0,0.01)

cbaseline~dnorm(0,0.01)
tbaseline[1]<-0
cage~dnorm(0,0.01)
tage[1]<-0
csex~dnorm(0,0.01)
tsex[1]<-0
ch3~dnorm(0,0.01)
th3[1]<-0
ch4~dnorm(0,0.01)
th4[1]<-0
ch6~dnorm(0,0.01)
th6[1]<-0
ch10~dnorm(0,0.01)
th10[1]<-0
ch11~dnorm(0,0.01)
th11[1]<-0
ch13~dnorm(0,0.01)
th13[1]<-0
ch17~dnorm(0,0.01)
th17[1]<-0

intercept~dnorm(0,0.01)

tage[2]~ dnorm(0,0.01)
tage[3]~ dnorm(0,0.01)
tsex[2]~ dnorm(0,0.01)
tsex[3]~ dnorm(0,0.01)
tbaseline[2]~ dnorm(0,0.01)
tbaseline[3]~ dnorm(0,0.01)
th3[2]~ dnorm(0,0.01)
th3[3]~ dnorm(0,0.01)
th4[2]~ dnorm(0,0.01)
th4[3]~ dnorm(0,0.01)
th6[2]~ dnorm(0,0.01)
th6[3]~ dnorm(0,0.01)
th10[2]~ dnorm(0,0.01)
th10[3]~ dnorm(0,0.01)
th11[2]~ dnorm(0,0.01)
th11[3]~ dnorm(0,0.01)
th13[2]~ dnorm(0,0.01)
th13[3]~ dnorm(0,0.01)
th17[2]~ dnorm(0,0.01)
th17[3]~ dnorm(0,0.01)

}
"


model1.spec<-textConnection(model.study) 
data <- list(yf=dat2$y,Np=length(dat2$age),baseline=dat2$baseline, 
             treat=dat2$treat.coded, HAMD_3=dat2$HAMD_3, HAMD_4=dat2$HAMD_4, HAMD_6=dat2$HAMD_6
             , HAMD_10=dat2$HAMD_10, HAMD_11=dat2$HAMD_11, HAMD_13=dat2$HAMD_13, 
             HAMD_17=dat2$HAMD_17, age=dat2$age, sex=dat2$sex)
jags.m=0
jags.m <- jags.model(model1.spec, data = data, n.chains =4, n.adapt = 5000) ### increase!

params <- c("ctreat",   "tbaseline", "tsex", "tage", "th3", "th4", "th6",  "th10",  
            "th11", "th13", "th17"            ) 

closeAllConnections()
samps<- coda.samples(jags.m, params, n.iter =20000) ### increase!
#summary(samps)
#round(MCMCsummary(samps)[,1:2], digits=4)
#MCMCtrace(samps,pdf = FALSE, params = "tbaseline") 
#lm(y~treat.coded*(baseline+age+sex+HAMD_3+HAMD_4+HAMD_6+HAMD_10+HAMD_11+HAMD_13+HAMD_17), data=dat2)

samps.all.imputed[[imputed]]=rbind(samps[[1]], samps[[2]], samps[[3]], samps[[4]])
  }
samps.all=samps.all.imputed[[1]]
for(kk in 2:Nimputation){samps.all=rbind(samps.all,samps.all.imputed[[kk]] )}

samps.all2=samps.all[, c( "ctreat[2]", "ctreat[3]",  "tage[2]", 
                         "tage[3]",  "tbaseline[2]", "tbaseline[3]",  
                         "th10[2]", "th10[3]",  "th11[2]", "th11[3]",  
                         "th13[2]", "th13[3]", "th17[2]", "th17[3]", 
                         "th3[2]", "th3[3]", "th4[2]", "th4[3]",  "th6[2]", 
                         "th6[3]",  "tsex[2]", "tsex[3]")]
mean.estimates.3arm[ii,]<-colMeans(samps.all2)
var.cov.3a[ii, ,]<-cov(samps.all2)
inv.var.cov.3a[ii,,]<-solve(var.cov.3a[ii, ,])

print(ii)
}



#  checking whether results make sense
# aggregate(dat2$y, list(dat2$treat), mean)
# a1=mean.estimates.3arm[ii,]
# a1[c(1,3,5,7,9,11,13,15,17,19,21)] %*% c(1, mean(dat2$age),mean(dat2$baseline),mean(dat2$sex),
#                                mean(dat2$HAMD_10),mean(dat2$HAMD_11), mean(dat2$HAMD_13),
#                                mean(dat2$HAMD_17), 
#                                mean(dat2$HAMD_3),
#                                mean(dat2$HAMD_4), mean(dat2$HAMD_6))
# a1[c(2,4,6,8,10,12,14,16,18,20,22)] %*% c(1, mean(dat2$age),mean(dat2$baseline),mean(dat2$sex),
#                                          mean(dat2$HAMD_10),mean(dat2$HAMD_11), mean(dat2$HAMD_13),
#                                          mean(dat2$HAMD_17), 
#                                          mean(dat2$HAMD_3),
#                                          mean(dat2$HAMD_4), mean(dat2$HAMD_6))


# recode treatments 2-armed studies
treats.2arm.coded<-treats.2arm
for(i in 1:Nt){treats.2arm.coded[treats.2arm.coded==treatments[i]]=i}


# recode treatments 3-armed studies
treats.3arm.coded<-treats.3arm
for(i in 1:Nt){treats.3arm.coded[treats.3arm.coded==treatments[i]]=i}
