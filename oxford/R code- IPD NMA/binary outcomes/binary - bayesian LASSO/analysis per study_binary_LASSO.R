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
    treat1<-sort(as.character(unique(dat2$treat)))
    treats.2arm[ii,1]<-treat1[1]
    treats.2arm[ii,2]<-treat1[2]
    treatments.compared[[ii]]<-treat1
    dat2$treat=as.character(dat2$treat)
    
    for(j in 1:Nt.per.study[i]){dat2$treat.coded[dat2$treat==treat1[j]]=j-1}
    
    dat2$y=as.numeric(dat2$y)
    # glm1=glm(y~treat.coded*(baseline+age+sex+HAMD_3+HAMD_4+HAMD_6+HAMD_10+HAMD_11+HAMD_13+HAMD_17), data=dat2,family = "binomial")
   
    # standardize covariates
    mbaseline=mean(dat2$baseline); sbaseline=sd(dat2$baseline); dat2$baseline=(dat2$baseline-mbaseline)/sbaseline
    mage=mean(dat2$age); sage=sd(dat2$age); dat2$age=(dat2$age-mage)/sage
    msex=mean(dat2$sex); ssex=sd(dat2$sex); dat2$sex=(dat2$sex-msex)/ssex
    
    m3=mean(dat2$HAMD_3); s3=sd(dat2$HAMD_3); dat2$HAMD_3=(dat2$HAMD_3-m3)/s3
    m4=mean(dat2$HAMD_4); s4=sd(dat2$HAMD_4); dat2$HAMD_4=(dat2$HAMD_4-m4)/s4
    m6=mean(dat2$HAMD_6); s6=sd(dat2$HAMD_6); dat2$HAMD_6=(dat2$HAMD_6-m6)/s6
    m10=mean(dat2$HAMD_10); s10=sd(dat2$HAMD_10); dat2$HAMD_10=(dat2$HAMD_10-m10)/s10
    m11=mean(dat2$HAMD_11); s11=sd(dat2$HAMD_11); dat2$HAMD_11=(dat2$HAMD_11-m11)/s11
    m13=mean(dat2$HAMD_13); s13=sd(dat2$HAMD_13); dat2$HAMD_13=(dat2$HAMD_13-m13)/s13
    m17=mean(dat2$HAMD_17); s17=sd(dat2$HAMD_17);dat2$HAMD_17=(dat2$HAMD_17-m17)/s17
    
    model.study <-  "
    model {
    for(i in 1:Np)
    {
    yf[i]~dbern(p[i])
    
    logit(p[i])<-intercept+
    ctreat*treat[i]+
    cbaseline*baseline[i]+
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
    intercept~dnorm(0,0.01)
    cbaseline~dnorm(0,0.1)
    cage~dnorm(0,0.1)
    csex~dnorm(0,0.1)
    h3~dnorm(0,0.1)
    h4~dnorm(0,0.1)
    h6~dnorm(0,0.1)
    h10~dnorm(0,0.1)
    h11~dnorm(0,0.1)
    h13~dnorm(0,0.1)
    h17~dnorm(0,0.1)
    
    ctreat~ dnorm(0,0.1)
    ctreat.age~ ddexp(0, lambda)
    ctreat.sex~ ddexp(0, lambda)
    th3~ ddexp(0, lambda)
    th4~ ddexp(0, lambda)
    th6~ ddexp(0, lambda)
    th10~ ddexp(0, lambda)
    th11~ ddexp(0, lambda)
    th13~ ddexp(0, lambda)
    th17~ ddexp(0, lambda)
    ctreat.baseline~ ddexp(0, lambda)

    lambda ~ dgamma(2, 0.1) # this diffuse prior has mean 20, variance 200   
    }
    "
    model1.spec<-textConnection(model.study) 
    data <- list(yf=dat2$y,Np=length(dat2$age),baseline=dat2$baseline, 
                 treat=dat2$treat.coded, HAMD_3=dat2$HAMD_3, HAMD_4=dat2$HAMD_4, HAMD_6=dat2$HAMD_6
                 , HAMD_10=dat2$HAMD_10, HAMD_11=dat2$HAMD_11, HAMD_13=dat2$HAMD_13, 
                 HAMD_17=dat2$HAMD_17, age=dat2$age, sex=dat2$sex)
    jags.m=0
    jags.m <- jags.model(model1.spec, data = data, n.chains =4, n.adapt = 1000) ### increase!
    
    params <- c("intercept", "cbaseline","cage","csex","h3","h4","h6","h10","h11","h13","h17",
                "ctreat.baseline","ctreat.age", "ctreat.sex", "th3", 
                "th4", "th6", "th10", "th11", "th13", "th17","ctreat"   ) 
    
    closeAllConnections()
    samps<- coda.samples(jags.m, params, n.iter =2000) ### increase!
    #summary(samps)
    #round(MCMCsummary(samps)[,1:2], digits=4)
    #MCMCtrace(samps,pdf = FALSE, params = "ctreat.baseline") 
    MCMCtrace(samps,pdf = FALSE, params = "ctreat") 
    samps.all.imputed[[imputed]]=rbind(samps[[1]], samps[[2]], samps[[3]], samps[[4]])
  }
  samps.all=samps.all.imputed[[1]]
  for(kk in 2:Nimputation){samps.all=rbind(samps.all,samps.all.imputed[[kk]] )}
  
  samps.all=samps.all[, c("intercept", "cbaseline","cage","csex","h3","h4","h6","h10","h11","h13","h17",
                          "ctreat.baseline","ctreat.age", "ctreat.sex", "th3", 
                          "th4", "th6", "th10", "th11", "th13", "th17","ctreat"   ) ]
  
  m1<-colMeans(samps.all)
  v1<-cov(samps.all)
  
  mX=c(mbaseline, mage, msex, m3, m4, m6, m10, m11, m13, m17)
  sdX=c(sbaseline, sage, ssex, s3, s4, s6, s10, s11, s13, s17)
  N=matrix(0,nrow=22,ncol=22)
  N[1,1]=1
  for(col in 1:10){
    N[1, 1+col]=-mX[col]/sdX[col]
    N[1+col, 1+col]=1/sdX[col]
    N[11+col, 11+col]=1/sdX[col]
    N[22,11+col]=-mX[col]/sdX[col]
  }
  N[22,22]=1
  
  #summary(glm1)$coeff

  mean.estimates.2arm[ii,]<- (N %*% m1)[12:22]
  var.cov.2arm[ii, ,]<- (N %*% v1 %*% t(N))[12:22,12:22]
  inv.var.cov.2arm[ii,,]<-solve(var.cov.2arm[ii, ,])
  print(i)
  }

# recode treatments 2-armed studies
treats.2arm.coded<-treats.2arm
for(i in 1:Nt){treats.2arm.coded[treats.2arm.coded==treatments[i]]=i}





######################################################################################


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
    treat1<-sort(as.character(unique(dat2$treat)))
    treats.3arm[ii,1]<-treat1[1]
    treats.3arm[ii,2]<-treat1[2]
    treats.3arm[ii,3]<-treat1[3]
    treatments.compared[[i]]<-treat1
    dat2$treat=as.character(dat2$treat)
    for(j in 1:Nt.per.study[i]){dat2$treat.coded[dat2$treat==treat1[j]]=j     }
    dat2$treat.coded=as.factor(dat2$treat.coded)
    
    ###  standardize covariates
    dat2$y=as.numeric(dat2$y)
    #glm1=glm(y~treat.coded*(baseline+age+sex+HAMD_3+HAMD_4+HAMD_6+HAMD_10+HAMD_11+HAMD_13+HAMD_17), data=dat2,family = "binomial")
    
    # standardize covariates
    mbaseline=mean(dat2$baseline); sbaseline=sd(dat2$baseline); dat2$baseline=(dat2$baseline-mbaseline)/sbaseline
    mage=mean(dat2$age); sage=sd(dat2$age); dat2$age=(dat2$age-mage)/sage
    msex=mean(dat2$sex); ssex=sd(dat2$sex); dat2$sex=(dat2$sex-msex)/ssex
    m3=mean(dat2$HAMD_3); s3=sd(dat2$HAMD_3); dat2$HAMD_3=(dat2$HAMD_3-m3)/s3
    m4=mean(dat2$HAMD_4); s4=sd(dat2$HAMD_4); dat2$HAMD_4=(dat2$HAMD_4-m4)/s4
    m6=mean(dat2$HAMD_6); s6=sd(dat2$HAMD_6); dat2$HAMD_6=(dat2$HAMD_6-m6)/s6
    m10=mean(dat2$HAMD_10); s10=sd(dat2$HAMD_10); dat2$HAMD_10=(dat2$HAMD_10-m10)/s10
    m11=mean(dat2$HAMD_11); s11=sd(dat2$HAMD_11); dat2$HAMD_11=(dat2$HAMD_11-m11)/s11
    m13=mean(dat2$HAMD_13); s13=sd(dat2$HAMD_13); dat2$HAMD_13=(dat2$HAMD_13-m13)/s13
    m17=mean(dat2$HAMD_17); s17=sd(dat2$HAMD_17);dat2$HAMD_17=(dat2$HAMD_17-m17)/s17
    
    
    
    model.study <-  "
    model {
    for(i in 1:Np)
    {
    yf[i]~dbern(p[i])
    
    logit(p[i])<-intercept+
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
    
    ctreat[1]<-0
    ctreat[2]~dnorm(0,0.1)
    ctreat[3]~dnorm(0,0.1)
    
    cbaseline~dnorm(0,0.1)
    tbaseline[1]<-0
    cage~dnorm(0,0.1)
    tage[1]<-0
    csex~dnorm(0,0.1)
    tsex[1]<-0
    ch3~dnorm(0,0.1)
    th3[1]<-0
    ch4~dnorm(0,0.1)
    th4[1]<-0
    ch6~dnorm(0,0.1)
    th6[1]<-0
    ch10~dnorm(0,0.1)
    th10[1]<-0
    ch11~dnorm(0,0.1)
    th11[1]<-0
    ch13~dnorm(0,0.1)
    th13[1]<-0
    ch17~dnorm(0,0.1)
    th17[1]<-0
    
    intercept~dnorm(0,0.01)
    
    tage[2]~  ddexp(0, ttt[1])
    tage[3]~  ddexp(0, ttt[2])
    tsex[2]~  ddexp(0, ttt[1])
    tsex[3]~  ddexp(0, ttt[2])
    tbaseline[2]~  ddexp(0, ttt[1])
    tbaseline[3]~  ddexp(0, ttt[2])
    th3[2]~  ddexp(0, ttt[1])
    th3[3]~  ddexp(0, ttt[2])
    th4[2]~  ddexp(0, ttt[1])
    th4[3]~  ddexp(0, ttt[2])
    th6[2]~  ddexp(0, ttt[1])
    th6[3]~  ddexp(0, ttt[2])
    th10[2]~  ddexp(0, ttt[1])
    th10[3]~  ddexp(0, ttt[2])
    th11[2]~  ddexp(0, ttt[1])
    th11[3]~  ddexp(0, ttt[2])
    th13[2]~  ddexp(0, ttt[1])
    th13[3]~  ddexp(0, ttt[2])
    th17[2]~  ddexp(0, ttt[1])
    th17[3]~  ddexp(0, ttt[2])
    
    for(i in 1:2){
    ttt[i] <- lambda[i]  # uses conditional prior
    lambda[i] ~ dgamma(2, 0.1) # this diffuse prior has mean 20, variance 200 
    }
  }
    "
    
    
    model1.spec<-textConnection(model.study) 
    data <- list(yf=dat2$y,Np=length(dat2$age),baseline=dat2$baseline, 
                 treat=dat2$treat.coded, HAMD_3=dat2$HAMD_3, HAMD_4=dat2$HAMD_4, HAMD_6=dat2$HAMD_6
                 , HAMD_10=dat2$HAMD_10, HAMD_11=dat2$HAMD_11, HAMD_13=dat2$HAMD_13, 
                 HAMD_17=dat2$HAMD_17, age=dat2$age, sex=dat2$sex)
    jags.m=0
    jags.m <- jags.model(model1.spec, data = data, n.chains =4, n.adapt = 500) ### increase!
    
    params <- c("intercept", "cbaseline","cage","csex","ch3","ch4","ch6","ch10","ch11","ch13","ch17",
                "tbaseline","tage", "tsex", "th3", 
                "th4", "th6", "th10", "th11", "th13", "th17","ctreat"   ) 
    closeAllConnections()
    samps<- coda.samples(jags.m, params, n.iter =1000) ### increase!
    #summary(samps)
    #round(MCMCsummary(samps)[,1:2], digits=4)
    #MCMCtrace(samps,pdf = FALSE, params = "tbaseline") 
    samps.all.imputed[[imputed]]=rbind(samps[[1]], samps[[2]], samps[[3]], samps[[4]])
  }
  samps.all=samps.all.imputed[[1]]
  for(kk in 2:Nimputation){samps.all=rbind(samps.all,samps.all.imputed[[kk]] )}
  
  samps.all2=samps.all[, c("intercept", "cbaseline","cage","csex","ch3","ch4","ch6","ch10","ch11",
                           "ch13","ch17",
                           "tbaseline[2]","tage[2]", "tsex[2]", "th3[2]", 
                           "th4[2]", "th6[2]", "th10[2]", "th11[2]", "th13[2]", "th17[2]",
                           "tbaseline[3]","tage[3]", "tsex[3]", "th3[3]", 
                           "th4[3]", "th6[3]", "th10[3]", "th11[3]", "th13[3]", "th17[3]",
                           "ctreat[2]", "ctreat[3]"   )]
  
  
  m1<-colMeans(samps.all2)
  v1<-cov(samps.all2)
  
  mX=c(mbaseline, mage, msex, m3, m4, m6, m10, m11, m13, m17)
  sdX=c(sbaseline, sage, ssex, s3, s4, s6, s10, s11, s13, s17)
  N=matrix(0,nrow=33,ncol=33)
  N[1,1]=1
  for(col in 1:10){
    N[1, 1+col]=-mX[col]/sdX[col]
    N[1+col, 1+col]=1/sdX[col]
    N[11+col, 11+col]=1/sdX[col]
    N[21+col,21+col]=1/sdX[col]
    
    N[32,11+col]=-mX[col]/sdX[col]
    N[33,21+col]=-mX[col]/sdX[col]
  }
  N[32,32]=1
  N[33,33]=1
  
  mean.estimates.3arm[ii,]<- (N %*% m1)[12:33]
  #summary(glm1)$coeff
  var.cov.3a[ii, ,]<- (N %*% v1 %*% t(N))[12:33,12:33]
  inv.var.cov.3a[ii,,]<-solve(var.cov.3a[ii, ,])
  #sqrt(diag((N %*% v1 %*% t(N))[12:33,12:33]))
  print(ii)
  }



# recode treatments 2-armed studies
treats.2arm.coded<-treats.2arm
for(i in 1:Nt){treats.2arm.coded[treats.2arm.coded==treatments[i]]=i}


# recode treatments 3-armed studies
treats.3arm.coded<-treats.3arm
for(i in 1:Nt){treats.3arm.coded[treats.3arm.coded==treatments[i]]=i}
