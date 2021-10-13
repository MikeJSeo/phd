library(rjags)
library(MCMCvis)

colnames(samps.all)
model.NMA <-  "
model {

##########   2 arm studies #######

for(i in 1:N.2arm)
{
y.2arm[i,]~ dmnorm(Mu[i,1:11], inv.var.cov.2arm[i,,])

Mu[i,1]<-baseline[treat[i,2]]-baseline[treat[i,1]]
Mu[i,2]<-age[treat[i,2]]-age[treat[i,1]]
Mu[i,3]<-sex[treat[i,2]]-sex[treat[i,1]]
Mu[i,4]<-th3[treat[i,2]]-th3[treat[i,1]]
Mu[i,5]<-th4[treat[i,2]]-th4[treat[i,1]]
Mu[i,6]<-th6[treat[i,2]]-th6[treat[i,1]]
Mu[i,7]<-th10[treat[i,2]]-th10[treat[i,1]]
Mu[i,8]<-th11[treat[i,2]]-th11[treat[i,1]]
Mu[i,9]<-th13[treat[i,2]]-th13[treat[i,1]]
Mu[i,10]<-th17[treat[i,2]]-th17[treat[i,1]]

Mu[i,11]~dnorm(delta[i], prec)
delta[i]<-d[treat[i,2]]-d[treat[i,1]]
}

d[1]<-0
age[1]<-0
baseline[1]<-0
sex[1]<-0
th10[1]<-0
th11[1]<-0
th13[1]<-0
th17[1]<-0
th3[1]<-0
th4[1]<-0
th6[1]<-0

for(i in 2: Nt){
d[i]~dnorm(0,1)
age[i]~dnorm(0,0.1)
baseline[i]~dnorm(0,0.1)
sex[i]~dnorm(0,0.1)
th10[i]~dnorm(0,0.1)
th11[i]~dnorm(0,0.1)
th13[i]~dnorm(0,0.1)
th17[i]~dnorm(0,0.1)
th3[i]~dnorm(0,0.1)
th4[i]~dnorm(0,0.1)
th6[i]~dnorm(0,0.1)

}

prec<-1/(tau*tau)
tau~dunif(0,2)


##########   3 arm studies #######

for(i in 1:N.3arm)
{
y.3arm[i,1:22]~ dmnorm(Mu3[i,1:22], inv.var.cov.3a[i,1:22,1:22])

Mu3[i,1]<-baseline[treat.3a[i,2]]-baseline[treat.3a[i,1]]
Mu3[i,2]<-age[treat.3a[i,2]]-age[treat.3a[i,1]]
Mu3[i,3]<-sex[treat.3a[i,2]]-sex[treat.3a[i,1]]
Mu3[i,4]<-th3[treat.3a[i,2]]-th3[treat.3a[i,1]]
Mu3[i,5]<-th4[treat.3a[i,2]]-th4[treat.3a[i,1]]
Mu3[i,6]<-th6[treat.3a[i,2]]-th6[treat.3a[i,1]]
Mu3[i,7]<-th10[treat.3a[i,2]]-th10[treat.3a[i,1]]
Mu3[i,8]<-th11[treat.3a[i,2]]-th11[treat.3a[i,1]]
Mu3[i,9]<-th13[treat.3a[i,2]]-th13[treat.3a[i,1]]
Mu3[i,10]<-th17[treat.3a[i,2]]-th17[treat.3a[i,1]]

Mu3[i,11]<-baseline[treat.3a[i,3]]-baseline[treat.3a[i,1]]
Mu3[i,12]<-age[treat.3a[i,3]]-age[treat.3a[i,1]]
Mu3[i,13]<-sex[treat.3a[i,3]]-sex[treat.3a[i,1]]
Mu3[i,14]<-th3[treat.3a[i,3]]-th3[treat.3a[i,1]]
Mu3[i,15]<-th4[treat.3a[i,3]]-th4[treat.3a[i,1]]
Mu3[i,16]<-th6[treat.3a[i,3]]-th6[treat.3a[i,1]]
Mu3[i,17]<-th10[treat.3a[i,3]]-th10[treat.3a[i,1]]
Mu3[i,18]<-th11[treat.3a[i,3]]-th11[treat.3a[i,1]]
Mu3[i,19]<-th13[treat.3a[i,3]]-th13[treat.3a[i,1]]
Mu3[i,20]<-th17[treat.3a[i,3]]-th17[treat.3a[i,1]]

Mu3[i,21:22]~dmnorm(theta[i,1:2], prec.3a[1:2,1:2])
theta[i,1]<- d[treat.3a[i,2]]-d[treat.3a[i,1]]
theta[i,2]<- d[treat.3a[i,3]]-d[treat.3a[i,1]]
}

prec.3a[1,1]<-1.33*prec
prec.3a[2,2]<-prec.3a[1,1]
prec.3a[1,2]<-(-0.67*prec)
prec.3a[2,1]<-prec.3a[1,2]

}
"


model2.spec<-textConnection(model.NMA) 
data <- list(N.2arm=N.2arm, y.2arm=mean.estimates.2arm, 
             inv.var.cov.2arm=inv.var.cov.2arm, treat=treats.2arm.coded, Nt=Nt, 
             treat.3a=treats.3arm.coded, y.3arm=mean.estimates.3arm, 
             inv.var.cov.3a=inv.var.cov.3a, N.3arm=N.3arm
             )
jags.m=0
jags.m <- jags.model(model2.spec, data = data, n.chains =4, n.adapt = 1000)



params <- c("d", "baseline", "sex", "age", "th3", "th4", "th6", "th10",  
            "th11", "th13", "th17", "tau") 

closeAllConnections()
samps<- coda.samples(jags.m, params, n.iter =1000)
#summary(samps)
NMAestimates=round(MCMCsummary(samps)[,1:2], digits=3)
MCMCtrace(samps,pdf = FALSE, params = "d") 




samps.NMA=rbind(samps[[1]], samps[[2]], samps[[3]], samps[[4]])
samps.NMA2=samps.NMA[, c("age[1]", "age[2]", "age[3]", "age[4]", "age[5]", "age[6]", 
                            "age[7]", "age[8]", "age[9]", "age[10]", "age[11]", "age[12]", 
                            "age[13]", 
                         "baseline[1]", "baseline[2]", "baseline[3]", "baseline[4]", 
                            "baseline[5]", "baseline[6]", "baseline[7]", "baseline[8]", "baseline[9]", 
                            "baseline[10]", "baseline[11]", "baseline[12]", "baseline[13]", 
                         "d[1]", "d[2]", "d[3]", "d[4]", "d[5]", "d[6]", "d[7]", "d[8]", 
                            "d[9]", "d[10]", "d[11]", "d[12]", "d[13]", 
                         "sex[1]", "sex[2]", "sex[3]", "sex[4]", "sex[5]", "sex[6]", "sex[7]", "sex[8]", "sex[9]", 
                            "sex[10]", "sex[11]", "sex[12]", "sex[13]",  "th10[1]", 
                            "th10[2]", "th10[3]", "th10[4]", "th10[5]", "th10[6]", "th10[7]", 
                            "th10[8]", "th10[9]", "th10[10]", "th10[11]", "th10[12]", "th10[13]", 
                            "th11[1]", "th11[2]", "th11[3]", "th11[4]", "th11[5]", "th11[6]", 
                            "th11[7]", "th11[8]", "th11[9]", "th11[10]", "th11[11]", "th11[12]", 
                            "th11[13]", "th13[1]", "th13[2]", "th13[3]", "th13[4]", "th13[5]", 
                            "th13[6]", "th13[7]", "th13[8]", "th13[9]", "th13[10]", "th13[11]", 
                            "th13[12]", "th13[13]", "th17[1]", "th17[2]", "th17[3]", "th17[4]", 
                            "th17[5]", "th17[6]", "th17[7]", "th17[8]", "th17[9]", "th17[10]", 
                            "th17[11]", "th17[12]", "th17[13]", "th3[1]", "th3[2]", "th3[3]", 
                            "th3[4]", "th3[5]", "th3[6]", "th3[7]", "th3[8]", "th3[9]", "th3[10]", 
                            "th3[11]", "th3[12]", "th3[13]", "th4[1]", "th4[2]", "th4[3]", 
                            "th4[4]", "th4[5]", "th4[6]", "th4[7]", "th4[8]", "th4[9]", "th4[10]", 
                            "th4[11]", "th4[12]", "th4[13]", "th6[1]", "th6[2]", "th6[3]", 
                            "th6[4]", "th6[5]", "th6[6]", "th6[7]", "th6[8]", "th6[9]", "th6[10]", 
                            "th6[11]", "th6[12]", "th6[13]")]
mean.estimates.nma<-colMeans(samps.NMA2)
var.cov.nma<-cov(samps.NMA2)
mean.estimates.nma
