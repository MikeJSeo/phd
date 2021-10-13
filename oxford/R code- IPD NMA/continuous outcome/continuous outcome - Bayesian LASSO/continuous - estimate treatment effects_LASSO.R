### estimating effects for a new patient
rows=names(mean.estimates.nma)
endrows=c()
for(i in 1:length(rows)){endrows[i]=substring(rows[i], nchar(rows[i])-3,nchar(rows[i]))}
# input patient characteristics
age=45
baseline=5
sex=1  ### 0:male, 1:female  
HAMD_10=0
HAMD_11=0
HAMD_13=1
HAMD_17=1
HAMD_3=0
HAMD_4=0
HAMD_6=1

#age.transf= (age-45)/15
#baseline.transf=(baseline-20)/4

new.patient=c(age, baseline, 
  1, sex, HAMD_10, HAMD_11, HAMD_13, HAMD_17, HAMD_3, HAMD_4, HAMD_6)
# transform patient characteristics


results=matrix(0, nrow = Nt, ncol = Nt)
SE=matrix(0, nrow = Nt, ncol = Nt)
for(i in 2:Nt){
  for(k in 1:(i-1)){
    which.i=c()
    for(j in 1:length(rows)){
       which.i=c(which.i,grepl(paste("[", i, "]", sep=""),rows[j],fixed = T))
    }
    which.k=c()
    for(j in 1:length(rows)){
      which.k=c(which.k,grepl(paste("[", k, "]", sep=""),rows[j],fixed = T))
    }
    
  var1=var.cov.nma[c(which.i| which.k),c(which.i| which.k) ]    
 results[i,k]= round(5*(mean.estimates.nma[which.i]-mean.estimates.nma[which.k]) %*% new.patient,2)
 
 new.patient.var=c(-age, age, -baseline, baseline,-1, +1, -sex, sex, -HAMD_10, HAMD_10, 
                   -HAMD_11, HAMD_11, -HAMD_13, HAMD_13, -HAMD_17, HAMD_17, -HAMD_3, HAMD_3, 
                   -HAMD_4, HAMD_4, -HAMD_6, HAMD_6)   
 SE[i,k]=round(sqrt(new.patient.var %*% var1 %*% new.patient.var)*5, digits=2)
    
  }
}
diag(results)<-treatments
diag(SE)<-treatments
round(as.numeric(results[2:Nt,1]), digits=1)
round(as.numeric(results[2:Nt,1]) -1.96* as.numeric(SE[2:Nt,1]), digits=1)
round(as.numeric(results[2:Nt,1]) +1.96* as.numeric(SE[2:Nt,1]), digits=1)

cbind(results[,1],diag(results))
## all treattments vs. placebo
paste(round(as.numeric(results[2:Nt,1]), digits=1),"[",
round(as.numeric(results[2:Nt,1]) -1.96* as.numeric(SE[2:Nt,1]), digits=1),"; ",
round(as.numeric(results[2:Nt,1]) +1.96* as.numeric(SE[2:Nt,1]), digits=1),"]", sep="")
