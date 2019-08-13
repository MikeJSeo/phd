#In the SchizoEFF data
#Run meta-regression of efficacy SMD to the year of randomisation using the model that is already there programmed.


#*********************************************************************************
#             Load the libraries needed             
#*********************************************************************************
library(meta)
library(metafor)
library(netmeta)
library(readxl)

library(devtools)
install_github("esm-ispm-unibe-ch/NMAJags")
library(NMAJags)
library(R2jags)
library(RCurl)


url <- "https://raw.githubusercontent.com/MikeJSeo/jags/master/data/SchizoEFF.csv"
data <- getURL(url)                
data <- read.csv(textConnection(data), sep = ";", dec = ".")
data[,c("OverallEfficSD", "OverallEfficM")] <- apply(apply(data[,c("OverallEfficSD", "OverallEfficM")], 2, gsub, patt=",", replace="."), 2, as.numeric)
SchizoEFF <- data

#transform the data into a list suitable for JAGS analysis
NMAdataContinuous=make.jagsNMA.data(studyid=Study_No,t=Drug,y=OverallEfficM,sd=OverallEfficSD,n=OverallEfficN,data=SchizoEFF,type="cont",reference = "Placebo")
#run Jags and create a jags object
NMAinJAGS<- jags(data = NMAdataContinuous, inits = NULL,
                 parameters.to.save = c("SMD","SMD.ref","tau", "SUCRA"), n.chains = 2, n.iter = 10000,
                 n.burnin = 1000,DIC=F,n.thin=10,
                 model.file = modelNMAContinuous)
print(NMAinJAGS)
SchizoEFF$Year = SchizoEFF$Year - mean(SchizoEFF$Year) # center the data


NMRdataContinuous= make.jagsNMA.data(studyid=Study_No,t=Drug,y=OverallEfficM,sd=OverallEfficSD,n=OverallEfficN,data=SchizoEFF,type="cont",reference = "Placebo", othervar = Year)
NMAinJAGS<- jags(data = NMRdataContinuous, inits = NULL,
                 parameters.to.save = c("SMD","SMD.ref","tau", "SUCRA"), n.chains = 2, n.iter = 10000,
                 n.burnin = 1000,DIC=F,n.thin=10,
                 model.file = modelNMRContinuous)
print(NMAinJAGS)
