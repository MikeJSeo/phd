#data_for_new_imputation <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/data_for_new_imputation.csv")
#table(data_for_new_imputation$TREATMENT_GROUP)
####

rm(list=ls())
data_1 <- read.csv("C:/Users/Orestis/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_1.csv")
data_2 <- read.csv("C:/Users/Orestis/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_2.csv")
data_3 <- read.csv("C:/Users/Orestis/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_3.csv")
data_4 <- read.csv("C:/Users/Orestis/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_4.csv")
data_5 <- read.csv("C:/Users/Orestis/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_5.csv")

data_1 <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_1.csv")
data_2 <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_2.csv")
data_3 <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_3.csv")
data_4 <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_4.csv")
data_5 <- read.csv("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/Petrushka - St An Plan/data for mi/post_processing/data_5.csv")

Npatients=length(data_5$STUDYID)
dat1=rbind(data_1, data_2, data_3, data_4, data_5)
dat1=cbind(dat1, ".imp"=rep(c(1:5),each=Npatients))

Nimputation=5

treatments=as.character(unique(dat1$TREATMENT_GROUP))
Ns<-length(table(dat1$STUDYID))## 66 studies
studies<-unique((dat1$STUDYID)) 
Nt<-length(treatments) ## 13 treatments

treatments=c("placebo","trazodone", "sertraline", "paroxetine",  "imipramine",  ## put placebo first
             "amitriptyline", "fluoxetine", "clomipramine", "citalopram", 
             "bupropion", "venlafaxine", "duloxetine", "escitalopram")
dat1$treat=NA
for (i in 1:Nt){dat1$treat[dat1$TREATMENT_GROUP==treatments[i]]=i}
table(dat1$TREATMENT_GROUP)


dat1=dat1[!(dat1$STUDYID=="29060/061" & dat1$treat==1),]  # remove one patient from 29060/061
table(dat1$STUDYID, dat1$treat)/5

dat1$PREMATURE_TERMINATION=as.character(dat1$PREMATURE_TERMINATION)
dat1$PREMATURE_TERMINATION[dat1$PREMATURE_TERMINATION=="No"]=0
dat1$PREMATURE_TERMINATION[dat1$PREMATURE_TERMINATION=="Yes"]=1
table(dat1$PREMATURE_TERMINATION)
study.data<-list()
Nt.per.study<-c()
Npatients.per.study=c()
for (i in 1:Ns){
  study.data[[i]]<-dat1[dat1$STUDYID==studies[i],c("AGE", "SEX", "treat", 
                                                   "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10" ,"HAMD_11", "HAMD_13", "HAMD_17", 
                                                   "HAMD_WEEK_0","PREMATURE_TERMINATION", ".imp") ]
  colnames(study.data[[i]])<-c("age", "sex", "treat", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10" ,"HAMD_11", "HAMD_13", "HAMD_17", 
                               "baseline", "y", "imp")
  Nt.per.study[i]<-length(unique(study.data[[i]]$treat))
  Npatients.per.study[i]<-length((study.data[[i]]$treat[study.data[[i]]$imp==1]))
  study.data[[i]]= study.data[[i]][ study.data[[i]]$imp!=0,]
}

##
trials.2arm=which(Nt.per.study==2)
N.2arm=length(trials.2arm)

trials.3arm=which(Nt.per.study==3)
N.3arm=length(trials.3arm)


table(dat1$treat, dat1$PREMATURE_TERMINATION)/5
table(dat1$STUDYID, dat1$PREMATURE_TERMINATION)/5
