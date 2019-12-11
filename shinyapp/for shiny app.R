rm(list=ls())
setwd("C:/Users/efthimiou/Google Drive/PROJECT/APPLIED PROJECTS/SUN_D/analysis 3/final models for Shiny")

### load the three prediction models - one model per treatment
load("treat1.rda")
load("treat2.rda")
load("treat3.rda")


### load the rescaling vectors
xmeans=readRDS("xmeans")
xsd=readRDS("xsd")


### input the patient-specific covariates via the shiny app
############################################################################################################################################################
age=45 # limits=25 to 75
sex=1 ### 1: male, 2: female
educatenumber=12 ### years of education. limits= 8 to 28
work_condition= 1  ### 1: full time employment	2: part-time employment	3: on sick leave	4: housewife	5: student	6: retired	7: not employed	
marriage_condition=2 ### 1: single (never married)	2: divorced or separated	3: widowed	4: married
depression_age= 23 ### age of onset of depression, range. limits 0 to 75
depression_episode_number=3 ### number of depression episodes, limits 1 to 50
episode_months= 2 ### length of current depressive episode in months. limits 0.5 to 276
physical_illness=0 ### physical illness, yes/no 

primemd_q1=2  ### we will ask toshi for a description. Range 0 to 3
primemd_q2=1  ### we will ask toshi for a description. Range 0 to 3
primemd_q3=2  ### we will ask toshi for a description. Range 0 to 3
primemd_q4=0  ### we will ask toshi for a description. Range 0 to 3
primemd_q5=2  ### we will ask toshi for a description. Range 0 to 3
primemd_q6=2  ### we will ask toshi for a description. Range 0 to 3
primemd_q7=3  ### we will ask toshi for a description. Range 0 to 3
primemd_q8=0  ### we will ask toshi for a description. Range 0 to 3
primemd_q9=2  ### we will ask toshi for a description. Range 0 to 3

phq9_q1_1=1   ### we will ask toshi for a description. Range 0 to 3
phq9_q2_1=1   ### we will ask toshi for a description. Range 0 to 3
phq9_q3_1=3   ### we will ask toshi for a description. Range 0 to 3
phq9_q4_1=2   ### we will ask toshi for a description. Range 0 to 3
phq9_q5_1=1   ### we will ask toshi for a description. Range 0 to 3
phq9_q6_1=1   ### we will ask toshi for a description. Range 0 to 3
phq9_q7_1=2   ### we will ask toshi for a description. Range 0 to 3
phq9_q8_1=1   ### we will ask toshi for a description. Range 0 to 3
phq9_q9_1=0   ### we will ask toshi for a description. Range 0 to 3

w1_fibser_q1=5  ### we will ask toshi for a description. Range 1 to 7
w1_fibser_q2=3  ### we will ask toshi for a description. Range 1 to 7
w1_fibser_q3=7  ### we will ask toshi for a description. Range 1 to 7
w1_fibser_q4=2  ### we will ask toshi for a description. Range 1 to 7

bdi_q1_1=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q2_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q3_1=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q4_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q5_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q6_1=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q7_1=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q8_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q9_1=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q10_1=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q11_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q12_1=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q13_1=3 ### we will ask toshi for a description. Range 0 to 3
bdi_q14_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q15_1=1 ### we will ask toshi for a description. Range 0 to 3

bdi_q16_1longer=3 ### name: hypersomnia, range: 0-3
bdi_q16_1shorter=0 ### name: insomnia, range: 0-3
bdi_q17_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q18_1longer=3  ### name: decreased appetite, range:0-3
bdi_q18_1shorter=0 ### name: increased appetite, range:0-3

bdi_q19_1=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q20_1=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q21_1=2 ### we will ask toshi for a description. Range 0 to 3

phq9_q1_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q2_3=2 ### we will ask toshi for a description. Range 0 to 3
phq9_q3_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q4_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q5_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q6_3=2 ### we will ask toshi for a description. Range 0 to 3
phq9_q7_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q8_3=1 ### we will ask toshi for a description. Range 0 to 3
phq9_q9_3=3 ### we will ask toshi for a description. Range 0 to 3

w3_fibser_q1=2  ### we will ask toshi for a description. Range 1 to 7
w3_fibser_q2=6  ### we will ask toshi for a description. Range 1 to 7
w3_fibser_q3=4  ### we will ask toshi for a description. Range 1 to 7
w3_fibser_q4=5  ### we will ask toshi for a description. Range 1 to 7

bdi_q1_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q2_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q3_3=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q4_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q5_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q6_3=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q7_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q8_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q9_3=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q10_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q11_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q12_3=1 ### we will ask toshi for a description. Range 0 to 3
bdi_q13_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q14_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q15_3=1 ### we will ask toshi for a description. Range 0 to 3


bdi_q16_3longer=3 ### name: hypersomnia, range: 0-3
bdi_q16_3shorter=0 ### name: insomnia, range: 0-3

bdi_q17_3=2 ### we will ask toshi for a description. Range 0 to 3

bdi_q18_3longer=3 ### name: decreased appetite, range:0-3
bdi_q18_3shorter=0 ### name: increased appetite, range:0-3

bdi_q19_3=0 ### we will ask toshi for a description. Range 0 to 3
bdi_q20_3=2 ### we will ask toshi for a description. Range 0 to 3
bdi_q21_3=2 ### we will ask toshi for a description. Range 0 to 3

############################################################################################################################################################
### shiny app input ends here


# redefine the categorical covariates
w2=1*(work_condition==2)
w3=1*(work_condition==3)
w4=1*(work_condition==4)
w5=1*(work_condition==5)
w6=1*(work_condition==6)
w7=1*(work_condition==7)
ed2=1*(marriage_condition==2)
ed3=1*(marriage_condition==3)
ed4=1*(marriage_condition==4)


### create data frame
dat1=data.frame(
  age, sex, educatenumber, w2, w3, 
  w4, w5, w6, w7, ed2, ed3, ed4, depression_age, 
  depression_episode_number, episode_months, physical_illness, 
  primemd_q1, primemd_q2, primemd_q3, primemd_q4, primemd_q5, 
  primemd_q6, primemd_q7, primemd_q8, primemd_q9, phq9_q1_1, 
  phq9_q2_1, phq9_q3_1, phq9_q4_1, phq9_q5_1, phq9_q6_1, 
  phq9_q7_1, phq9_q8_1, phq9_q9_1, w1_fibser_q1, w1_fibser_q2, 
  w1_fibser_q3, w1_fibser_q4, bdi_q1_1, bdi_q2_1, bdi_q3_1, 
  bdi_q4_1, bdi_q5_1, bdi_q6_1, bdi_q7_1, bdi_q8_1, bdi_q9_1, 
  bdi_q10_1, bdi_q11_1, bdi_q12_1, bdi_q13_1, bdi_q14_1, 
  bdi_q15_1, bdi_q17_1, bdi_q19_1, bdi_q20_1, bdi_q21_1, 
  phq9_q1_3, phq9_q2_3, phq9_q3_3, phq9_q4_3, phq9_q5_3, 
  phq9_q6_3, phq9_q7_3, phq9_q8_3, phq9_q9_3, w3_fibser_q1, 
  w3_fibser_q2, w3_fibser_q3, w3_fibser_q4, bdi_q1_3, bdi_q2_3, 
  bdi_q3_3, bdi_q4_3, bdi_q5_3, bdi_q6_3, bdi_q7_3, bdi_q8_3, 
  bdi_q9_3, bdi_q10_3, bdi_q11_3, bdi_q12_3, bdi_q13_3, 
  bdi_q14_3, bdi_q15_3, bdi_q17_3, bdi_q19_3, bdi_q20_3, 
  bdi_q21_3, bdi_q16_1longer, bdi_q16_1shorter, bdi_q18_1longer, 
  bdi_q18_1shorter, bdi_q16_3longer, bdi_q16_3shorter, bdi_q18_3longer, 
  bdi_q18_3shorter)


### rescale
dat2=scale(dat1,center=xmeans, scale=xsd)

### predict the outcome for the three different treamtents
y1<- exp(predict(svm_Radial.t1, newdata = dat2))-1   #### continue sertraline
y2<- exp(predict(svm_Radial.t2, newdata = dat2))-1   #### combine with mirtazapine
y3<- exp(predict(svm_Radial.t3, newdata = dat2))-1   #### switch to mirtazapine 
ind=1*(y1<y2&y1<y3)+2*(y2<y1&y2<y3)+3*(y3<y2&y3<y1)
best.treat="continue sertraline"
if(ind==2){best.treat="combine sertraline and mirtazapine"}
if(ind==3){best.treat="switch to mirtazapine"}


best.strategy="combine sertraline and mirtazapine (second best: continue sertraline)"
if(ind==2){best.strategy="combine sertraline and mirtazapine (or switch to mirtazapine)"}
if(ind==3){best.strategy="switch to mirtazapine (or combine sertraline and mirtazapine("}

#### Shiny app output
############################################################################################################################################################
print(paste("The predicted PHQ9 score after 6 weeks, if continuing on sertraline, is ", round(y1,digits=1),".",sep=""))
print(paste("The predicted PHQ9 score after 6 weeks, if combining sertraline and mirtazapine, is ", round(y2,digits=1),".",sep=""))
print(paste("The predicted PHQ9 score after 6 weeks, if switching to mirtazapine, is ", round(y3,digits=1),".",sep=""))

## in bold and blue, and BIG fonts!
print(paste("The best predicted treatment strategy is to ", best.strategy,".",sep=""))
############################################################################################################################################################

