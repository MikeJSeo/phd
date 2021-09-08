library(caret)
library(kernlab)
library(gridExtra)

setwd("C:/Users/ms19g661/Desktop")
load("data for main analysis")

### fit the SVMs
#grid_radial <- expand.grid(sigma = c(0.0005, 0.001, 0.002), C = c(  0.4, 1, 1.6))
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
svm_Radial.t1 <- train(y ~., data = dat.t1, method = "svmRadial", trControl=trctrl, tuneLength = 20, selectionFunction ="min")

svm_Radial.t2<- train(y ~., data = dat.t2, method = "svmRadial",trControl=trctrl,tuneLength = 20,selectionFunction ="min")

svm_Radial.t3<- train(y ~., data = dat.t3, method = "svmRadial",trControl=trctrl,tuneLength = 20, selectionFunction ="min")


#### in-sample predictions
y.in.1<- predict(svm_Radial.t1, newdata = dat.t1)
median(abs(exp(dat.t1$y)-exp(y.in.1))) 

y.in.2<- predict(svm_Radial.t2, newdata = dat.t2)
median(abs(exp(dat.t2$y)-exp(y.in.2))) 

y.in.3<- predict(svm_Radial.t3, newdata = dat.t3)
median(abs(exp(dat.t3$y)-exp(y.in.3))) 

abs.error=data.frame(a1=c(abs(exp(dat.t1$y)-exp(y.in.1)), abs(exp(dat.t2$y)-exp(y.in.2)), abs(exp(dat.t3$y)-exp(y.in.3)))
                     ,t=c(rep("A", length(y.in.1)),rep("B", length(y.in.2)), rep("C", length(y.in.3)) ))
median(abs.error$a1)

ggplot(abs.error, aes(x=a1)) +
  geom_histogram(color="black", fill="white", binwidth = 1)

##### using the model
dat3.1=dat3
dat3.1$t3=0
dat3.1$t2=0
y1<- exp(predict(svm_Radial.t1, newdata = dat3.1))-1
median(abs(y1[dat3$t2==0&dat3$t3==0]-exp(dat3.1[dat3$t2==0&dat3$t3==0,]$y)+1), na.rm=T)

dat3.2=dat3
dat3.2$t3=0
dat3.2$t2=1
y2<- exp(predict(svm_Radial.t2, newdata = dat3.2))-1
median(abs(y2[dat3$t2==1&dat3$t3==0]-exp(dat3.2[dat3$t2==1&dat3$t3==0,]$y)+1), na.rm=T)

dat3.3=dat3
dat3.3$t3=1
dat3.3$t2=0
y3<- exp(predict(svm_Radial.t3, newdata = dat3.3))-1
median(abs(y3[dat3$t3==1&dat3$t2==0]-exp(dat3.3[dat3$t3==1&dat3$t2==0,]$y)+1), na.rm=T)

pred.compare=data.frame(exp(dat3$y)-1,y1,y2,y3)
colnames(pred.compare)=c("y","y1","y2","y3")
pred.compare$best=1*(pred.compare$y1<pred.compare$y2&pred.compare$y1<pred.compare$y3)+
  2*(pred.compare$y2<pred.compare$y1&pred.compare$y2<pred.compare$y3)+
  3*(pred.compare$y3<pred.compare$y1&pred.compare$y3<pred.compare$y2)

pred.compare$treat=1*(dat3$t2==0&dat3$t3==0)+
  2*(dat3$t2==1&dat3$t3==0)+
  3*(dat3$t2==0&dat3$t3==1)

histogram(pred.compare$best)
table((pred.compare$best))

#### histograms of PAI treatment 2 vs 3
pai23<-ggplot(pred.compare, aes(x=y2-y3)) + 
  geom_histogram(color="black", fill="white", binwidth = 1)
pai23

pai12<-ggplot(pred.compare, aes(x=y1-y2)) + 
  geom_histogram(color="black", fill="white", binwidth = 1)
pai12

pai13<-ggplot(pred.compare, aes(x=y1-y3)) + 
  geom_histogram(color="black", fill="white", binwidth = 1)
pai13

grid.arrange(pai12, pai13, pai23,nrow = 1)
