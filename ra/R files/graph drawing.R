

validation <- c(rep("internal", 8*4), c(rep("external", 8*4)))    
Approach <- rep(c("Approach I", "Approach II(a)", "Approach II(b)", "Approach II(c)", "Approach III(a) w=0.25", "Approach III(a) w=0.5", "Approach III(b) w=0.25","Approach III(b) w=0.5"), 8)
dataset <- rep(c("SCQM MSE", "SCQM Bias", "BSRBR-RA MSE", "BSRBR-RA Bias"), each = 8)
#dataset <- factor(dataset, levels = c("SCQM MSE", "SCQM Bias", "BSRBR-RA MSE", "BSRBR-RA Bias"))
dat <- data.frame(validation, Approach, value = 1:64)
dat$dataset <- factor(dataset, levels = c("SCQM MSE", "SCQM Bias", "BSRBR-RA MSE", "BSRBR-RA Bias"))

library(reshape2)
library(ggplot2)
# simple barchart
ggplot(dat, aes(x = Approach, y= value, fill = Approach)) + geom_bar(stat = "identity") + 
 facet_grid (validation~dataset,switch = "y") + 
  theme(axis.title.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
  
