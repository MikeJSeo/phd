library(RColorBrewer)

validation <- c(rep("Internal validation", 9*4), c(rep("Internal-external validation", 9*4)))    
Approach <- rep(c("Approach I", "Approach II(a)", "Approach II(b)", "Approach II(c)", "Approach III(a) w=0.25", "Approach III(a) w=0.5", "Approach III(b) w=0.25","Approach III(b) w=0.5", "Approach IV"), 8)
dataset <- rep(c("SCQM MSE", "SCQM Bias", "BSRBR-RA MSE", "BSRBR-RA Bias"), each = 9)
#dataset <- factor(dataset, levels = c("SCQM MSE", "SCQM Bias", "BSRBR-RA MSE", "BSRBR-RA Bias"))
value <- c(1.44, 1.54, 1.59, 1.46, 1.54, 1.53, 1.47, 1.47, 1.62,
           0.10, 0.22, 0.25, 0.05, 0.18, 0.18, 0.04, 0.05, 0.27,
           1.39, 1.43, 1.43, 1.45, 1.42, 1.41, 1.44, 1.44, 1.45,
           0.13, 0.11, 0.07, 0.19, 0.09, 0.05, 0.18, 0.20, 0.05,
           1.85, 1.74, 1.84, 1.85, 1.87, 1.86, 1.84, 1.84, 1.86,
           0.51, 0.42, 0.48, 0.48, 0.50, 0.51, 0.47, 0.48, 0.50,
           1.67, 1.72, 1.55, 1.71, 1.74, 1.73, 1.79, 1.76, 1.52, 
           -0.05, 0.11, 0.02, -0.24, -0.17, -0.09, -0.38, -0.35, -0.02
           )

max.height <- c(rep(2.5, 9),
                rep(0.3, 9),
                rep(2.5, 9),
                rep(0.3, 9),
                rep(2.5, 9),
                rep(1, 9),
                rep(2.5, 9),
                rep(0.25, 9)
                )
dat <- data.frame(validation, Approach, value = value)
dat$dataset <- factor(dataset, levels = c("SCQM MSE", "BSRBR-RA MSE", "SCQM Bias", "BSRBR-RA Bias"))
dat$validation <- factor(validation, levels = c("Internal validation", "Internal-external validation"))

library(reshape2)
library(ggplot2)
# simple barchart

nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

ggplot(dat, aes(x = Approach, y= value, fill = Approach, position = "dodge")) + geom_bar(stat = "identity") + 
  theme(axis.title.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=mycolors) +
  geom_blank(aes(y=max.height)) + 
  facet_wrap (validation~dataset, ncol = 4, scales = "free")

# ggplot(dat, aes(x = Approach, y= value, fill = Approach, position = "dodge")) + geom_bar(stat = "identity") + 
#   theme(axis.title.y = element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   scale_fill_manual(values=mycolors) +
#   geom_blank(aes(y=max.height)) + 
#   facet_grid (validation~dataset, switch = "y", scales = "free")
  
