# Loading
library("ggplot2")
library("readxl")

setwd("C:/Users/mike/Desktop/Github/phd/varselect/simulations")

simulation1 <- read_excel("simulation1.result.xlsx")
simulation2 <- read_excel("simulation15.result.xlsx")

data1 <- as.data.frame(simulation1)[,4, drop = FALSE]
colnames(data1) <- "error"
data1$models <- c("A", "B", "C", "D", "E", "F", "G")
data1$simulations <- c("Scenario 1 \n N = 5")

data2 <- as.data.frame(simulation2)[,4, drop = FALSE]
colnames(data2) <- "error"
data2$models <- c("A", "B", "C", "D", "E", "F", "G")
data2$simulations <- c("Scenario 15 \n N = 10")

data <- rbind(data1, data2)

plot1 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient spec. trt mse") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot2 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot3 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot4 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot5 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot6 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot7 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient spec. trt mse") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot8 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot9 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot10 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot11 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot12 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot13 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient spec. trt mse") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot14 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot15 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot16 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot17 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot18 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot19 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient spec. trt mse") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot20 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot21 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot22 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot23 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot24 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot25 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient spec. trt mse") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot26 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot27 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot28 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot29 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")
plot30 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("") + 
  xlab("10 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")




require(gridExtra)
grid.arrange(plot1,plot2, plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16,plot17,plot18,plot19,plot20,plot21,plot22,plot23,plot24,plot25,plot26,plot27,plot28,plot29,plot30, ncol=6)




