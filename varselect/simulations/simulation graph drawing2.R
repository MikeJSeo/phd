# Loading
library("ggplot2")
library("readxl")

setwd("C:/Users/mike/Desktop/Github/phd/varselect/simulations")

simulation1 <- read_excel("simulation1.result.xlsx")
simulation2 <- read_excel("simulation2.result.xlsx")
simulation3 <- read_excel("simulation3.result.xlsx")
simulation4 <- read_excel("simulation4.result.xlsx")
simulation5 <- read_excel("simulation5.result.xlsx")
simulation6 <- read_excel("simulation6.result.xlsx")
simulation7 <- read_excel("simulation7.result.xlsx")
simulation8 <- read_excel("simulation8.result.xlsx")
simulation9 <- read_excel("simulation9.result.xlsx")
simulation10 <- read_excel("simulation10.result.xlsx")
simulation11 <- read_excel("simulation11.result.xlsx")
simulation12 <- read_excel("simulation12.result.xlsx")
simulation13 <- read_excel("simulation13.result.xlsx")
simulation14 <- read_excel("simulation14.result.xlsx")
simulation15 <- read_excel("simulation15.result.xlsx")
simulation16 <- read_excel("simulation16.result.xlsx")
simulation17 <- read_excel("simulation17.result.xlsx")
simulation18 <- read_excel("simulation18.result.xlsx")
simulation19 <- read_excel("simulation19.result.xlsx")
simulation20 <- read_excel("simulation20.result.xlsx")
simulation21 <- read_excel("simulation21.result.xlsx")
simulation22 <- read_excel("simulation22.result.xlsx")
simulation23 <- read_excel("simulation23.result.xlsx")
simulation24 <- read_excel("simulation24.result.xlsx")
simulation25 <- read_excel("simulation25.result.xlsx")
simulation26 <- read_excel("simulation26.result.xlsx")
simulation27 <- read_excel("simulation27.result.xlsx")
simulation28 <- read_excel("simulation28.result.xlsx")


make_data <- function(simulation_a, simulation_b, a_name, b_name, mse = 4, xlab){
  
  
  data1 <- as.data.frame(simulation_a)[,mse, drop = FALSE]
  colnames(data1) <- "error"
  data1$models <- c("A", "B", "C", "D", "E", "F", "G")
  data1$simulations <- a_name
  
  data2 <- as.data.frame(simulation_b)[,mse, drop = FALSE]
  colnames(data2) <- "error"
  data2$models <- c("A", "B", "C", "D", "E", "F", "G")
  data2$simulations <- b_name
  
  ylab <- ifelse(mse == 4, "patient spec. trt mse", "treatment mse")
  
  data <- rbind(data1, data2)
  list(data = data, ylab = ylab, xlab = xlab)
}


data1 <- make_data(simulation1, simulation15, a_name = "Scenario 1 \n N = 5", b_name = "Scenario 15 \n N = 10",
                  xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.2, no effect modification")

data2 <- make_data(simulation2, simulation16, a_name = "Scenario 2 \n N = 5", b_name = "Scenario 16 \n N = 10",
                   xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.5, no effect modification")

data3 <- make_data(simulation3, simulation17, a_name = "Scenario 3 \n N = 5", b_name = "Scenario 17 \n N = 10",
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.2, small effect modification")

data4 <- make_data(simulation4, simulation18, a_name = "Scenario 4 \n N = 5", b_name = "Scenario 18 \n N = 10",
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.5, small effect modification")

data5 <- make_data(simulation5, simulation19, a_name = "Scenario 5 \n N = 5", b_name = "Scenario 19 \n N = 10", 
                   xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.2, large effect modification")

data6 <- make_data(simulation6, simulation20, a_name = "Scenario 6 \n N = 5", b_name = "Scenario 20 \n N = 10", 
                   xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.5, large effect modification")

data7 <- make_data(simulation7, simulation21, a_name = "Scenario 7 \n N = 5", b_name = "Scenario 21 \n N = 10", 
                   xlab = "15 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")

data8 <- make_data(simulation8, simulation22, a_name = "Scenario 8 \n N = 5", b_name = "Scenario 22 \n N = 10", 
                   xlab = "15 covariates, no effect modifiers \n \u03c4 = 0.2, small effect modification")




plot1 <- ggplot(data=data$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab(data$ylab) + 
  xlab(data$xlab)



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




