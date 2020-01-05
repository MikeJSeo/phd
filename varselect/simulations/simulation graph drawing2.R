# Loading
library("ggplot2")
library("readxl")

setwd("C:/Users/mike/Desktop/Github/phd/varselect/simulation_results_renumbered")

simulation1 <- read_excel("simulation1.result.xlsx")
simulation2 <- read_excel("simulation2.result.xlsx")
simulation3 <- read_excel("simulation3.result.xlsx")
simulation4 <- read_excel("simulation4.result.xlsx")
simulation5 <- read_excel("simulation5.result.xlsx")
simulation6 <- read_excel("simulation6.result.xlsx")
# simulation7 <- read_excel("simulation7.result.xlsx")
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
# simulation22 <- read_excel("simulation22.result.xlsx")
simulation23 <- read_excel("simulation23.result.xlsx")
simulation24 <- read_excel("simulation24.result.xlsx")
simulation25 <- read_excel("simulation25.result.xlsx")
simulation26 <- read_excel("simulation26.result.xlsx")
simulation27 <- read_excel("simulation27.result.xlsx")
simulation28 <- read_excel("simulation28.result.xlsx")
simulation29 <- read_excel("simulation29.result.xlsx")
simulation30 <- read_excel("simulation30.result.xlsx")
# simulation31 <- read_excel("simulation31.result.xlsx")
# simulation32 <- read_excel("simulation32.result.xlsx")
# simulation33 <- read_excel("simulation33.result.xlsx")
# simulation34 <- read_excel("simulation34.result.xlsx")
# simulation35 <- read_excel("simulation35.result.xlsx")
# simulation36 <- read_excel("simulation36.result.xlsx")
# simulation37 <- read_excel("simulation37.result.xlsx")
# simulation38 <- read_excel("simulation38.result.xlsx")
# simulation39 <- read_excel("simulation39.result.xlsx")
# simulation40 <- read_excel("simulation40.result.xlsx")
# simulation41 <- read_excel("simulation41.result.xlsx")
# simulation42 <- read_excel("simulation42.result.xlsx")
# simulation43 <- read_excel("simulation43.result.xlsx")
# simulation44 <- read_excel("simulation44.result.xlsx")
# simulation45 <- read_excel("simulation45.result.xlsx")
# simulation46 <- read_excel("simulation46.result.xlsx")
# simulation47 <- read_excel("simulation47.result.xlsx")
# simulation48 <- read_excel("simulation48.result.xlsx")
# simulation49 <- read_excel("simulation49.result.xlsx")
# simulation50 <- read_excel("simulation50.result.xlsx")
# simulation51 <- read_excel("simulation51.result.xlsx")
# simulation52 <- read_excel("simulation52.result.xlsx")
# simulation53 <- read_excel("simulation53.result.xlsx")
# simulation54 <- read_excel("simulation54.result.xlsx")
# simulation55 <- read_excel("simulation55.result.xlsx")
# simulation56 <- read_excel("simulation56.result.xlsx")
# simulation57 <- read_excel("simulation57.result.xlsx")
# simulation58 <- read_excel("simulation58.result.xlsx")
# simulation59 <- read_excel("simulation59.result.xlsx")
# simulation60 <- read_excel("simulation60.result.xlsx")




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


data1 <- make_data(simulation1, simulation16, a_name = "Scenario 1 \n N = 5", b_name = "Scenario 16 \n N = 10",
                  xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.2, no effect modification")

data2 <- make_data(simulation2, simulation17, a_name = "Scenario 2 \n N = 5", b_name = "Scenario 17 \n N = 10",
                   xlab = "10 covariates, no effect modifiers \n \u03c4 = 0.5, no effect modification")

data3 <- make_data(simulation3, simulation18, a_name = "Scenario 3 \n N = 5", b_name = "Scenario 18 \n N = 10",
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.2, small effect modification")

data4 <- make_data(simulation4, simulation19, a_name = "Scenario 4 \n N = 5", b_name = "Scenario 19 \n N = 10",
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.5, small effect modification")

data5 <- make_data(simulation5, simulation20, a_name = "Scenario 5 \n N = 5", b_name = "Scenario 20 \n N = 10", 
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.2, large effect modification")

data6 <- make_data(simulation6, simulation21, a_name = "Scenario 6 \n N = 5", b_name = "Scenario 21 \n N = 10", 
                   xlab = "10 covariates, one effect modifiers \n \u03c4 = 0.5, large effect modification")

# data7 <- make_data(simulation7, simulation22, a_name = "Scenario 7 \n N = 5", b_name = "Scenario 22 \n N = 10", 
#                    xlab = "10 covariates, ten effect modifiers \n \u03c4 = 0.2, small effect modification")

data8 <- make_data(simulation8, simulation23, a_name = "Scenario 8 \n N = 5", b_name = "Scenario 23 \n N = 10", 
                   xlab = "15 covariates, two effect modifiers \n \u03c4 = 0.2, small effect modification")

data9 <- make_data(simulation9, simulation24, a_name = "Scenario 9 \n N = 5", b_name = "Scenario 24 \n N = 10", 
                   xlab = "15 covariates, two effect modifiers \n \u03c4 = 0.5, small effect modification")

data10 <- make_data(simulation10, simulation25, a_name = "Scenario 10 \n N = 5", b_name = "Scenario 25 \n N = 10", 
                   xlab = "15 covariates, two effect modifiers \n \u03c4 = 0.2, large effect modification")

data11 <- make_data(simulation11, simulation26, a_name = "Scenario 11 \n N = 5", b_name = "Scenario 26 \n N = 10", 
                    xlab = "15 covariates, two effect modifiers \n \u03c4 = 0.5, large effect modification")

data12 <- make_data(simulation12, simulation27, a_name = "Scenario 12 \n N = 5", b_name = "Scenario 27 \n N = 10", 
                    xlab = "15 covariates, three effect modifiers \n \u03c4 = 0.2, small effect modification")

data13 <- make_data(simulation13, simulation28, a_name = "Scenario 13 \n N = 5", b_name = "Scenario 28 \n N = 10", 
                    xlab = "15 covariates, three effect modifiers \n \u03c4 = 0.5, small effect modification")

data14 <- make_data(simulation14, simulation29, a_name = "Scenario 14 \n N = 5", b_name = "Scenario 29 \n N = 10", 
                    xlab = "15 covariates, three effect modifiers \n \u03c4 = 0.2, large effect modification")

data15 <- make_data(simulation15, simulation30, a_name = "Scenario 15 \n N = 5", b_name = "Scenario 30 \n N = 10", 
                    xlab = "15 covariates, three effect modifiers \n \u03c4 = 0.5, large effect modification")




plot1 <- ggplot(data=data1$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab(data1$ylab) + 
  xlab(data1$xlab)




require(gridExtra)
grid.arrange(plot1,plot2, plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16,plot17,plot18,plot19,plot20,plot21,plot22,plot23,plot24,plot25,plot26,plot27,plot28,plot29,plot30, ncol=6)




