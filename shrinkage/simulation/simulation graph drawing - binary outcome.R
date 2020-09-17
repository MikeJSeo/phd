# Loading
library("ggplot2")
library("readxl")
library("grid")
require("gridExtra")

setwd("~/GitHub/phd/shrinkage/simulation/simulation_results")


B1 <- read_excel("B1.xlsx")
B2 <- read_excel("B2.xlsx")
B3 <- read_excel("B3.xlsx")
B4 <- read_excel("B4.xlsx")
B5 <- read_excel("B5.xlsx")
B6 <- read_excel("B6.xlsx")
B7 <- read_excel("B7.xlsx")
B8 <- read_excel("B8.xlsx")
B9 <- read_excel("B9.xlsx")
B10 <- read_excel("B10.xlsx")
B11 <- read_excel("B11.xlsx")
B12 <- read_excel("B12.xlsx")
B13 <- read_excel("B13.xlsx")
B14 <- read_excel("B14.xlsx")
B15 <- read_excel("B15.xlsx")
B16 <- read_excel("B16.xlsx")
B17 <- read_excel("B17.xlsx")
B18 <- read_excel("B18.xlsx")
B19 <- read_excel("B19.xlsx")
B20 <- read_excel("B20.xlsx")
B21 <- read_excel("B21.xlsx")
B22 <- read_excel("B22.xlsx")
B23 <- read_excel("B23.xlsx")
B24 <- read_excel("B24.xlsx")
B25 <- read_excel("B25.xlsx")
B26 <- read_excel("B26.xlsx")
B27 <- read_excel("B27.xlsx")
B28 <- read_excel("B28.xlsx")
B29 <- read_excel("B29.xlsx")
B30 <- read_excel("B30.xlsx")
B31 <- read_excel("B31.xlsx")
B32 <- read_excel("B32.xlsx")
B33 <- read_excel("B33.xlsx")
B34 <- read_excel("B34.xlsx")
B35 <- read_excel("B35.xlsx")
B36 <- read_excel("B36.xlsx")

maxy <- 0.20
acc <- 0.01

make_data <- function(simulation_a, simulation_b, a_name, b_name, mse = 1, xlab){
  
  
  data1 <- as.data.frame(simulation_a)[,mse, drop = FALSE]
  colnames(data1) <- "error"
  data1$models <- c("A", "B", "C", "D", "E", "G", "H", "F")
  data1$simulations <- a_name
  
  data2 <- as.data.frame(simulation_b)[,mse, drop = FALSE]
  colnames(data2) <- "error"
  data2$models <- c("A", "B", "C", "D", "E", "G", "H", "F")
  data2$simulations <- b_name
  
  data <- rbind(data1, data2)
  
  data$simulations <- factor(c(rep(a_name, 8), rep(b_name,8)), levels = c(a_name, b_name))
  list(data = data, xlab = xlab)
}


### grouped by sample size
data1 <- make_data(B1, B15, a_name = "B1 \n N = 5", b_name = "B15 \n N = 10",
                   xlab = "10 covariates, 0 effect modifier \n \u03c4 = 0.2, no effect modification")

data2 <- make_data(B2, B16, a_name = "B2 \n N = 5", b_name = "B16 \n N = 10",
                   xlab = "10 covariates, 0 effect modifier \n \u03c4 = 0.5, no effect modification")

data3 <- make_data(B3, B17, a_name = "B3 \n N = 5", b_name = "B17 \n N = 10",
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.2, small effect modification")

data4 <- make_data(B4, B18, a_name = "B4 \n N = 5", b_name = "B18 \n N = 10",
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.5, small effect modification")

data5 <- make_data(B5, B19, a_name = "B5 \n N = 5", b_name = "B19 \n N = 10", 
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.2, large effect modification")

data6 <- make_data(B6, B20, a_name = "B6 \n N = 5", b_name = "B20 \n N = 10", 
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.5, large effect modification")

data7 <- make_data(B7, B21, a_name = "B7 \n N = 5", b_name = "B21 \n N = 10",
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.2, small effect modification")

data8 <- make_data(B8, B22, a_name = "B8 \n N = 5", b_name = "B22 \n N = 10", 
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.5, small effect modification")

data9 <- make_data(B9, B23, a_name = "B9 \n N = 5", b_name = "B23 \n N = 10", 
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.2, large effect modification")

data10 <- make_data(B10, B24, a_name = "B10 \n N = 5", b_name = "B24 \n N = 10", 
                    xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.5, large effect modification")

data11 <- make_data(B11, B25, a_name = "B11 \n N = 5", b_name = "B25 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.2, small effect modification")

data12 <- make_data(B12, B26, a_name = "B12 \n N = 5", b_name = "B26 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.5, small effect modification")

data13 <- make_data(B13, B27, a_name = "B13 \n N = 5", b_name = "B27 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.2, large effect modification")

data14 <- make_data(B14, B28, a_name = "B14 \n N = 5", b_name = "B28 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.5, large effect modification")

data15 <- make_data(B29, B30, a_name = "B29 \n \u03c4 = 0.2", b_name = "B30 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 10 effect modifiers \n N = 5, small effect modification")

data16 <- make_data(B31, B32, a_name = "B31 \n Uniform(-0.4,0.4)", b_name = "B32 \n Uniform(-1.0,1.0)", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification")

data17 <- make_data(B33, B34, a_name = "B33 \n \u03c4 = 0.2", b_name = "B34 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification \n sample size 50 to 500")

data18 <- make_data(B35, B36, a_name = "B35 \n \u03c4 = 0.2", b_name = "B36 \n \u03c4 = 0.5", 
                    xlab = "30 covariates, 1 effect modifier \n N = 5, small effect modification")

##################################
####grouped by heterogeneity
data1 <- make_data(B1, B2, a_name = "B1 \n \u03c4 = 0.2", b_name = "B2 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 0 effect modifier \n N = 5, no effect modification")

data2 <- make_data(B3, B4, a_name = "B3 \n \u03c4 = 0.2", b_name = "B4 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification")

data3 <- make_data(B5, B6, a_name = "B5 \n \u03c4 = 0.2", b_name = "B6 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 1 effect modifier \n N = 5, large effect modification")

data4 <- make_data(B7, B8, a_name = "B7 \n \u03c4 = 0.2", b_name = "B8 \n \u03c4 = 0.5",
                   xlab = "15 covariates, 2 effect modifiers \n N = 5, small effect modification")

data5 <- make_data(B9, B10, a_name = "B9 \n \u03c4 = 0.2", b_name = "B10 \n \u03c4 = 0.5", 
                   xlab = "15 covariates, 2 effect modifiers \n N = 5, large effect modification")

data6 <- make_data(B11, B12, a_name = "B11 \n \u03c4 = 0.2", b_name = "B12 \n \u03c4 = 0.5", 
                   xlab = "15 covariates, 3 effect modifiers \n N = 5, small effect modification")

data7 <- make_data(B13, B14, a_name = "B13 \n \u03c4 = 0.2", b_name = "B14 \n \u03c4 = 0.5",
                   xlab = "15 covariates, 3 effect modifiers \n N = 5, large effect modification")

data8 <- make_data(B15, B16, a_name = "B15 \n \u03c4 = 0.2", b_name = "B16 \n \u03c4 = 0.5", 
                   xlab = "10 covariates, 0 effect modifier \n N = 10, no effect modification")

data9 <- make_data(B17, B18, a_name = "B17 \n \u03c4 = 0.2", b_name = "B18 \n \u03c4 = 0.5", 
                   xlab = "10 covariates, 1 effect modifier \n N = 10, small effect modification")

data10 <- make_data(B19, B20, a_name = "B19 \n \u03c4 = 0.2", b_name = "B20 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 10, large effect modification")

data11 <- make_data(B21, B22, a_name = "B21 \n \u03c4 = 0.2", b_name = "B22 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 2 effect modifiers \n N = 10, small effect modification")

data12 <- make_data(B23, B24, a_name = "B23 \n \u03c4 = 0.2", b_name = "B24 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 2 effect modifiers \n N = 10, large effect modification")

data13 <- make_data(B25, B26, a_name = "B25 \n \u03c4 = 0.2", b_name = "B26 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 3 effect modifiers \n N = 10, small effect modification")

data14 <- make_data(B27, B28, a_name = "B27 \n \u03c4 = 0.2", b_name = "B28 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 3 effect modifiers \n N = 10, large effect modification")

data15 <- make_data(B29, B30, a_name = "B29 \n \u03c4 = 0.2", b_name = "B30 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 10 effect modifiers \n N = 5, large effect modification")

data16 <- make_data(B31, B32, a_name = "B31 \n Uniform(-0.4,0.4)", b_name = "B32 \n Uniform(-1.0,1.0)", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification")

data17 <- make_data(B33, B34, a_name = "B33 \n \u03c4 = 0.2", b_name = "B34 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification \n sample size 50 to 500")

data18 <- make_data(B35, B36, a_name = "B35 \n \u03c4 = 0.2", b_name = "B36 \n \u03c4 = 0.5", 
                    xlab = "30 covariates, 1 effect modifier \n N = 5, small effect modification")




#################################

plot1 <- ggplot(data=data1$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) + 
  xlab(data1$xlab)

plot2 <- ggplot(data=data2$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data2$xlab)

plot3 <- ggplot(data=data3$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data3$xlab)

plot4 <- ggplot(data=data4$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data4$xlab)

plot5 <- ggplot(data=data5$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data5$xlab)

plot6 <- ggplot(data=data6$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data6$xlab)

plot7 <- ggplot(data=data7$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data7$xlab)

plot8 <- ggplot(data=data8$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data8$xlab)

plot9 <- ggplot(data=data9$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data9$xlab)

plot10 <- ggplot(data=data10$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data10$xlab)

plot11 <- ggplot(data=data11$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data11$xlab)

plot12 <- ggplot(data=data12$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data12$xlab)

plot13 <- ggplot(data=data13$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data13$xlab)

plot13 <- ggplot(data=data13$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data13$xlab)

plot14 <- ggplot(data=data14$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data14$xlab)

plot15 <- ggplot(data=data15$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data15$xlab)

plot16 <- ggplot(data=data16$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data16$xlab)

plot17 <- ggplot(data=data17$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data17$xlab)

plot18 <- ggplot(data=data18$data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) +
  theme(axis.title.y=element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = acc),
                     limits = c(0,maxy)) +
  xlab(data18$xlab)


#patient specific traetment MSE
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         ncol=6, as.table = FALSE),
             left = textGrob("Patient specific treatment MSE", rot = 90, vjust = 0.5))

#Treatment MSE
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         ncol=6, as.table = FALSE),
             left = textGrob("Treatment MSE", rot = 90, vjust = 0.5))


#true effect modifier
grid.arrange(arrangeGrob(plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         ncol=4, as.table = FALSE),
             left = textGrob("True effect modifier MSE", rot = 90, vjust = 0.5))

#false effect modifier
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot16, plot17, plot18,
                         ncol=6, as.table = FALSE),
             left = textGrob("False effect modifier MSE", rot = 90, vjust = 0.5))



