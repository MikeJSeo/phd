# Loading
library("ggplot2")
library("readxl")
library("grid")
require("gridExtra")

setwd("~/GitHub/phd/shrinkage/simulation/simulation_results")


C1 <- read_excel("C1.xlsx")
C2 <- read_excel("C2.xlsx")
C3 <- read_excel("C3.xlsx")
C4 <- read_excel("C4.xlsx")
C5 <- read_excel("C5.xlsx")
C6 <- read_excel("C6.xlsx")
C7 <- read_excel("C7.xlsx")
C8 <- read_excel("C8.xlsx")
C9 <- read_excel("C9.xlsx")
C10 <- read_excel("C10.xlsx")
C11 <- read_excel("C11.xlsx")
C12 <- read_excel("C12.xlsx")
C13 <- read_excel("C13.xlsx")
C14 <- read_excel("C14.xlsx")
C15 <- read_excel("C15.xlsx")
C16 <- read_excel("C16.xlsx")
C17 <- read_excel("C17.xlsx")
C18 <- read_excel("C18.xlsx")
C19 <- read_excel("C19.xlsx")
C20 <- read_excel("C20.xlsx")
C21 <- read_excel("C21.xlsx")
C22 <- read_excel("C22.xlsx")
C23 <- read_excel("C23.xlsx")
C24 <- read_excel("C24.xlsx")
C25 <- read_excel("C25.xlsx")
C26 <- read_excel("C26.xlsx")
C27 <- read_excel("C27.xlsx")
C28 <- read_excel("C28.xlsx")
C29 <- read_excel("C29.xlsx")
C30 <- read_excel("C30.xlsx")

maxy <- 0.2
acc <- 0.01

make_data <- function(simulation_a, simulation_b, a_name, b_name, mse = 4, xlab){
  
  
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


### grouped by number of studies
data1 <- make_data(C1, C15, a_name = "C1 \n N = 5", b_name = "C15 \n N = 10",
                   xlab = "10 covariates, 0 effect modifier \n \u03c4 = 0.2, no effect modification")

data2 <- make_data(C2, C16, a_name = "C2 \n N = 5", b_name = "C16 \n N = 10",
                   xlab = "10 covariates, 0 effect modifier \n \u03c4 = 0.5, no effect modification")

data3 <- make_data(C3, C17, a_name = "C3 \n N = 5", b_name = "C17 \n N = 10",
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.2, small effect modification")

data4 <- make_data(C4, C18, a_name = "C4 \n N = 5", b_name = "C18 \n N = 10",
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.5, small effect modification")

data5 <- make_data(C5, C19, a_name = "C5 \n N = 5", b_name = "C19 \n N = 10", 
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.2, large effect modification")

data6 <- make_data(C6, C20, a_name = "C6 \n N = 5", b_name = "C20 \n N = 10", 
                   xlab = "10 covariates, 1 effect modifier \n \u03c4 = 0.5, large effect modification")

data7 <- make_data(C7, C21, a_name = "C7 \n N = 5", b_name = "C21 \n N = 10",
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.2, small effect modification")

data8 <- make_data(C8, C22, a_name = "C8 \n N = 5", b_name = "C22 \n N = 10", 
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.5, small effect modification")

data9 <- make_data(C9, C23, a_name = "C9 \n N = 5", b_name = "C23 \n N = 10", 
                   xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.2, large effect modification")

data10 <- make_data(C10, C24, a_name = "C10 \n N = 5", b_name = "C24 \n N = 10", 
                    xlab = "15 covariates, 2 effect modifiers \n \u03c4 = 0.5, large effect modification")

data11 <- make_data(C11, C25, a_name = "C11 \n N = 5", b_name = "C25 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.2, small effect modification")

data12 <- make_data(C12, C26, a_name = "C12 \n N = 5", b_name = "C26 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.5, small effect modification")

data13 <- make_data(C13, C27, a_name = "C13 \n N = 5", b_name = "C27 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.2, large effect modification")

data14 <- make_data(C14, C28, a_name = "C14 \n N = 5", b_name = "C28 \n N = 10", 
                    xlab = "15 covariates, 3 effect modifiers \n \u03c4 = 0.5, large effect modification")

data15 <- make_data(C29, C30, a_name = "C29 \n \u03c4 = 0.2", b_name = "C30 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 10 effect modifiers \n N = 5, small effect modification")

data16 <- make_data(C29, C30, a_name = "C31 \n Uniform(-0.4,0.4)", b_name = "C32 \n Uniform(-1.0,1.0)", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification")

data17 <- make_data(C29, C30, a_name = "C33 \n \u03c4 = 0.2", b_name = "C34 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification \n sample size 50 to 500")

data18 <- make_data(C29, C30, a_name = "C35 \n \u03c4 = 0.2", b_name = "C36 \n \u03c4 = 0.5", 
                    xlab = "30 covariates, 1 effect modifier \n N = 5, small effect modification")


#grouped by heterogeneity
data1 <- make_data(C1, C2, a_name = "C1 \n \u03c4 = 0.2", b_name = "C2 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 0 effect modifier \n N = 5, no effect modification")

data2 <- make_data(C3, C4, a_name = "C3 \n \u03c4 = 0.2", b_name = "C4 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 1 effect modifier \n N = 5, small effect modification")

data3 <- make_data(C5, C6, a_name = "C5 \n \u03c4 = 0.2", b_name = "C6 \n \u03c4 = 0.5",
                   xlab = "10 covariates, 1 effect modifier \n N = 5, large effect modification")

data4 <- make_data(C7, C8, a_name = "C7 \n \u03c4 = 0.2", b_name = "C8 \n \u03c4 = 0.5",
                   xlab = "15 covariates, 2 effect modifiers \n N = 5, small effect modification")

data5 <- make_data(C9, C10, a_name = "C9 \n \u03c4 = 0.2", b_name = "C10 \n \u03c4 = 0.5", 
                   xlab = "15 covariates, 2 effect modifiers \n N = 5, large effect modification")

data6 <- make_data(C11, C12, a_name = "C11 \n \u03c4 = 0.2", b_name = "C12 \n \u03c4 = 0.5", 
                   xlab = "15 covariates, 3 effect modifiers \n N = 5, small effect modification")

data7 <- make_data(C13, C14, a_name = "C13 \n \u03c4 = 0.2", b_name = "C14 \n \u03c4 = 0.5",
                   xlab = "15 covariates, 3 effect modifiers \n N = 5, large effect modification")

data8 <- make_data(C15, C16, a_name = "C15 \n \u03c4 = 0.2", b_name = "C16 \n \u03c4 = 0.5", 
                   xlab = "10 covariates, 0 effect modifier \n N = 10, no effect modification")

data9 <- make_data(C17, C18, a_name = "C17 \n \u03c4 = 0.2", b_name = "C18 \n \u03c4 = 0.5", 
                   xlab = "10 covariates, 1 effect modifier \n N = 10, small effect modification")

data10 <- make_data(C19, C20, a_name = "C19 \n \u03c4 = 0.2", b_name = "C20 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 10, large effect modification")

data11 <- make_data(C21, C22, a_name = "C21 \n \u03c4 = 0.2", b_name = "C22 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 2 effect modifiers \n N = 10, small effect modification")

data12 <- make_data(C23, C24, a_name = "C23 \n \u03c4 = 0.2", b_name = "C24 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 2 effect modifiers \n N = 10, large effect modification")

data13 <- make_data(C25, C26, a_name = "C25 \n \u03c4 = 0.2", b_name = "C26 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 3 effect modifiers \n N = 10, small effect modification")

data14 <- make_data(C27, C28, a_name = "C27 \n \u03c4 = 0.2", b_name = "C28 \n \u03c4 = 0.5", 
                    xlab = "15 covariates, 3 effect modifiers \n N = 10, large effect modification")

data15 <- make_data(C29, C30, a_name = "C29 \n \u03c4 = 0.2", b_name = "C30 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 10 effect modifiers \n N = 5, small effect modification")

data16 <- make_data(C29, C30, a_name = "C31 \n Uniform(-0.4,0.4)", b_name = "C32 \n Uniform(-1.0,1.0)", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, large effect modification")

data17 <- make_data(C29, C30, a_name = "C33 \n \u03c4 = 0.2", b_name = "C34 \n \u03c4 = 0.5", 
                    xlab = "10 covariates, 1 effect modifier \n N = 5, large effect modification \n sample size 50 to 500")

data18 <- make_data(C29, C30, a_name = "C35 \n \u03c4 = 0.2", b_name = "C36 \n \u03c4 = 0.5", 
                    xlab = "30 covariates, 1 effect modifier \n N = 5, large effect modification")

##########################



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


#windowsFonts(Times = windowsFont("Times New Roman"))
#fg <- frameGrob()
#tg <- textGrob("Continuous outcome", gp = gpar(fontsize = 28, fontfamily= "Times"))
#rg <- rectGrob(x = tg$x+unit(4, "mm"), y = tg$y, width = stringWidth(tg$label)*5 + unit(9, "mm") ,                 
#               height = stringHeight(tg$label) + unit(10,"mm"), gp = gpar(fill = "light grey", lty = 0))
#fg <- packGrob(fg, rg)
#fg <- packGrob(fg, tg)


# fg2 <- frameGrob()
# tg2 <- textGrob("Binary outcome", gp = gpar(fontsize = 28, fontfamily= "Times"))
# rg2 <- rectGrob(x = tg2$x+unit(4, "mm"), y = tg2$y, width = stringWidth(tg$label)*5 + unit(9, "mm"),                 
#                 height = stringHeight(tg2$label) + unit(10,"mm"), gp = gpar(fill = "light grey", lty = 0))
# fg2 <- packGrob(fg2, rg2)
# fg2 <- packGrob(fg2, tg2)


#patient specific treatment MSE
grid.arrange(arrangeGrob(plot1,plot2, plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         top=NULL, ncol=6, as.table = FALSE),
             left = textGrob("Patient specific treatment MSE", rot = 90, vjust = 0.5))


#treatment MSE
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         ncol=6, as.table = FALSE),
             left = textGrob("Treatment MSE", rot = 90, vjust = 0.5))

#true effect modifier
grid.arrange(arrangeGrob(plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18,
                         ncol=4, as.table = FALSE),
             left = textGrob("True effect modifier MSE", rot = 90, vjust = 0.5))


#false effect modifier
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, plot13, plot14, plot14, plot14, plot14,
                         ncol=6, as.table = FALSE),
             left = textGrob("False effect modifier MSE", rot = 90, vjust = 0.5))

