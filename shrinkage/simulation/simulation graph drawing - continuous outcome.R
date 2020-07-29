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



make_data <- function(simulation_a, simulation_b, a_name, b_name, mse = 4, xlab){
  
  
  data1 <- as.data.frame(simulation_a)[,mse, drop = FALSE]
  colnames(data1) <- "error"
  data1$models <- c("A", "B", "C", "D", "E", "F", "G")
  data1$simulations <- a_name
  
  data2 <- as.data.frame(simulation_b)[,mse, drop = FALSE]
  colnames(data2) <- "error"
  data2$models <- c("A", "B", "C", "D", "E", "F", "G")
  data2$simulations <- b_name
  
  data <- rbind(data1, data2)
  
  data$simulations <- factor(c(rep(a_name, 7), rep(b_name,7)), levels = c(a_name, b_name))
  list(data = data, xlab = xlab)
}