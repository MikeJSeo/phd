# Loading
library("ggplot2")
library("readxl")

setwd("C:/Users/mike/Desktop/Github/phd/varselect/simulations")

simulation1 <- read_excel("simulation1.result.xlsx")
simulation2 <- read_excel("simulation15.result.xlsx")

data1 <- as.data.frame(simulation1)[,4, drop = FALSE]
colnames(data1) <- "error"
data1$models <- c("A", "B", "C", "D", "E", "F", "G")
data1$simulations <- c("#1 \n N = 5")

data2 <- as.data.frame(simulation2)[,4, drop = FALSE]
colnames(data2) <- "error"
data2$models <- c("A", "B", "C", "D", "E", "F", "G")
data2$simulations <- c("#15 \n N = 10")

data <- rbind(data1, data2)

plot1 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot2 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot3 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot4 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot5 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot6 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot7 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot8 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot9 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot10 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot11 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
plot12 <- ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")


require(gridExtra)
grid.arrange(plot1, plot2, plot3,plot4,plot5,plot6,plot7,plot8,plot9, plot10, plot11, plot12, ncol=4)


for(i in 1:12){
ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") + facet_grid(~simulations) + 
  ylab("patient specific treatment mse") + 
  xlab("10 covariates, 0 effect modifiers \n \u03c4 = 0.2, small effect modifiers")
}



facet_grid(~simulations, labeller = labeller(simulations = c("hi", "hi2"))) +

p + facet_grid(dose ~ supp, labeller = label_both)

# New facet label names for dose variable
dose.labs <- c("D0.5", "D1", "D2")
names(dose.labs) <- c("0.5", "1", "2")

# New facet label names for supp variable
supp.labs <- c("Orange Juice", "Vitamin C")
names(supp.labs) <- c("OJ", "VC")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)
  
  
  df <- data.frame(name=c('foo','bar','foo','bar'),
                   period=c('old','old','recent','recent'),
                   val=c(1.23,2.17,4.15,3.65),
                   stringsAsFactors=F)
df$n = as.numeric(factor(df$period))
df = ddply(df,.(period,name),transform, x=paste(c(rep(' ',n-1), name), collapse=''))
df$x = factor(df$x, levels=df[order(df$val), 'x'])
p = ggplot(data = df, aes(x = x, y = val))
p = p + geom_bar(stat='identity')
p = p + facet_grid(~period, scale='free_x')
p  
  
  

ggplot(data, aes(x = error)) +
  geom_histogram(color="black", fill="white")
  geom_histogram(aes(y = ..count..))

, binwidth = 10,
                 colour = barlines, fill = barfill)


p7 <- ggplot(airquality_trimmed, aes(x = Ozone)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = barfill) +
  scale_x_continuous(name = "Mean ozone in\nparts per billion",
                     breaks = seq(0, 175, 25),
                     limits=c(0, 175)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency histogram of mean ozone") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  facet_grid(. ~ Month.f, scales = "free")
p7