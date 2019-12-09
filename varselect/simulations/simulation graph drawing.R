# Loading
library("readxl")

simulation1 <- read_excel("simulation1.result.xlsx")
simulation1[,4]

simulation15 <- read_excel("simulation15.result.xlsx")
simulation15[,4]

data <- as.data.frame(simulation1)[,4, drop = FALSE]
colnames(data) <- "error"
data$models <- c("A", "B", "C", "D", "E", "F", "G")


ggplot(data=data, aes(x=models, y=error)) +
  geom_bar(stat="identity") +
  

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