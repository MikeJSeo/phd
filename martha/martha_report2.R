library("readxl")
#devtools::install_github("guido-s/netmeta", ref = "develop", force = TRUE)
#library(netmeta)
#library(meta)
#library(knitr)
#library(metasens)

setwd("C:/Users/ms19g661/Desktop")
original <- read_excel("martha.xlsx", sheet = "AE DATA inc. update ", col_names = FALSE)
original <- as.data.frame(original) # change from tibble to data.frame

original[original == "Placebo"] = "placebo"
if(original[842, 4] == "*") original[842,4] <- "paroxetine"
original[!is.na(original[,1]) & original[,1] == "Jefferson2000 (29060/785)" & original[,5] == "paroxetine CR", 4] <- "paroxetine"
original[1,]
Jefferson2000 (29060/785)

