library("readxl")
#devtools::install_github("guido-s/netmeta", ref = "develop", force = TRUE)
library(netmeta)
library(meta)

setwd("C:/Users/ms19g661/Desktop") #change working directory
#setwd("C:/Users/mike/Desktop")

original <- read_excel("martha.xlsx", sheet = "DATA", col_names = FALSE)
original <- as.data.frame(original)

#remove last column which is sum
original <- original[-length(original[,1]),]

#remove all NA rows (last five rows)
row.has.all.na <- apply(original, 1, function(x){all(is.na(x))})
original <- original[!row.has.all.na,]

# Fix missing treatment; Study ID = Jefferson2000 (29060/785)
if(original[839, 4] == "*") original[839,4] <- "paroxetine"
original[original == "Placebo"] = "placebo"


#####################################################################
# Preprocessing:#####################################################
# write a function that would create separate dataset for each outcome

split_data <- function(martha, outcome){

  # only keep necessary characteristic variables we need for now
  characteristics <- which(martha[3,] %in% c("StudyID", "Drug", "No_randomised"))
  martha_characteristics <- martha[, characteristics]
  colnames(martha_characteristics) <- martha[3, characteristics]

  # extract outcome; only keep trials that are not all '*'; also remove the first 3 row headers
  martha_outcome <- as.matrix(martha[,martha[1,] %in% outcome])
  ind <- apply(martha_outcome, 1, function(x) all(x == "*"))
  ind[1:3] <- TRUE
  martha_outcome <- as.matrix(martha_outcome[!ind, ])
  martha_characteristics <- martha_characteristics[!ind,]

  # change No_randomised to numeric
  martha_characteristics["No_randomised"][martha_characteristics["No_randomised"] == "*"] <- 0   #some values are missing - denoted with *
  martha_characteristics["No_randomised"] <- sapply(martha_characteristics[,"No_randomised"], as.numeric)

  # change the * to 0 and add the same columns up; change the values to numeric
  martha_outcome[martha_outcome == "*"] <- 0
  martha_outcome <- apply(martha_outcome, 2, function(x){x[grepl('S ', x)] <- NA; x}) #delete some weird values (i.e. "S 0" for Suicidal Ideation
  martha_outcome <- apply(martha_outcome, 2, as.numeric)
  martha_outcome <- apply(martha_outcome, 1, sum, na.rm = TRUE)

  # combine the characteristics and outcome
  martha_combined <- cbind(martha_characteristics, martha_outcome)
  print(sum(martha_combined$martha_outcome, na.rm = TRUE)) #check this number with what we have calculated previously

  return(martha_combined)
}

# function to remove one arm trial; think the netmeta automatically gets rid of one arm
remove.onearm <- function(data, warn=TRUE) {
  # Remove 1-arm studies
  sel <- as.logical(sapply(data[,'StudyID'],
                           function(study) {sum(data[,'StudyID'] == study) > 1
                           }))

  if (warn && !all(sel)) {
    warning(paste("Removing", sum(!sel), "one-arm studies:",
                  paste(data[!sel,'StudyID'], collapse=", ")))
  }
  data[sel, , drop=FALSE]
}


############################# functions for Analysis#################################

# function to do meta analysis after grouping treatments into one
do_meta_analysis <- function(data){

  data$Drug <- ifelse(data$Drug == "placebo", "placebo", "treatment")
  # combine identical treatments together
  meta <- aggregate(data[,c("No_randomised", "martha_outcome")], list(Drug = data[,"Drug"], StudyID = data[,"StudyID"]), sum)

  p1 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = meta, sm = "OR")
  model1 = metabin(event.e = martha_outcome2, n.e = No_randomised2, event.c = martha_outcome1, n.c = No_randomised1,
                   data = p1, studlab = StudyID, sm = "OR")
  return(model1)
}

# function to perform MH-NMA
do_MH_NMA <- function(data, method = "MH", random.effects = TRUE){
  # combine identical treatments together
  data <- aggregate(data[,c("No_randomised", "martha_outcome")], list(Drug = data[,"Drug"], StudyID = data[,"StudyID"]), sum)

  p2 = pairwise(treat = Drug, event = martha_outcome, n = No_randomised, studlab = StudyID, data = data, sm = "OR")

  if(method == "Inverse"){
    model2 <- netmetabin(p2, ref = "placebo", method = method, comb.random = random.effects, allstudies = TRUE)
  }else{
    model2 <- netmetabin(p2, ref = "placebo", method = method)
  }

  return(model2)
}


################################## start of analysis
settings.meta(digits = 2, digits.pval = 2)

#############################################
#### suicidal ideation
suicidal <- split_data(original, "SUICIDAL IDEATION/BEHAVIOUR or SELF HARM") #total number of events is 365
model1 <- do_meta_analysis(suicidal)
model2 <- do_MH_NMA(suicidal)
model3 <- do_MH_NMA(suicidal, method = "Inverse")
model4 <- do_MH_NMA(suicidal, method = "Inverse", random.effects = TRUE)

netgraph(model2, seq = "optimal", col = "black", plastic = FALSE, points = TRUE, pch = 21, cex.points = 3,
         col.points = "black", bg.points = "gray", thickness = "se.fixed", multiarm = FALSE, number.of.studies = TRUE)
summary(model1)
funnel(model1, pch = 16,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("green", "yellow", "pink"))
legend(2, 0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("green", "yellow", "pink"), bty = "n")

forest(model2, sortvar = model2$TE.fixed[,which(colnames(model2$TE.fixed)=="placebo")])
forest(model3, sortvar = model3$TE.fixed[,which(colnames(model3$TE.fixed)=="placebo")])

netleague(model3)

#############################################
# Nausea
nausea <- split_data(original, "NAUSEA") 
model5 <- do_meta_analysis(nausea)
model6 <- do_MH_NMA(nausea, method = "Inverse", random.effects = TRUE)

netgraph(model6, seq = "optimal", col = "black", plastic = FALSE, points = TRUE, pch = 21, cex.points = 3,
         col.points = "black", bg.points = "gray", thickness = "se.fixed", multiarm = FALSE, number.of.studies = TRUE)
summary(model5)
funnel(model5, pch = 16,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("green", "yellow", "pink"))
legend(8, 0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("green", "yellow", "pink"), bty = "n")
forest(model6, sortvar = model6$TE.random[,which(colnames(model6$TE.random)=="placebo")])

############################################
# Agitation
agitation <- split_data(original, "AGITATION") 
model7 <- do_meta_analysis(agitation)
model8 <- do_MH_NMA(agitation, method = "Inverse", random.effects = TRUE)

netgraph(model8, seq = "optimal", col = "black", plastic = FALSE, points = TRUE, pch = 21, cex.points = 3,
        col.points = "black", bg.points = "gray", thickness = "se.fixed", multiarm = FALSE, number.of.studies = TRUE)
summary(model7)

funnel(model7, pch = 16,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("green", "yellow", "pink"))
legend(2, 0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("green", "yellow", "pink"), bty = "n")

forest(model8, sortvar = model8$TE.random[,which(colnames(model8$TE.random)=="placebo")])


###########################################3
# Tremor

tremor <- split_data(original, "TREMOR") 
model9 <- do_meta_analysis(tremor)
model10 <- do_MH_NMA(tremor, method = "Inverse", random.effects = TRUE)
netgraph(model10, seq = "optimal", col = "black", plastic = FALSE, points = TRUE, pch = 21, cex.points = 3,
         col.points = "black", bg.points = "gray", thickness = "se.fixed", multiarm = FALSE, number.of.studies = TRUE)
summary(model9)
funnel(model9, pch = 16,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("green", "yellow", "pink"))
legend(10, 0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("green", "yellow", "pink"), bty = "n")
forest(model10, sortvar = model10$TE.random[,which(colnames(model10$TE.random)=="placebo")])
