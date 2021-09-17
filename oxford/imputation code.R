#devtools::install_github("MikeJSeo/bipd") #please install this github - I put some functions in here for convenience
library(bipd)
library(fastDummies) #to create dummy columns for treatment

setwd("C:/Users/ms19g661/Desktop") #please change directory
data <- read.csv("data_for_new_imputation.csv")

covariates <- c("AGE", "SEX", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
           #     "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
          #      "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                "HAMD_WEEK_1", "HAMD_WEEK_2", "HAMD_WEEK_3")#, "HAMD_WEEK_4", "HAMD_WEEK_5", "HAMD_WEEK_6", "HAMD_WEEK_7", "HAMD_WEEK_9", "HAMD_WEEK_10")
#y <- "HAMD_WEEK_8"

# Merge two studies
data$STUDYID[data$STUDYID %in% c("29060/073", "29060/059")] <- "29060/059_073"

# Change treatment to dummy variables
table(data$STUDYID, data$TREATMENT_GROUP)
data <- dummy_cols(data, select_columns = "TREATMENT_GROUP", remove_selected_columns = TRUE)
#covariates <- c(covariates, grep("TREATMENT_GROUP", colnames(data), value = TRUE))

# Define variable type
#typeofvar <- c("continuous", "binary", rep("count", 7), "continuous",
#               rep("binary", 6), rep("continuous", 9), rep("binary", 13))
#typeofvar <- c("continuous", "binary", rep("count", 7), "continuous",
#               rep("binary", 6), rep("continuous", 9))
#typeofvar <- c("continuous", "binary", rep("count", 7), "continuous", rep("continuous", 9))
typeofvar <- c("continuous", "binary", rep("count", 7), "continuous", rep("continuous", 3))

names(typeofvar) <- covariates               
              
#create imputation related objects
newdata <- data[, c("HAMD_WEEK_8", "STUDYID", covariates)]
newdata[, c("STUDYID", covariates[typeofvar == "binary"])] <- lapply(newdata[,c("STUDYID", covariates[typeofvar == "binary"]), drop = FALSE], as.factor)


#convert cluster variable STUDYID to integer
newdata$STUDYID <- as.numeric(newdata$STUDYID)

missingPattern <- findMissingPattern(newdata, covariates, studyname = "STUDYID")
meth <- getCorrectMeth(newdata, missingPattern, outcomename = "HAMD_WEEK_8", studyname = "STUDYID", typeofvar = typeofvar, interaction = FALSE)
#meth["HAMD_WEEK_8"] <- "2l.2stage.norm" 
meth["HAMD_WEEK_8"] <- "2l.jomo" 

pred <- getCorrectPred(newdata, missingPattern, outcomename = "HAMD_WEEK_8", studyname = "STUDYID", interaction = FALSE)

set.seed(1)
imp <- mice(newdata, pred = pred, meth = meth)


# There are studies with too few observations: 29060/073 has 9
table(data$STUDYID)
 
# merge studies

# Shows missing percentage for each variable in each study
View(missingP$missingpercent)

kable_out <- knitr::kable(missingP$missingpercent, "html")#%>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
readr::write_file(kable_out, "kable_out.html")
kable_out2 <- knitr::kable(table(data$STUDYID), "html")
readr::write_file(kable_out2, "number of studies.html")

