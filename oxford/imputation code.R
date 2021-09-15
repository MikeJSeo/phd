devtools::install_github("MikeJSeo/bipd") #please install this github - I put some functions in here for convenience
library(bipd)

setwd("C:/Users/ms19g661/Desktop") #please change directory
data <- read.csv("data_for_new_imputation.csv")

covariates <- c("AGE", "SEX", "TREATMENT_GROUP", "HAMD_3", "HAMD_4", "HAMD_6", "HAMD_10", "HAMD_11", "HAMD_13", "HAMD_17", "HAMD_BASELINE",
                "AE_CATEGORY_ABDOMINAL.PAIN.DISCOMFORT", "AE_CATEGORY_FATIGUE", "AE_CATEGORY_HEADACHE", "AE_CATEGORY_NAUSEA",
                "AE_CATEGORY_SEDATION.SOMNOLENCE", "AE_CATEGORY_SEXUAL.DYSFUNCTION",
                "HAMD_WEEK_1", "HAMD_WEEK_2", "HAMD_WEEK_3", "HAMD_WEEK_4", "HAMD_WEEK_5", "HAMD_WEEK_6", "HAMD_WEEK_7", "HAMD_WEEK_9", "HAMD_WEEK_10")
y_covariate <- "HAMD_WEEK_8"

missingP <- findMissingPattern(data, covariates, studyname = "STUDYID")

# There are studies with too few observations: 29060/073 has 9
table(data$STUDYID)

# Only


data[covariates]

#shows missing percentage for each variable in each study
missingP$missingpercent[]

missingP$missingpercent[missingP$without_sys_covariates]