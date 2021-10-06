# Here we demonstrate a simple example of predicting systematically missing IPD with the three different approaches mentioned.

# Before going to imputation, there might be some preprocessing one might need to do.
# There are couple of instances that won't allow imputing systematically missing data (check if your data applies)
# a) For every study, covariate should have more than two values. For instance, if one study only has measurements 
# for female, then we need to throw away that study or variable
# b) Also, there's a problem when there is no observation to measure interaction effect. 
# For instance, including a study where comorbid anxiety (binary variable) is only observed for treated patients and not
# for untreated patients, would lead to an error.

# Let's simulate some fake IPD with systematically missing studies.

mydata2 <- preprocess.data(mydata, covariates, typeofvar, interaction = TRUE,
                           studyname = "study", treatmentname = "treat", outcomename = "y")
missP <- findMissingPattern(mydata2, covariates, typeofvar,
                            studyname = "study", treatmentname = "treat", outcomename = "y")
meth <- getCorrectMeth(mydata2, missP)
pred <- getCorrectPred(mydata2, missP)


meth[meth == "2l.2stage.norm"] <- "2l.glm.norm"
meth[meth == "2l.2stage.bin"] <- "2l.glm.bin"

meth[meth == "2l.2stage.norm"] <- "2l.jomo"
meth[meth == "2l.2stage.bin"] <- "2l.jomo"


imputationapproach <- ipdma.impute(mydata, covariates = covariates, typeofvar = typeofvar, interaction = TRUE,
                                   studyname = "study", treatmentname = "treat", outcomename = "y", meth = meth)





# For whatever reason this wrapper imputation function doesn't work, one can dig in to see which part might have gone wrong. 
# Also, if one doesn't like the selected method or prediction matrix, one can simply specify a different one.