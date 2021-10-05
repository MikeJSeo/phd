# Here we demonstrate a simple example of predicting systematically missing IPD with the three different approaches mentioned.


# First let's simulate some fake IPD with systematically missing studies.

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





# For whatever reason this wrapper imputation function doesn't work, he/she can dig in to see which part might have gone wrong. 
# Also, if the user doesn't like the selected method or prediction matrix, he/she can simply put their own.