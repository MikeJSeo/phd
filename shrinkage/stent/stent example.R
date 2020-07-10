# load packages needed
library(readstata13) #reading stat data file
library(lme4) #for fitting glmm
library(glmnet) #for lasso/ridge

## The github library ("bipd") contains functions for generating sample data, calculating treatment effect,
## and running Bayesian IPD-MA methods.
library(devtools)
devtools::install_github("MikeJSeo/bipd")
library(bipd) 