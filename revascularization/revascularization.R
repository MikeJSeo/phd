
setwd("C:/Users/mike/Desktop")
source("revas.R")

#devtools::install_github("MikeJSeo/bnma")
library(bnma) #for analysis


#######################################MACE

# Nodesplitting model
set.seed(1)
network_n1 <- with(MACE_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "COI"), dic = FALSE))
result_n1 <- nodesplit.network.run(network_n1, n.run = 100000)
network_n2 <- with(MACE_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "MV-Staged"), dic = FALSE))
result_n2 <- nodesplit.network.run(network_n2, n.run = 100000)
network_n3 <- with(MACE_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("COI", "MV-Staged"), dic = FALSE))
result_n3 <- nodesplit.network.run(network_n3, n.run = 100000)

summary(result_n1)$p_value # MV-Index and COI
summary(result_n2)$p_value # MV-Index and MV-Staged
summary(result_n3)$p_value # COI and MV-Staged


# Network meta-analysis
set.seed(1)
network <- with(MACE_data, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", Treat.order =  c("COI", "MV-Index", "MV-Staged"), covariate = covariate, covariate.model = "independent", rank.preference = "lower"))
result <- network.run(network, n.run = 100000)
summary(result)


exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"]) # MV-Index vs COI
exp(summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV-Staged vs COI
exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"] - summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV Index vs MV Staged

# league table
exp(relative.effects.table(result))

# Covariate plot
network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Index")
network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Staged")

mean(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))
sd(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))



# Forrest plot
network.forest.plot(result, label.margin = 10, title = "A. MACE")

# Rankogram
network.rankogram(result, title = "Rankogram for MACE")


########################################## all cause mortality

# Nodesplitting model
set.seed(1)
network_n1 <- with(all_cause_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "COI"), dic = FALSE))
result_n1 <- nodesplit.network.run(network_n1, n.run = 100000)
network_n2 <- with(all_cause_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "MV-Staged"), dic = FALSE))
result_n2 <- nodesplit.network.run(network_n2, n.run = 100000)
network_n3 <- with(all_cause_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("COI", "MV-Staged"), dic = FALSE))
result_n3 <- nodesplit.network.run(network_n3, n.run = 100000)

summary(result_n1)$p_value # MV-Index and COI
summary(result_n2)$p_value # MV-Index and MV-Staged
summary(result_n3)$p_value # COI and MV-Staged


# Network meta analysis
set.seed(1)
network <- with(all_cause_data, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", Treat.order =  c("COI", "MV-Index", "MV-Staged"), covariate = covariate, covariate.model = "independent", rank.preference = "lower"))
result <- network.run(network, n.run = 100000)
summary(result)

exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"]) # MV-Index vs COI
exp(summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV-Staged vs COI
exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"] - summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV Index vs MV Staged


#league table
exp(relative.effects.table(result))

# covariate plot
network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Index")

mean(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"])) # MV Index vs MV Staged
sd(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))


# Forrest plot
network.forest.plot(result, label.margin = 10, title = "B. All Cause Mortality")

## rankogram
network.rankogram(result, title = "Rankogram for All Cause Mortality")



########################################################### Cardiovascular mortality
set.seed(1)
network_n1 <- with(cardio_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "COI"), dic = FALSE))
result_n1 <- nodesplit.network.run(network_n1, n.run = 100000)
network_n2 <- with(cardio_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "MV-Staged"), dic = FALSE))
result_n2 <- nodesplit.network.run(network_n2, n.run = 100000)
network_n3 <- with(cardio_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("COI", "MV-Staged"), dic = FALSE))
result_n3 <- nodesplit.network.run(network_n3, n.run = 100000)

summary(result_n1)$p_value # MV-Index and COI
summary(result_n2)$p_value # MV-Index and MV-Staged
summary(result_n3)$p_value # COI and MV-Staged

# Network meta-analysis
set.seed(1)
network <- with(cardio_data, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", Treat.order =  c("COI", "MV-Index", "MV-Staged"), covariate = covariate, covariate.model = "independent", rank.preference = "lower"))
result <- network.run(network, n.run = 100000)
summary(result)

exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"]) # MV-Index vs COI
exp(summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV-Staged vs COI
exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"] - summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV Index vs MV Staged

#league table
exp(relative.effects.table(result))

network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Index")

mean(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"])) # MV Index vs MV Staged
sd(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))

network.forest.plot(result, label.margin = 10, title = "C. Cardiovascular Mortality")
network.rankogram(result, title = "Rankogram for Cardiovascular Mortality")



#############################################################MI

set.seed(1)
network_n1 <- with(MI_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "COI"), dic = FALSE))
result_n1 <- nodesplit.network.run(network_n1, n.run = 100000)
network_n2 <- with(MI_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "MV-Staged"), dic = FALSE))
result_n2 <- nodesplit.network.run(network_n2, n.run = 100000)
network_n3 <- with(MI_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("COI", "MV-Staged"), dic = FALSE))
result_n3 <- nodesplit.network.run(network_n3, n.run = 100000)

summary(result_n1)$p_value # MV-Index and COI
summary(result_n2)$p_value # MV-Index and MV-Staged
summary(result_n3)$p_value # COI and MV-Staged


set.seed(1)
network <- with(MI_data, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", Treat.order =  c("COI", "MV-Index", "MV-Staged"), covariate = covariate, covariate.model = "independent", rank.preference = "lower"))
result <- network.run(network, n.run = 100000)
summary(result)

exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"]) # MV-Index vs COI
exp(summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV-Staged vs COI
exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"] - summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV Index vs MV Staged

exp(relative.effects.table(result))
network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Index")

mean(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"])) # MV Index vs MV Staged
sd(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))

network.forest.plot(result, label.margin = 10, title = "D. MI")
network.rankogram(result, title = "Rankogram for MI")


################################revascularization

set.seed(1)
network_n1 <- with(revas_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "COI"), dic = FALSE))
result_n1 <- nodesplit.network.run(network_n1, n.run = 100000)
network_n2 <- with(revas_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("MV-Index", "MV-Staged"), dic = FALSE))
result_n2 <- nodesplit.network.run(network_n2, n.run = 100000)
network_n3 <- with(revas_data, nodesplit.network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", pair = c("COI", "MV-Staged"), dic = FALSE))
result_n3 <- nodesplit.network.run(network_n3, n.run = 100000)

summary(result_n1)$p_value # MV-Index and COI
summary(result_n2)$p_value # MV-Index and MV-Staged
summary(result_n3)$p_value # COI and MV-Staged


set.seed(1)
network <- with(revas_data, network.data(Outcomes = Outcomes, Study = Study, Treat = Treat, N = N, response = "binomial", Treat.order =  c("COI", "MV-Index", "MV-Staged"), covariate = covariate, covariate.model = "independent", rank.preference = "lower"))
result <- network.run(network, n.run = 100000)
summary(result)

exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"]) # MV-Index vs COI
exp(summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV-Staged vs COI
exp(summary(result)$summary.samples$statistics["d[2]" , "Mean"] - summary(result)$summary.samples$statistics["d[3]" , "Mean"]) # MV Index vs MV Staged

exp(relative.effects.table(result))
network.covariate.plot(result, base.treatment = "COI", comparison.treatment = "MV-Index")

mean(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"])) # MV Index vs MV Staged
sd(unlist(result$samples[,"beta1[2]"]) - unlist(result$samples[,"beta1[3]"]))

network.forest.plot(result, label.margin = 10, title = "E. Revascularization")
network.rankogram(result, title = "Rankogram for Revascularization")