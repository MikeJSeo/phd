setwd("~/GitHub/phd/ra/Result")
load("Approach1.result.RData")
load("Approach2a.result.RData")
load("Approach2b.result.RData")
load("Approach2c.result.RData")
load("Approach3a25.result.RData")
load("Approach3a50.result.RData")
load("Approach3b25.result.RData")
load("Approach3b50.result.RData")

library(ggplot2)

# length of prediction/observation vectors for different treatments
BSRBR.t1.length <- length(Approach1.result$prediction_BSRBR_internal$y1)
BSRBR.t2.length <- length(Approach1.result$prediction_BSRBR_internal$y2)
BSRBR.t3.length <- length(Approach1.result$prediction_BSRBR_internal$y3)
BSRBR.treat <- Approach1.result$prediction_BSRBR_internal$treat

SCQM.t1.length <- length(Approach1.result$prediction_SCQM_internal$y1)
SCQM.t2.length <- length(Approach1.result$prediction_SCQM_internal$y2)
SCQM.t3.length <- length(Approach1.result$prediction_SCQM_internal$y3)
SCQM.treat <- Approach1.result$prediction_SCQM_internal$treat

###BSRBR, calibration plot1, internal validation

pred <- c(Approach1.result$prediction_BSRBR_internal$pred1, Approach2a.result$prediction_BSRBR_internal$pred1,
          Approach2b.result$prediction_BSRBR_internal$pred1, Approach2c.result$prediction_BSRBR_internal$pred1,
          Approach3a25.result$prediction_BSRBR_internal$pred1, Approach3a50.result$prediction_BSRBR_internal$pred1, 
          Approach3b25.result$prediction_BSRBR_internal$pred1, Approach3b50.result$prediction_BSRBR_internal$pred1,
          Approach1.result$prediction_BSRBR_internal$pred2, Approach2a.result$prediction_BSRBR_internal$pred2,
          Approach2b.result$prediction_BSRBR_internal$pred2, Approach2c.result$prediction_BSRBR_internal$pred2,
          Approach3a25.result$prediction_BSRBR_internal$pred2, Approach3a50.result$prediction_BSRBR_internal$pred2, 
          Approach3b25.result$prediction_BSRBR_internal$pred2, Approach3b50.result$prediction_BSRBR_internal$pred2,
          Approach1.result$prediction_BSRBR_internal$pred3, Approach2a.result$prediction_BSRBR_internal$pred3,
          Approach2b.result$prediction_BSRBR_internal$pred3, Approach2c.result$prediction_BSRBR_internal$pred3,
          Approach3a25.result$prediction_BSRBR_internal$pred3, Approach3a50.result$prediction_BSRBR_internal$pred3, 
          Approach3b25.result$prediction_BSRBR_internal$pred3, Approach3b50.result$prediction_BSRBR_internal$pred3
          )
obs <- c(Approach1.result$prediction_BSRBR_internal$y1, Approach2a.result$prediction_BSRBR_internal$y1,
         Approach2b.result$prediction_BSRBR_internal$y1, Approach2c.result$prediction_BSRBR_internal$y1,
         Approach3a25.result$prediction_BSRBR_internal$y1, Approach3a50.result$prediction_BSRBR_internal$y1, 
         Approach3b25.result$prediction_BSRBR_internal$y1, Approach3b50.result$prediction_BSRBR_internal$y1,
         Approach1.result$prediction_BSRBR_internal$y2, Approach2a.result$prediction_BSRBR_internal$y2,
         Approach2b.result$prediction_BSRBR_internal$y2, Approach2c.result$prediction_BSRBR_internal$y2,
         Approach3a25.result$prediction_BSRBR_internal$y2, Approach3a50.result$prediction_BSRBR_internal$y2, 
         Approach3b25.result$prediction_BSRBR_internal$y2, Approach3b50.result$prediction_BSRBR_internal$y2,
         Approach1.result$prediction_BSRBR_internal$y3, Approach2a.result$prediction_BSRBR_internal$y3,
         Approach2b.result$prediction_BSRBR_internal$y3, Approach2c.result$prediction_BSRBR_internal$y3,
         Approach3a25.result$prediction_BSRBR_internal$y3, Approach3a50.result$prediction_BSRBR_internal$y3, 
         Approach3b25.result$prediction_BSRBR_internal$y3, Approach3b50.result$prediction_BSRBR_internal$y3
         )

intercept <- c(rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[1]), each = BSRBR.t1.length),
               rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[1]), each = BSRBR.t2.length),
               rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[1],
               Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[1]), each = BSRBR.t3.length)
               )
  
slope <- c(rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[2],
           Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[2]), each = BSRBR.t1.length),
           rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[3],
           Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[3]), each = BSRBR.t2.length),
           rep(c(Approach1.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach2a.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach2b.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach2c.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach3a25.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach3a50.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach3b25.result$calibration_BSRBR_internal$slope1$coefficients[4],
           Approach3b50.result$calibration_BSRBR_internal$slope1$coefficients[4]), each = BSRBR.t3.length)
           )

approach_id1 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                     "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t1.length)
approach_id2 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t2.length)
approach_id3 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t3.length)
approach_id <- c(approach_id1, approach_id2, approach_id3)

treatment <- c(rep("Placebo + DMARDs", each = BSRBR.t1.length*8),
                  rep("RTX + DMARDs", each = BSRBR.t2.length*8),
                  rep("TCZ + DMARDs", each = BSRBR.t3.length*8))

data <- cbind(pred, obs, intercept, slope)
data <- as.data.frame(data)
data$approach_id <- approach_id
data$treatment <- as.factor(treatment)
data <- data[complete.cases(data),]

ggplot(data, aes(x = pred, y = obs)) + 
  geom_jitter(position = position_jitter(width = 0.3), aes(color = treatment, shape = treatment), size = 1, alpha = 1) + 
  geom_abline(aes(intercept = intercept, slope = slope, group = "approach_id", color = treatment, lty = treatment), size = 1) +
  facet_wrap(~approach_id, ncol = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  labs(x = "predicted outcome",
       y = "observed outcome") +
  theme(legend.position="top") + 
  xlim(-1, 9) + ylim(-1, 9)


#BSRBR, calibration plot1, internal-external validation
          
pred <- c(Approach1.result$prediction_BSRBR_external$pred1, Approach2a.result$prediction_BSRBR_external$pred1,
          Approach2b.result$prediction_BSRBR_external$pred1, Approach2c.result$prediction_BSRBR_external$pred1,
          Approach3a25.result$prediction_BSRBR_external$pred1, Approach3a50.result$prediction_BSRBR_external$pred1, 
          Approach3b25.result$prediction_BSRBR_external$pred1, Approach3b50.result$prediction_BSRBR_external$pred1,
          Approach1.result$prediction_BSRBR_external$pred2, Approach2a.result$prediction_BSRBR_external$pred2,
          Approach2b.result$prediction_BSRBR_external$pred2, Approach2c.result$prediction_BSRBR_external$pred2,
          Approach3a25.result$prediction_BSRBR_external$pred2, Approach3a50.result$prediction_BSRBR_external$pred2, 
          Approach3b25.result$prediction_BSRBR_external$pred2, Approach3b50.result$prediction_BSRBR_external$pred2,
          Approach1.result$prediction_BSRBR_external$pred3, Approach2a.result$prediction_BSRBR_external$pred3,
          Approach2b.result$prediction_BSRBR_external$pred3, Approach2c.result$prediction_BSRBR_external$pred3,
          Approach3a25.result$prediction_BSRBR_external$pred3, Approach3a50.result$prediction_BSRBR_external$pred3, 
          Approach3b25.result$prediction_BSRBR_external$pred3, Approach3b50.result$prediction_BSRBR_external$pred3
)
obs <- c(Approach1.result$prediction_BSRBR_external$y1, Approach2a.result$prediction_BSRBR_external$y1,
         Approach2b.result$prediction_BSRBR_external$y1, Approach2c.result$prediction_BSRBR_external$y1,
         Approach3a25.result$prediction_BSRBR_external$y1, Approach3a50.result$prediction_BSRBR_external$y1, 
         Approach3b25.result$prediction_BSRBR_external$y1, Approach3b50.result$prediction_BSRBR_external$y1,
         Approach1.result$prediction_BSRBR_external$y2, Approach2a.result$prediction_BSRBR_external$y2,
         Approach2b.result$prediction_BSRBR_external$y2, Approach2c.result$prediction_BSRBR_external$y2,
         Approach3a25.result$prediction_BSRBR_external$y2, Approach3a50.result$prediction_BSRBR_external$y2, 
         Approach3b25.result$prediction_BSRBR_external$y2, Approach3b50.result$prediction_BSRBR_external$y2,
         Approach1.result$prediction_BSRBR_external$y3, Approach2a.result$prediction_BSRBR_external$y3,
         Approach2b.result$prediction_BSRBR_external$y3, Approach2c.result$prediction_BSRBR_external$y3,
         Approach3a25.result$prediction_BSRBR_external$y3, Approach3a50.result$prediction_BSRBR_external$y3, 
         Approach3b25.result$prediction_BSRBR_external$y3, Approach3b50.result$prediction_BSRBR_external$y3
)

intercept <- c(rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2a.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2b.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2c.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[1]), each = BSRBR.t1.length),
               rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2a.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2b.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2c.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[1]), each = BSRBR.t2.length),
               rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2a.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2b.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach2c.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[1]), each = BSRBR.t3.length)
)

slope <- c(rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach2a.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach2b.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach2c.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[2],
                 Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[2]), each = BSRBR.t1.length),
           rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach2a.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach2b.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach2c.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[3],
                 Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[3]), each = BSRBR.t2.length),
           rep(c(Approach1.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach2a.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach2b.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach2c.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach3a25.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach3a50.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach3b25.result$calibration_BSRBR_external$slope1$coefficients[4],
                 Approach3b50.result$calibration_BSRBR_external$slope1$coefficients[4]), each = BSRBR.t3.length)
)

approach_id1 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t1.length)
approach_id2 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t2.length)
approach_id3 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = BSRBR.t3.length)
approach_id <- c(approach_id1, approach_id2, approach_id3)

treatment <- c(rep("Placebo + DMARDs", each = BSRBR.t1.length*8),
               rep("RTX + DMARDs", each = BSRBR.t2.length*8),
               rep("TCZ + DMARDs", each = BSRBR.t3.length*8)
)

data <- cbind(pred, obs, intercept, slope)
data <- as.data.frame(data)
data$approach_id <- approach_id
data$treatment <- as.factor(treatment)
data <- data[complete.cases(data),]

ggplot(data, aes(x = pred, y = obs)) + 
  geom_jitter(position = position_jitter(width = 0.3), aes(color = treatment, shape = treatment), size = 1, alpha = 1) + 
  geom_abline(aes(intercept = intercept, slope = slope, group = "approach_id", color = treatment, lty = treatment), size = 1) +
  facet_wrap(~approach_id, ncol = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  labs(x = "predicted outcome",
       y = "observed outcome") +
  theme(legend.position="top") +
  xlim(-1, 9) + ylim(-1, 9)


################SCQM

###SCQM, calibration plot1, internal validation

pred <- c(Approach1.result$prediction_SCQM_internal$pred1, Approach2a.result$prediction_SCQM_internal$pred1,
          Approach2b.result$prediction_SCQM_internal$pred1, Approach2c.result$prediction_SCQM_internal$pred1,
          Approach3a25.result$prediction_SCQM_internal$pred1, Approach3a50.result$prediction_SCQM_internal$pred1, 
          Approach3b25.result$prediction_SCQM_internal$pred1, Approach3b50.result$prediction_SCQM_internal$pred1,
          Approach1.result$prediction_SCQM_internal$pred2, Approach2a.result$prediction_SCQM_internal$pred2,
          Approach2b.result$prediction_SCQM_internal$pred2, Approach2c.result$prediction_SCQM_internal$pred2,
          Approach3a25.result$prediction_SCQM_internal$pred2, Approach3a50.result$prediction_SCQM_internal$pred2, 
          Approach3b25.result$prediction_SCQM_internal$pred2, Approach3b50.result$prediction_SCQM_internal$pred2,
          Approach1.result$prediction_SCQM_internal$pred3, Approach2a.result$prediction_SCQM_internal$pred3,
          Approach2b.result$prediction_SCQM_internal$pred3, Approach2c.result$prediction_SCQM_internal$pred3,
          Approach3a25.result$prediction_SCQM_internal$pred3, Approach3a50.result$prediction_SCQM_internal$pred3, 
          Approach3b25.result$prediction_SCQM_internal$pred3, Approach3b50.result$prediction_SCQM_internal$pred3
)
obs <- c(Approach1.result$prediction_SCQM_internal$y1, Approach2a.result$prediction_SCQM_internal$y1,
         Approach2b.result$prediction_SCQM_internal$y1, Approach2c.result$prediction_SCQM_internal$y1,
         Approach3a25.result$prediction_SCQM_internal$y1, Approach3a50.result$prediction_SCQM_internal$y1, 
         Approach3b25.result$prediction_SCQM_internal$y1, Approach3b50.result$prediction_SCQM_internal$y1,
         Approach1.result$prediction_SCQM_internal$y2, Approach2a.result$prediction_SCQM_internal$y2,
         Approach2b.result$prediction_SCQM_internal$y2, Approach2c.result$prediction_SCQM_internal$y2,
         Approach3a25.result$prediction_SCQM_internal$y2, Approach3a50.result$prediction_SCQM_internal$y2, 
         Approach3b25.result$prediction_SCQM_internal$y2, Approach3b50.result$prediction_SCQM_internal$y2,
         Approach1.result$prediction_SCQM_internal$y3, Approach2a.result$prediction_SCQM_internal$y3,
         Approach2b.result$prediction_SCQM_internal$y3, Approach2c.result$prediction_SCQM_internal$y3,
         Approach3a25.result$prediction_SCQM_internal$y3, Approach3a50.result$prediction_SCQM_internal$y3, 
         Approach3b25.result$prediction_SCQM_internal$y3, Approach3b50.result$prediction_SCQM_internal$y3
)

intercept <- c(rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[1]), each = SCQM.t1.length),
               rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[1]), each = SCQM.t2.length),
               rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[1]), each = SCQM.t3.length)
)

slope <- c(rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach2a.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach2b.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach2c.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[2],
                 Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[2]), each = SCQM.t1.length),
           rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach2a.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach2b.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach2c.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[3],
                 Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[3]), each = SCQM.t2.length),
           rep(c(Approach1.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach2a.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach2b.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach2c.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach3a25.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach3a50.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach3b25.result$calibration_SCQM_internal$slope1$coefficients[4],
                 Approach3b50.result$calibration_SCQM_internal$slope1$coefficients[4]), each = SCQM.t3.length)
)

approach_id1 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t1.length)
approach_id2 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t2.length)
approach_id3 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t3.length)
approach_id <- c(approach_id1, approach_id2, approach_id3)

treatment <- c(rep("Placebo + DMARDs", each = SCQM.t1.length*8),
               rep("RTX + DMARDs", each = SCQM.t2.length*8),
               rep("TCZ + DMARDs", each = SCQM.t3.length*8))

data <- cbind(pred, obs, intercept, slope)
data <- as.data.frame(data)
data$approach_id <- approach_id
data$treatment <- as.factor(treatment)
data <- data[complete.cases(data),]

ggplot(data, aes(x = pred, y = obs)) + 
  geom_jitter(position = position_jitter(width = 0.3), aes(color = treatment, shape = treatment), size = 1, alpha = 1) + 
  geom_abline(aes(intercept = intercept, slope = slope, group = "approach_id", color = treatment, lty = treatment), size = 1) +
  facet_wrap(~approach_id, ncol = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  labs(x = "predicted outcome",
       y = "observed outcome") +
  theme(legend.position="top") + 
  xlim(-1, 9) + ylim(-1, 9)


#SCQM, calibration plot1, internal-external validation

pred <- c(Approach1.result$prediction_SCQM_external$pred1, Approach2a.result$prediction_SCQM_external$pred1,
          Approach2b.result$prediction_SCQM_external$pred1, Approach2c.result$prediction_SCQM_external$pred1,
          Approach3a25.result$prediction_SCQM_external$pred1, Approach3a50.result$prediction_SCQM_external$pred1, 
          Approach3b25.result$prediction_SCQM_external$pred1, Approach3b50.result$prediction_SCQM_external$pred1,
          Approach1.result$prediction_SCQM_external$pred2, Approach2a.result$prediction_SCQM_external$pred2,
          Approach2b.result$prediction_SCQM_external$pred2, Approach2c.result$prediction_SCQM_external$pred2,
          Approach3a25.result$prediction_SCQM_external$pred2, Approach3a50.result$prediction_SCQM_external$pred2, 
          Approach3b25.result$prediction_SCQM_external$pred2, Approach3b50.result$prediction_SCQM_external$pred2,
          Approach1.result$prediction_SCQM_external$pred3, Approach2a.result$prediction_SCQM_external$pred3,
          Approach2b.result$prediction_SCQM_external$pred3, Approach2c.result$prediction_SCQM_external$pred3,
          Approach3a25.result$prediction_SCQM_external$pred3, Approach3a50.result$prediction_SCQM_external$pred3, 
          Approach3b25.result$prediction_SCQM_external$pred3, Approach3b50.result$prediction_SCQM_external$pred3
)
obs <- c(Approach1.result$prediction_SCQM_external$y1, Approach2a.result$prediction_SCQM_external$y1,
         Approach2b.result$prediction_SCQM_external$y1, Approach2c.result$prediction_SCQM_external$y1,
         Approach3a25.result$prediction_SCQM_external$y1, Approach3a50.result$prediction_SCQM_external$y1, 
         Approach3b25.result$prediction_SCQM_external$y1, Approach3b50.result$prediction_SCQM_external$y1,
         Approach1.result$prediction_SCQM_external$y2, Approach2a.result$prediction_SCQM_external$y2,
         Approach2b.result$prediction_SCQM_external$y2, Approach2c.result$prediction_SCQM_external$y2,
         Approach3a25.result$prediction_SCQM_external$y2, Approach3a50.result$prediction_SCQM_external$y2, 
         Approach3b25.result$prediction_SCQM_external$y2, Approach3b50.result$prediction_SCQM_external$y2,
         Approach1.result$prediction_SCQM_external$y3, Approach2a.result$prediction_SCQM_external$y3,
         Approach2b.result$prediction_SCQM_external$y3, Approach2c.result$prediction_SCQM_external$y3,
         Approach3a25.result$prediction_SCQM_external$y3, Approach3a50.result$prediction_SCQM_external$y3, 
         Approach3b25.result$prediction_SCQM_external$y3, Approach3b50.result$prediction_SCQM_external$y3
)

intercept <- c(rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_external$slope1$coefficients[1]), each = SCQM.t1.length),
               rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_external$slope1$coefficients[1]), each = SCQM.t2.length),
               rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2a.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2b.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach2c.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3a50.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b25.result$calibration_SCQM_external$slope1$coefficients[1],
                     Approach3b50.result$calibration_SCQM_external$slope1$coefficients[1]), each = SCQM.t3.length)
)

slope <- c(rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach2a.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach2b.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach2c.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach3a25.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach3a50.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach3b25.result$calibration_SCQM_external$slope1$coefficients[2],
                 Approach3b50.result$calibration_SCQM_external$slope1$coefficients[2]), each = SCQM.t1.length),
           rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach2a.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach2b.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach2c.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach3a25.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach3a50.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach3b25.result$calibration_SCQM_external$slope1$coefficients[3],
                 Approach3b50.result$calibration_SCQM_external$slope1$coefficients[3]), each = SCQM.t2.length),
           rep(c(Approach1.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach2a.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach2b.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach2c.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach3a25.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach3a50.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach3b25.result$calibration_SCQM_external$slope1$coefficients[4],
                 Approach3b50.result$calibration_SCQM_external$slope1$coefficients[4]), each = SCQM.t3.length)
)

approach_id1 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t1.length)
approach_id2 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t2.length)
approach_id3 <- rep(c("Approach I", "Approach IIa", "Approach IIb", "Approach IIc", "Approach IIIa w = 0.25",
                      "Approach IIIa w = 0.5", "Approach IIIb w = 0.25", "Approach IIIb w = 0.5"), each = SCQM.t3.length)
approach_id <- c(approach_id1, approach_id2, approach_id3)

treatment <- c(rep("Placebo + DMARDs", each = SCQM.t1.length*8),
               rep("RTX + DMARDs", each = SCQM.t2.length*8),
               rep("TCZ + DMARDs", each = SCQM.t3.length*8)
)

data <- cbind(pred, obs, intercept, slope)
data <- as.data.frame(data)
data$approach_id <- approach_id
data$treatment <- as.factor(treatment)
data <- data[complete.cases(data),]

ggplot(data, aes(x = pred, y = obs)) + 
  geom_jitter(position = position_jitter(width = 0.3), aes(color = treatment, shape = treatment), size = 1, alpha = 1) + 
  geom_abline(aes(intercept = intercept, slope = slope, group = "approach_id", color = treatment, lty = treatment), size = 1) +
  facet_wrap(~approach_id, ncol = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  labs(x = "predicted outcome",
       y = "observed outcome")+
  theme(legend.position="top") + 
  xlim(-1,9) + ylim(-1,9)
