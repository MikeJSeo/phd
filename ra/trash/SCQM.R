library(tidyverse)

setwd("C:/Users/ms19g661/Desktop/ra_data/SCQM")

#adverseevent <- read.csv("adverseevent.csv", header = TRUE)
#doctor <- read.csv("doctor.csv", header = TRUE)
#hospital <- read.csv("hospital.csv", header = TRUE)
medication <- read.csv("medication.csv", header = TRUE)
medicationadjustment <- read.csv("medicationadjustment.csv", header = TRUE)
patient <- read.csv("patient.csv", header = TRUE)
#rauxrayscore <- read.csv("rauxrayscore.csv", header = TRUE)
#rauxrayscore_new_prep <- read.csv("rauxrayscore_new_prep.csv", header = TRUE)
scqm_medication <- read.csv("scqm_medication.csv", header = TRUE)
visit <- read.csv("visit.csv", header = TRUE)


med <- as_tibble(scqm_medication)
med2 <- med %>%
  mutate(patient.id = scqm_medication.patient_id,
         drug = scqm_medication.drug) %>%
  select(patient.id, drug)
  

patient <- as_tibble(patient)
patient2 <- patient %>% select(patient.id, patient.gender, patient.date_of_birth, patient.date_first_symptoms, patient.date_diagnosis)

c(visit.date, visit.weight_kg, visit.height_cm) 