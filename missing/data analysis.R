library(dplyr)

setwd("C:/Users/ms19g661/Desktop")
#setwd("C:/Users/mike/Desktop")
data <- read.csv("data_ICBT.csv")


# pick which treatment to compare
mydata <- data %>% filter(treat.m2 %in% c("TAU", "Guided", "Unguided"))

# merge Guided and Unguided as treatment
mydata <- mydata %>% mutate(treat.m2 = ifelse(treat.m2 %in% c("Guided", "Unguided"), 1, 0)) %>%
            rename(treat = treat.m2)

# Delete 4 studies that have same gender(female) for all participants
mydata <- mydata %>% filter(!study %in% c("Forsell, 2017", "Milgrom, 2016")) %>%
  mutate(gender = na_if(gender, "")) %>%
  mutate(gender = ifelse(gender == "Female", 1, 0))


# visualizing variables
#library(summarytools)
#view(dfSummary(mydata), method = "browser")

# leave studies only with two treatment
mydata2 <- mydata %>% group_by(study) %>% filter(length(unique(treat)) == 2)
mydata <- mydata %>% filter(study %in% unique(mydata2$study))

# pick the covariates of interest
# ethnic, educ, employ, mdd, prevep, ComorbidAnxiety, ComorbidMentalHealth, Comorbidphysicaldisorder, Medication, alcohol
mydata <- mydata %>% select(study, y, baseline, gender, age, relstat, treat, ComorbidAnxiety, ComorbidMentalHealth)

# drop studies that have same gender for all participants




mydata %>% group_by(study) %>% summarize_all(~mean(., na.rm = TRUE))
data %>% group_by(study) %>% summarize_all(~sd(., na.rm = TRUE))

mydata <- data %>% select(study)

data%>% select(treat.m2)

# change to factor
data <- as_tibble(data)
cols <- c("study", "gender", "relstat")
data <- data %>% mutate_at(cols, as.factor)

# count number of NAs
data %>% summarize_all(~sum(is.na(.)))

# use fully observed data
data <- data %>% na.omit() %>%
  mutate(across(c("baseline", "age"), scale))


