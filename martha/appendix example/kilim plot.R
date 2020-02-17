library(ggplot2)

# import results from analysis
outcome <- c("NAUSEA", "NAUSEA", "NAUSEA", "NAUSEA", "NAUSEA", "NAUSEA", "NAUSEA", "HEADACHE", "HEADACHE", "HEADACHE", "HEADACHE", "HEADACHE", "HEADACHE", "HEADACHE", "DRY MOUTH", "DRY MOUTH", "DRY MOUTH", "DRY MOUTH", "DRY MOUTH", "DRY MOUTH", "DRY MOUTH", "INSOMNIA", "INSOMNIA", "INSOMNIA", "INSOMNIA", "INSOMNIA", "INSOMNIA", "INSOMNIA", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "SEXUAL DYSFUNCTION", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA", "DIARRHOEA", "SUICIDAL IDEATION", "SUICIDAL IDEATION", "SUICIDAL IDEATION", "SUICIDAL IDEATION", "SUICIDAL IDEATION", "AGGRESSION", "AGGRESSION", "AGGRESSION", "AGGRESSION", "ACCIDENTAL OVERDOSE", "ACCIDENTAL OVERDOSE", "ACCIDENTAL OVERDOSE", "ACCIDENTAL OVERDOSE", "ACCIDENTAL OVERDOSE", "NAUSEA", "HEADACHE", "DRY MOUTH", "INSOMNIA", "SEXUAL DYSFUNCTION", "DIARRHOEA", "SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")
drug <- c("amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "mirtazapine", "reboxetine", "venlafaxine", "vortioxetine", "amitriptyline", "duloxetine", "fluoxetine", "venlafaxine", "vortioxetine", "amitriptyline", "fluoxetine", "reboxetine", "venlafaxine", "duloxetine", "fluoxetine", "reboxetine", "venlafaxine", "vortioxetine", "placebo", "placebo", "placebo", "placebo", "placebo", "placebo", "placebo", "placebo", "placebo")
Zscore <- c(-0.137416881226947, 15.0660739283112, 7.98783510098807, -0.836096836491973, 2.62382088507179, 13.8168401406245, 11.583867267738, -0.431728668338137, 1.76884902224012, 1.26498386483495, -1.04202729782999, 0.978985522801796, 0.999969855147623, 0.660734860721772, 18.612522080122, 9.62527892512888, 3.49430574241265, 4.10913506046947, 10.4592122618402, 9.67147207168257, 0.7733907835475, -1.83032872668648, 7.0128934823859, 7.35703682389831, -0.118633646277675, 10.7286633456809, 7.87160564210759, 0.29389597960155, 2.77532600885408, 5.77803164432315, 7.3273753474793, -0.277908056684703, 2.19078954156341, 6.69834918183775, 2.96774529324648, -1.13849612001688, 4.3710631000871, 5.07817155711118, 0.318330695396653, -3.90087229955386, 0.832841130759115, 1.4707208368339, 0.38766123999469, 0.100696672120537, -1.01232946482651, -2.32522456508816, -0.40411961213333, 0.692704619190701, 0.928098421444019, 1.10378303923714, 1.84562053951767, 0.275691085610779, -0.406941104170722, -0.177639029331149, 1.18640837742748, 1.35525487102772, NA, NA, NA, NA, NA, NA, NA, NA, NA)
event.rate <- c("8.5%", "29%", "16%", "7%", "12.7%", "24.7%", "24.2%", "16%", "18.7%", "18%", "14.4%", "18.2%", "17.9%", "17.6%", "54%", "19.7%", "11.6%", "20.5%", "26.2%", "19%", "9.3%", "4.6%", "13%", "11.8%", "6.6%", "19.7%", "13.1%", "7.3%", "9%", "7.8%", "10.1%", "2.7%", "7.2%", "10.9%", "5.2%", "4.9%", "10.3%", "10.9%", "10.9%", "3.8%", "7.8%", "8.3%", "3.2%", "2%", "0.9%", "1%", "1.6%", "4.5%", "6%", "4.8%", "35.4%", "2.7%", "1.6%", "1.9%", "4.8%", "4.3%", "8.7%", "16.9%", "8.5%", "7%", "3.5%", "7.2%", "1.9%", "3.2%", "2.1%")

#Z score2 is the truncated Zscore with lower bound = -2.5, upper bound = 2.5
Zscore2 <- Zscore
Zscore2[Zscore2 > 2.5] <- 2.5
Zscore2[Zscore2 < -2.5] <- -2.5
  
kilim_data <- data.frame(outcome = outcome, drug = drug, Zscore = Zscore, event.rate = event.rate, Zscore2 = Zscore2)

#set the ordering
treatment.selection <- c("vortioxetine", "venlafaxine", "reboxetine", "mirtazapine", "fluoxetine", "duloxetine", "amitriptyline", "placebo")
outcome.selection <- c("NAUSEA", "HEADACHE", "DRY MOUTH", "INSOMNIA", "SEXUAL DYSFUNCTION","DIARRHOEA", "SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")  

kilim_data$drug <- factor(kilim_data$drug, level = treatment.selection,ordered = TRUE)
kilim_data$outcome <- factor(kilim_data$outcome, level = outcome.selection,ordered = TRUE)

ggplot(kilim_data, aes(outcome, drug)) + geom_tile(aes(fill = round(Zscore2,2)), colour = "white") + 
  geom_text(aes(label= event.rate), size = 6) +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", na.value = "lightskyblue1", 
                       breaks = c(-2.326348, -1.281552, 0, 1.281552, 2.326348), limits = c(-2.5, 2.5),
                       labels = c("p < 0.01", "p = 0.1", "p = 1.00", "p = 0.1", "p < 0.01")) +
  labs(x = "",y = "") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=30,hjust=1,vjust=1.0, size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "left",
        legend.text = element_text(size = 14))