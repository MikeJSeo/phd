library("shiny")
library("ggplot2")

load("final.rda")

ui <- shinyUI(fluidPage(
  shinyjs::inlineCSS(list(body = "color:DarkBlue")),
  # Application title
  titlePanel(h1("The Kilim plot: a tool for visualizing network meta-analysis results for multiple outcomes")),
  
  tabPanel("Heatplot", plotOutput("plot1")),
  
  hr(),
  h3("Clinically important values (in risk difference)"),
  
  fluidRow(
    column(3,
           sliderInput("NAUSEA", "NAUSEA:", min = 0, max = 0.1, value = 0, step = 0.01),
           sliderInput("HEADACHE", "HEADACHE:", min = 0, max = 0.1, value = 0, step = 0.01),
           sliderInput("DRY_MOUTH", "DRY MOUTH:", min = 0, max = 0.1, value = 0, step = 0.01)
      ),
      column(4, offset = 1,
            sliderInput("INSOMNIA", "INSOMNIA:", min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("SEXUAL_DYSFUNCTION", "SEXUAL DYSFUNCTION:", min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("DIARRHOEA", "DIARRHOEA:", min = 0, max = 0.1, value = 0, step = 0.01)
      ),
      column(4,
            sliderInput("SUICIDAL_IDEATION", "SUICIDAL IDEATION:", min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("AGGRESSION", "AGGRESSION:", min = 0, max = 0.1, value = 0, step = 0.01),
            sliderInput("ACCIDENTAL_OVERDOSE", "ACCIDENTAL OVERDOSE:", min = 0, max = 0.1, value = 0, step = 0.01)
      )
    ),
  
  hr(),
  h3("Absolute event rate for the reference intervention (external information; if none, leave it as 0)"),
  
  fluidRow(
    column(3,
           sliderInput("NAUSEA_e", "NAUSEA:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("HEADACHE_e", "HEADACHE:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("DRY_MOUTH_e", "DRY MOUTH:", min = 0, max = 0.99, value = 0, step = 0.01)
    ),
    column(4, offset = 1,
           sliderInput("INSOMNIA_e", "INSOMNIA:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("SEXUAL_DYSFUNCTION_e", "SEXUAL DYSFUNCTION:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("DIARRHOEA_e", "DIARRHOEA:", min = 0, max = 0.99, value = 0, step = 0.01)
    ),
    column(4,
           sliderInput("SUICIDAL_IDEATION_e", "SUICIDAL IDEATION:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("AGGRESSION_e", "AGGRESSION:", min = 0, max = 0.99, value = 0, step = 0.01),
           sliderInput("ACCIDENTAL_OVERDOSE_e", "ACCIDENTAL OVERDOSE:", min = 0, max = 0.99, value = 0, step = 0.01)
    )
  )
    
  )
)


server <- shinyServer(function(input, output) {
  
  rate.pla.list <- unique(final$rate.pla)  
  outcome.selection <- c("NAUSEA", "HEADACHE", "DRY MOUTH", "INSOMNIA", "SEXUAL DYSFUNCTION","DIARRHOEA", "SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")  
  treatment.selection <- c("vortioxetine", "venlafaxine", "reboxetine", "mirtazapine", "fluoxetine", "duloxetine", "amitriptyline", "placebo")

  ## calculate adjust z-score
  getData <- reactive({
  
  if(input$NAUSEA_e != 0){
    rate.pla.list[1] <- input$NAUSEA_e
  }  
    
  if(input$HEADACHE_e != 0){
    rate.pla.list[2] <- input$HEADACHE_e
  }
  
  if(input$DRY_MOUTH_e != 0){
    rate.pla.list[3] <- input$DRY_MOUTH_e
  }
  
  if(input$INSOMNIA_e != 0){
    rate.pla.list[4] <- input$INSOMNIA_e
  }

  if(input$SEXUAL_DYSFUNCTION_e != 0){
    rate.pla.list[5] <- input$SEXUAL_DYSFUNCTION_e
  }

  if(input$DIARRHOEA_e != 0){
    rate.pla.list[6] <- input$DIARRHOEA_e
  }

  if(input$SUICIDAL_IDEATION_e != 0){
    rate.pla.list[7] <- input$SUICIDAL_IDEATION_e
  }

  if(input$AGGRESSION_e != 0){
    rate.pla.list[8] <- input$AGGRESSION_e
  }

  if(input$ACCIDENTAL_OVERDOSE_e != 0){
    rate.pla.list[9] <- input$ACCIDENTAL_OVERDOSE_e
  }
    
  rate.pla <- rep(rate.pla.list, table(final$outcome))  
  odds.pla=rate.pla/(1-rate.pla)
  # calculate event rate for treatment using the formula  
  final$event.rate <- round(final$OR* odds.pla/ (1+final$OR*odds.pla) * 100)
  
  clinically.important.RD.list <- c(input$NAUSEA, input$HEADACHE, input$DRY_MOUTH, input$INSOMNIA, input$SEXUAL_DYSFUNCTION, input$DIARRHOEA, input$SUICIDAL_IDEATION, input$AGGRESSION, input$ACCIDENTAL_OVERDOSE)

  Zscore <- vector()
  for(i in 1:length(rate.pla.list)){
    rate.pla <- rate.pla.list[i]
    clinically.important.RD <- clinically.important.RD.list[i]
    risk.drugs <- clinically.important.RD + rate.pla
    OR.import <- risk.drugs/(1-risk.drugs)/((rate.pla)/(1-rate.pla))
    logOR <- final[final$outcome == outcome.selection[i], "logOR"]
    seTE <- final[final$outcome == outcome.selection[i], "seTE"]
    Zscore <- c(Zscore, (logOR-log(OR.import))/seTE)
  }

  final_data <- final
  final_data$Zscore <- Zscore
  final_data <- final_data[,c("outcome", "drug", "Zscore", "event.rate")]

  #add in placebo
  bb <- data.frame(outcome = outcome.selection, drug = rep("placebo", length(outcome.selection)), Zscore = rep(NA, length(outcome.selection)), event.rate = round(rate.pla.list*100) )
  final_data <- rbind(final_data, bb)

  final_data$drug <- factor(final_data$drug,
                            level = treatment.selection,ordered = TRUE)

  final_data$Zscore2 <- final_data$Zscore #truncated zscore
  final_data$Zscore2[final_data$Zscore2 < -2.5] = -2.5
  final_data$Zscore2[final_data$Zscore2 > 2.5] = 2.5

  # add % sign
  aa <- function(x){ if(!is.na(x)){paste0(x, "%")} else{x}}
  final_data$event.rate <- sapply(final_data$event.rate, aa)
  
  # fill in missing combination
  dat2 <- with(final_data, expand.grid(outcome = levels(outcome), drug = levels(drug)))
  final_data2 <- merge(final_data, dat2, all.y = TRUE)
  
  final_data2[final_data2$drug != "placebo" & is.na(final_data2$Zscore), "Zscore2"] <- 0
  final_data2[final_data2$drug != "placebo" & is.na(final_data2$Zscore), "event.rate"] <- "-"
  
  return(final_data2)
  
  })

  output$plot1 = renderPlot({
    
  final_data2 = getData()  
  ggplot(final_data2, aes(outcome, drug)) + geom_tile(aes(fill = round(Zscore2,1)), colour = "white") + 
    geom_text(aes(label= event.rate), size = 6) +
    scale_fill_gradient2(low = "green", mid = "white", high = "red", na.value = "lightskyblue1", 
                         breaks = c(-2.326348, -1.281552, 0, 1.281552, 2.326348), limits = c(-2.5, 2.5),
                         labels = c("p < 0.01", "p = 0.1", "p = 1.00", "p = 0.1", "p < 0.01")) +
    labs(x = "",y = "") +
    theme(legend.title = element_blank(),
          legend.position = "left",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_x_discrete(position = "top") 
  })
})

shinyApp(ui = ui, server = server)


