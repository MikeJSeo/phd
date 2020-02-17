library("shiny")
library("ggplot2")
#install.packages("ggplot2")
#install.packages("netmeta")

load("final.rda")

ui <- shinyUI(fluidPage(
  shinyjs::inlineCSS(list(body = "color:DarkBlue")),
  # Application title
  titlePanel(h1("The Kilim plot: a tool for visualizing network meta-analysis results for multiple outcomes")),
  
  tabPanel("Heatplot", plotOutput("plot1")),
  
  hr(),
  h3("Clinically important threshold (in risk difference)"),
  
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
    )
  )
  
  
)


server <- shinyServer(function(input, output) {
  
  rate.pla.list <- unique(final$rate.pla)  
  outcome.selection <- c("NAUSEA", "HEADACHE", "DRY MOUTH", "INSOMNIA", "SEXUAL DYSFUNCTION","DIARRHOEA", "SUICIDAL IDEATION", "AGGRESSION", "ACCIDENTAL OVERDOSE")  
  treatment.selection <- c("vortioxetine", "venlafaxine", "reboxetine", "mirtazapine", "fluoxetine", "duloxetine", "amitriptyline", "placebo")

  # function to add % sign..
  aa <- function(x){
    if(!is.na(x)){
      paste0(x, "%")
    } else{
      x
    }
  }
  
  ## calculate adjust z-score
  getData <- reactive({
  
  clinically.important.RD.list <- c(input$NAUSEA, input$HEADACHE, input$DRY_MOUTH, input$INSOMNIA, input$SEXUAL_DYSFUNCTION, input$DIARRHOEA, input$SUICIDAL_IDEATION, input$AGGRESSION, input$ACCIDENTAL_OVERDOSE)

  Zscore <- vector()
  for(i in 1:length(rate.pla.list)){
    rate.pla <- rate.pla.list[i]
    clinically.important.RD <- clinically.important.RD.list[i]
    risk.drugs <- clinically.important.RD+ rate.pla
    OR.import <- risk.drugs/(1-risk.drugs)/((rate.pla)/(1-rate.pla))
    logOR <- final[final$outcome == outcome.selection[i], "logOR"]
    seTE <- final[final$outcome == outcome.selection[i], "seTE"]
    Zscore <- c(Zscore, (logOR-log(OR.import))/seTE)
  }

  final_data <- final
  final_data$Zscore <- Zscore
  final_data <- final_data[,c("outcome", "drug", "Zscore", "event.rate")]

  #add in placebo
  bb <- data.frame(outcome = outcome.selection, drug = rep("placebo", length(outcome.selection)), Zscore = rep(NA, length(outcome.selection)), event.rate = round(rate.pla.list*100,1) )
  final_data <- rbind(final_data, bb)

  final_data$drug <- factor(final_data$drug,
                            level = treatment.selection,ordered = TRUE)

  final_data$Zscore2 <- final_data$Zscore #truncated zscore
  final_data$Zscore2[final_data$Zscore2 < -2.5] = -2.5
  final_data$Zscore2[final_data$Zscore2 > 2.5] = 2.5

  final_data$event.rate <- sapply(final_data$event.rate, aa)
  return(final_data)
  
  })

  output$data1 = renderTable({
    final_data = getData()
    final_data
    
  })
  

  output$plot1 = renderPlot({
    
  final_data = getData()  
  ggplot(final_data, aes(outcome, drug)) + geom_tile(aes(fill = round(Zscore2,2)), colour = "white") + 
    geom_text(aes(label= event.rate)) +
    scale_fill_gradient2(low = "green", mid = "white", high = "red", na.value = "lightskyblue1", 
                         breaks = c(-2.326348, -1.281552, 0, 1.281552, 2.326348), limits = c(-2.5, 2.5),
                         labels = c("p < 0.01", "p = 0.1", "p = 1.00", "p = 0.1", "p < 0.01")) +
    labs(x = "",y = "") +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle=30,hjust=1,vjust=1.0, size = 14),
          axis.text.y = element_text(size = 14),
          legend.position = "left",
          legend.text = element_text(size = 14))
  })
})

shinyApp(ui = ui, server = server)


