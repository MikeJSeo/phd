library(coda)
library(shiny)

result <- list()

load("prediction-ADAS-result.RData")
ADAS_prediction_matrix <- as.matrix(samples[,c("alpha", paste0("beta[", 1:7, "]"))])
ADAS_prediction_sigma <- as.matrix(samples[,c("sigma")])

load("IPDMA-ADAS-result.RData")
ADAS_IPDMA_matrix <- as.matrix(samples[,c("d",paste0("gamma[", 1:7,"]"))])

load("IPDMA-CIBIC-result.RData")
CIBIC_IPDMA_matrix <- as.matrix(samples[,c("d",paste0("gamma[", 1:7,"]"))])


ui <- shinyUI(fluidPage(
  # Application title
  tags$head(
    tags$style(HTML("

      /* Change font of header text */
      h2 {
        font-size: 400%;
        font-weight: bold;
        color: black;
      }"))
  ),
  
  titlePanel("Cognitive and global outcomes on donepezil vs placebo for Alzheimer's disease"),            
             
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    sliderInput("AGE", label = "Age", min = 60, max = 90, value = 75, step = 1),
    selectInput("SEX",label="Gender",
                choices=list("Male"=0, "Female"=1),selected=1),
    sliderInput("WEIGHT", label = "Weight (kg)", min = 35, max = 115, value = 62, step = 1),
    selectInput("AP",label="Antipsychotic use at baseline",
                choices=list("No"=0, "Yes"=1),selected=0),
    selectInput("AMNOTAP",label="Use of any medication other than antipsychotics at baseline",
                choices=list("No"=0, "Yes"=1),selected=1),
    sliderInput("ADAS_TRANSFORMED_BASE", "Baseline ADAS-cog total score", min = 6, max = 70, value = 34, step = 0.1),
    sliderInput("CDR_TRANSFORMED_BASE", "Baseline CDR-SB total score", min = 2, max = 18, value = 9, step = 0.1)
  ),
  
  
  mainPanel(
    span(textOutput("text0", inline = TRUE), style="color:black; font-size: 300%; font-weight: bold"),
    HTML('<br/>'),
    span(textOutput("text1", inline = TRUE), style="color:black; font-size: 150%"),
    span(textOutput("text2", inline = TRUE), style="color:blue; font-size: 150%; font-weight: bold"),
    HTML('<br/>'),
    span(textOutput("text3", inline = TRUE), style="color:black; font-size: 150%"),
    span(textOutput("text4", inline = TRUE), style="color:blue; font-size: 150%; font-weight: bold"),
    HTML('<br/>'),
    HTML('<br/>'),
    span(textOutput("text5", inline = TRUE), style="color:black; font-size: 300%; font-weight: bold"),
    HTML('<br/>'),
    span(textOutput("text6", inline = TRUE), style="color:black; font-size: 150%"),
    span(textOutput("text7", inline = TRUE), style="color:blue; font-size: 150%; font-weight: bold")
  )      
  ))
  
server <- shinyServer(function(input, output) {
 
  mean_val <- c(0, 74.55107868, 0.65683476, 61.75197076, 0.06343165, 0.64890580, 34.44052410, 8.57902253)
  sd_val <- c(1, 8.4182890, 0.4748414, 14.1669708, 0.2437764, 0.4773880, 15.5341925, 3.3758396)

  
  getOutput <- reactive({
    
    patient.input <- as.vector(c(1,input$AGE, input$SEX, input$WEIGHT, input$AP, input$AMNOTAP, input$ADAS_TRANSFORMED_BASE, input$CDR_TRANSFORMED_BASE), mode = "integer")
    patient.input.standardized <- (patient.input - mean_val) / sd_val
    
    ADAS_prediction_mean <- ADAS_prediction_matrix %*% patient.input.standardized
    ADAS_ypred <- rnorm(length(ADAS_prediction_mean), ADAS_prediction_mean, ADAS_prediction_sigma)  
    ADAS_ypred_mean <- mean(ADAS_ypred)
    ADAS_ypred_CrI <- quantile(ADAS_ypred, probs = c(0.025, 0.975))
    
    ADAS_IPDMA_result <- ADAS_IPDMA_matrix %*% patient.input.standardized
    ADAS_IPDMA_mean <- mean(ADAS_IPDMA_result)
    ADAS_IPDMA_CrI <- quantile(ADAS_IPDMA_result, prob = c(0.025, 0.975))
    
    CIBIC_IPDMA_result <- CIBIC_IPDMA_matrix %*% patient.input.standardized
    CIBIC_IPDMA_mean <- mean(CIBIC_IPDMA_result)
    CIBIC_IPDMA_CrI <- quantile(CIBIC_IPDMA_result, prob = c(0.025, 0.975))
    
    data <- list(ADAS_ypred_mean = ADAS_ypred_mean, ADAS_ypred_CrI = ADAS_ypred_CrI, ADAS_IPDMA_mean = ADAS_IPDMA_mean, ADAS_IPDMA_CrI = ADAS_IPDMA_CrI, CIBIC_IPDMA_mean = CIBIC_IPDMA_mean, CIBIC_IPDMA_CrI = CIBIC_IPDMA_CrI)
  })
   
  output$text0<- renderText({
    "ADAS-cog total score after 24 weeks [cognitive outcome]"
  })
  
  output$text1 <- renderText({
    "Predicted outcome for patients taking placebo "
  })
    
  output$text2 <- renderText({
    data = getOutput()
    paste0(round(data$ADAS_ypred_mean, digits = 1), ", 95% CrI [", round(data$ADAS_ypred_CrI[1], digits = 1), "; ", round(data$ADAS_ypred_CrI[2], digits = 1),"]")
  })
  
  output$text3 = renderText({
    "Patient-specific treatment effect of taking donepezil over placebo "
  })
  
  output$text4 <- renderText({
    data = getOutput()
    paste0(round(data$ADAS_IPDMA_mean, digits = 1), ", 95% CrI [", round(data$ADAS_IPDMA_CrI[1], digits = 1), "; ", round(data$ADAS_IPDMA_CrI[2], digits = 1),"]")
  })
  
  output$text5 <- renderText({
    "CIBIC-Plus score after 24 weeks [global outcome]"
  })
    
  output$text6 <- renderText({
    "Patient-specific treatment effect of taking donepezil over placebo "
  })
  
  output$text7 <- renderText({
    data = getOutput()
    paste0(round(data$CIBIC_IPDMA_mean, digits = 1), ", 95% CrI [", round(data$CIBIC_IPDMA_CrI[1], digits = 1), "; ", round(data$CIBIC_IPDMA_CrI[2], digits = 1),"]")
  })
})


# Create Shiny object
shinyApp(ui = ui, server = server)
  