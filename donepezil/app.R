library(rjags)
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
  titlePanel(h1("IPD-MA and prediction analysis for Donepezil study")),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    sliderInput("AGE", label = "Age", min = 0, max = 100, value = 75, step = 1),
    selectInput("SEX",label="Gender",
                choices=list("Male"=0, "Female"=1),selected=1),
    sliderInput("WEIGHT", label = "Weight (kg)", min = 0, max = 200, value = 62, step = 1),
    selectInput("AP",label="Antipsychotic use at baseline",
                choices=list("No"=0, "Yes"=1),selected=0),
    selectInput("AMNOTAP",label="Use of any medication other than antipsychotics at baseline",
                choices=list("No"=0, "Yes"=1),selected=1),
    sliderInput("ADAS_TRANSFORMED_BASE", "Baseline transformed ADAS-cog total score", min = 0, max = 100, value = 34, step = 1),
    sliderInput("CDR_TRANSFORMED_BASE", "Baseline transformed CDR sum of boxes total score", min = 0, max = 20, value = 9, step = 1)
  ),
  
  
  mainPanel(
    span(textOutput("text0"), style="color:black; font-size: 400%; font-weight: bold"),
    span(textOutput("text1"), style="color:black; font-size: 200%"),
    span(textOutput("text2"), style="color:black; font-size: 200%"),
    span(textOutput("text3"), style="color:black; font-size: 400%; font-weight: bold"),
    span(textOutput("text4"), style="color:black; font-size: 200%")
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
    "Primary outcome: ADAS TRANSFORMED"
  })
  
  output$text1 <- renderText({
    data = getOutput()
    paste0("Predicted outcome for patients taking placebo: ", round(data$ADAS_ypred_mean, digits = 2), ", 95% CrI [", round(data$ADAS_ypred_CrI[1], digits =2), "; ", round(data$ADAS_ypred_CrI[2], digits = 2),"]")
  })
  
  
  output$text2 = renderText({
    data = getOutput()
    paste0("Patient-specific treatment effect of taking Donepezil: ", round(data$ADAS_IPDMA_mean, digits = 2), ", 95% CrI [", round(data$ADAS_IPDMA_CrI[1], digits =2), "; ", round(data$ADAS_IPDMA_CrI[2], digits = 2),"]")
  })

  output$text3 <- renderText({
    "Secondary outcome: CIBIC PLUS TRANSFORMED"
  })
    
  output$text4 = renderText({
    data = getOutput()
    paste0("Patient-specific treatment effect of taking Donepezil: ", round(data$CIBIC_IPDMA_mean, digits = 2), ", 95% CrI [", round(data$CIBIC_IPDMA_CrI[1], digits =2), "; ", round(data$CIBIC_IPDMA_CrI[2], digits = 2),"]")
  })
})

# Create Shiny object
shinyApp(ui = ui, server = server)
  