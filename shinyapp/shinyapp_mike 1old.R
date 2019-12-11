# rsconnect::deployApp('C:/Users/efthimiou/Desktop/post doc/PROJECT/APPLIED PROJECTS/CBASP IPD NMA/cbasp shiny/CBASP')
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##############################################################################


library(shiny)
#install.packages("caret")
#install.packages("kerlab")

### load the three prediction models - one model per treatment
load("treat1.rda")
load("treat2.rda")
load("treat3.rda")

### load the rescaling vectors
xmeans=readRDS("xmeans")
xsd=readRDS("xsd")



##### global
# The list of work condition
work_choices <- list("Full time employment" = 1,
               "Part-time employment" = 2,
               "On-sick leave" = 3, 
               "Housewife" = 4,
               "Student" = 5,
               "Retired" = 6,
               "Not employed" = 7)

marriage_choices <- list("Single (never married)" = 1,
                         "Divorced or separated" = 2,
                         "Widowed" = 3,
                         "Married" = 4)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  shinyjs::inlineCSS(list(body = "color:DarkBlue")),
  # Application title
  titlePanel(h1("Prediction model in a pragmatic megatrial of acute treatment for major depression")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    

    
    sidebarPanel(
      h2("Input patient characteristics"),
      sliderInput("age", "Age in years", min = 25, max = 75, value = 45),
      radioButtons("sex", "Sex", c("Male" = 1, "Female" = 2)),      
      sliderInput("educatenumber", "Years of education", min = 8, max = 28, value = 12),
      selectInput("work", "Employment", choices = work_choices),
      selectInput("marriage", "Marital status", choices = marriage_choices),
      sliderInput("depression_age", "Age of onset of depression", min = 0, max = 75, value = 23),
      sliderInput("depression_episode_number", "Number of depression episodes", min = 1, max = 50, value = 3),
      sliderInput("episode_months", "Length of current depressive episode in months", min = 1, max = 276, value = 2),
      radioButtons("physical_illness", "Physical Illness", c("No" = 0, "Yes" = 1)),
      sliderInput("primemd_q1", "PHQ-9 item 1 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q2", "PHQ-9 item 2 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q3", "PHQ-9 item 3 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q4", "PHQ-9 item 4 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q5", "PHQ-9 item 5 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q6", "PHQ-9 item 6 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q7", "PHQ-9 item 7 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q8", "PHQ-9 item 8 at baseline", min = 0, max = 3, value = 0),
      sliderInput("primemd_q9", "PHQ-9 item 9 at baseline", min = 0, max = 3, value = 0),
      sliderInput("phq9_q1_1", "PHQ-9 item 1 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q2_1", "PHQ-9 item 2 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q3_1", "PHQ-9 item 3 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q4_1", "PHQ-9 item 4 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q5_1", "PHQ-9 item 5 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q6_1", "PHQ-9 item 6 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q7_1", "PHQ-9 item 7 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q8_1", "PHQ-9 item 8 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q9_1", "PHQ-9 item 9 at week 1", min = 0, max = 3, value = 0),
      sliderInput("w1_fibser_q1", "Adherence at week 1", min = 1, max = 7, value = 1),
      sliderInput("w1_fibser_q2", "FIBSER item 1 at week 1", min = 1, max = 7, value = 1),
      sliderInput("w1_fibser_q3", "FIBSER item 2 at week 1", min = 1, max = 7, value = 1),
      sliderInput("w1_fibser_q4", "FIBSER item 3 at week 1", min = 1, max = 7, value = 1),
      sliderInput("bdi_q1_1", "BDI-II item 1 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q2_1", "BDI-II item 2 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q3_1", "BDI-II item 3 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q4_1", "BDI-II item 4 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q5_1", "BDI-II item 5 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q6_1", "BDI-II item 6 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q7_1", "BDI-II item 7 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q8_1", "BDI-II item 8 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q9_1", "BDI-II item 9 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q10_1", "BDI-II item 10 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q11_1", "BDI-II item 11 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q12_1", "BDI-II item 12 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q13_1", "BDI-II item 13 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q14_1", "BDI-II item 14 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q15_1", "BDI-II item 15 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q16_1longer", "Hypersomnia at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q16_1shorter", "Insomnia at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q17_1", "BDI-II item 17 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q18_1longer", "Decreased appetite at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q18_1shorter", "Increased appetite at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q19_1", "BDI-II item 19 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q20_1", "BDI-II item 20 at week 1", min = 0, max = 3, value = 0),
      sliderInput("bdi_q21_1", "BDI-II item 21 at week 1", min = 0, max = 3, value = 0),
      sliderInput("phq9_q1_3", "PHQ-9 item 1 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q2_3", "PHQ-9 item 2 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q3_3", "PHQ-9 item 3 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q4_3", "PHQ-9 item 4 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q5_3", "PHQ-9 item 5 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q6_3", "PHQ-9 item 6 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q7_3", "PHQ-9 item 7 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q8_3", "PHQ-9 item 8 at week 3", min = 0, max = 3, value = 0),
      sliderInput("phq9_q9_3", "PHQ-9 item 9 at week 3", min = 0, max = 3, value = 0),
      sliderInput("w3_fibser_q1", "Adherence at week 3", min = 1, max = 7, value = 1),
      sliderInput("w3_fibser_q2", "FIBSER item 1 at week 3", min = 1, max = 7, value = 1),
      sliderInput("w3_fibser_q3", "FIBSER item 1 at week 3", min = 1, max = 7, value = 1),
      sliderInput("w3_fibser_q4", "FIBSER item 1 at week 3", min = 1, max = 7, value = 1),
      sliderInput("bdi_q1_3", "BDI-II item 1 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q2_3", "BDI-II item 2 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q3_3", "BDI-II item 3 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q4_3", "BDI-II item 4 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q5_3", "BDI-II item 5 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q6_3", "BDI-II item 6 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q7_3", "BDI-II item 7 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q8_3", "BDI-II item 8 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q9_3", "BDI-II item 9 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q10_3", "BDI-II item 10 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q11_3", "BDI-II item 11 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q12_3", "BDI-II item 12 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q13_3", "BDI-II item 13 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q14_3", "BDI-II item 14 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q15_3", "BDI-II item 15 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q16_3longer", "Hypersomnia at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q16_3shorter", "Insomnia at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q17_3", "BDI-II item 17 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q18_3longer", "Decreased appetite at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q18_3shorter", "Increased appetite at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q19_3", "BDI-II item 19 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q20_3", "BDI-II item 20 at week 3", min = 0, max = 3, value = 0),
      sliderInput("bdi_q21_3", "BDI-II item 21 at week 3", min = 0, max = 3, value = 0)
      
    ),
    mainPanel(
      
      tabsetPanel(id = 'gene set',
                  tabPanel("Result", textOutput("text1"), textOutput("text2"), textOutput("text3"), textOutput("text4"),
                           tags$head(tags$style("#text4{color: blue;font-size: 25px;font-style:bold;} "))
                           
                           ),
                  tabPanel("Input data", tableOutput("dataframe"))
      )      
      
      
    )
  )))


server <- shinyServer(function(input, output) {


  
  getData = reactive({
    
    w2=1*(input$work==2)
    w3=1*(input$work==3)
    w4=1*(input$work==4)
    w5=1*(input$work==5)
    w6=1*(input$work==6)
    w7=1*(input$work==7)
    ed2=1*(input$marriage==2)
    ed3=1*(input$marriage==3)
    ed4=1*(input$marriage==4)
    
    sex = as.numeric(input$sex)
    physical_illness = as.numeric(input$physical_illness)
    
    age = input$age
    educatenumber = input$educatenumber
    depression_age = input$depression_age
    depression_episode_number = input$depression_episode_number
    episode_months = input$episode_months
    primemd_q1 = input$primemd_q1
    primemd_q2 = input$primemd_q2
    primemd_q3 = input$primemd_q3
    primemd_q4 = input$primemd_q4
    primemd_q5 = input$primemd_q5
    primemd_q6 = input$primemd_q6
    primemd_q7 = input$primemd_q7
    primemd_q8 = input$primemd_q8
    primemd_q9 = input$primemd_q9
    phq9_q1_1 = input$phq9_q1_1
    phq9_q2_1 = input$phq9_q2_1
    phq9_q3_1 = input$phq9_q3_1
    phq9_q4_1 = input$phq9_q4_1
    phq9_q5_1 = input$phq9_q5_1
    phq9_q6_1 = input$phq9_q6_1
    phq9_q7_1 = input$phq9_q7_1
    phq9_q8_1 = input$phq9_q8_1
    phq9_q9_1 = input$phq9_q9_1
    w1_fibser_q1 = input$w1_fibser_q1
    w1_fibser_q2 = input$w1_fibser_q2
    w1_fibser_q3 = input$w1_fibser_q3
    w1_fibser_q4 = input$w1_fibser_q4
    bdi_q1_1 = input$bdi_q1_1
    bdi_q2_1 = input$bdi_q2_1
    bdi_q3_1 = input$bdi_q3_1
    bdi_q4_1 = input$bdi_q4_1
    bdi_q5_1 = input$bdi_q5_1
    bdi_q6_1 = input$bdi_q6_1
    bdi_q7_1 = input$bdi_q7_1
    bdi_q8_1 = input$bdi_q8_1
    bdi_q9_1 = input$bdi_q9_1
    bdi_q10_1 = input$bdi_q10_1
    bdi_q11_1 = input$bdi_q11_1
    bdi_q12_1 = input$bdi_q12_1
    bdi_q13_1 = input$bdi_q13_1
    bdi_q14_1 = input$bdi_q14_1
    bdi_q15_1 = input$bdi_q15_1
    bdi_q16_1longer = input$bdi_q16_1longer
    bdi_q16_1shorter = input$bdi_q16_1shorter
    bdi_q17_1 = input$bdi_q17_1
    bdi_q18_1longer = input$bdi_q18_1longer
    bdi_q18_1shorter = input$bdi_q18_1shorter
    bdi_q19_1 = input$bdi_q19_1
    bdi_q20_1 = input$bdi_q20_1
    bdi_q21_1 = input$bdi_q21_1
    phq9_q1_3 = input$phq9_q1_3
    phq9_q2_3 = input$phq9_q2_3
    phq9_q3_3 = input$phq9_q3_3
    phq9_q4_3 = input$phq9_q4_3
    phq9_q5_3 = input$phq9_q5_3
    phq9_q6_3 = input$phq9_q6_3
    phq9_q7_3 = input$phq9_q7_3
    phq9_q8_3 = input$phq9_q8_3
    phq9_q9_3 = input$phq9_q9_3
    w3_fibser_q1 = input$w3_fibser_q1
    w3_fibser_q2 = input$w3_fibser_q2
    w3_fibser_q3 = input$w3_fibser_q3
    w3_fibser_q4 = input$w3_fibser_q4
    bdi_q1_3 = input$bdi_q1_3
    bdi_q2_3 = input$bdi_q2_3
    bdi_q3_3 = input$bdi_q3_3
    bdi_q4_3 = input$bdi_q4_3
    bdi_q5_3 = input$bdi_q5_3
    bdi_q6_3 = input$bdi_q6_3
    bdi_q7_3 = input$bdi_q7_3
    bdi_q8_3 = input$bdi_q8_3
    bdi_q9_3 = input$bdi_q9_3
    bdi_q10_3 = input$bdi_q10_3
    bdi_q11_3 = input$bdi_q11_3
    bdi_q12_3 = input$bdi_q12_3
    bdi_q13_3 = input$bdi_q13_3
    bdi_q14_3 = input$bdi_q14_3
    bdi_q15_3 = input$bdi_q15_3
    bdi_q16_3longer = input$bdi_q16_3longer
    bdi_q16_3shorter = input$bdi_q16_3shorter
    bdi_q17_3 = input$bdi_q17_3
    bdi_q18_3longer = input$bdi_q18_3longer
    bdi_q18_3shorter = input$bdi_q18_3shorter
    bdi_q19_3 = input$bdi_q19_3
    bdi_q20_3 = input$bdi_q20_3
    bdi_q21_3 = input$bdi_q21_3
    
    ### create data frame
    dat1=data.frame(
      age, sex, educatenumber, w2, w3, 
      w4, w5, w6, w7, ed2, ed3, ed4, depression_age, 
      depression_episode_number, episode_months, physical_illness, 
      primemd_q1, primemd_q2, primemd_q3, primemd_q4, primemd_q5, 
      primemd_q6, primemd_q7, primemd_q8, primemd_q9, phq9_q1_1, 
      phq9_q2_1, phq9_q3_1, phq9_q4_1, phq9_q5_1, phq9_q6_1, 
      phq9_q7_1, phq9_q8_1, phq9_q9_1, w1_fibser_q1, w1_fibser_q2, 
      w1_fibser_q3, w1_fibser_q4, bdi_q1_1, bdi_q2_1, bdi_q3_1, 
      bdi_q4_1, bdi_q5_1, bdi_q6_1, bdi_q7_1, bdi_q8_1, bdi_q9_1, 
      bdi_q10_1, bdi_q11_1, bdi_q12_1, bdi_q13_1, bdi_q14_1, 
      bdi_q15_1, bdi_q17_1, bdi_q19_1, bdi_q20_1, bdi_q21_1, 
      phq9_q1_3, phq9_q2_3, phq9_q3_3, phq9_q4_3, phq9_q5_3, 
      phq9_q6_3, phq9_q7_3, phq9_q8_3, phq9_q9_3, w3_fibser_q1, 
      w3_fibser_q2, w3_fibser_q3, w3_fibser_q4, bdi_q1_3, bdi_q2_3, 
      bdi_q3_3, bdi_q4_3, bdi_q5_3, bdi_q6_3, bdi_q7_3, bdi_q8_3, 
      bdi_q9_3, bdi_q10_3, bdi_q11_3, bdi_q12_3, bdi_q13_3, 
      bdi_q14_3, bdi_q15_3, bdi_q17_3, bdi_q19_3, bdi_q20_3, 
      bdi_q21_3, bdi_q16_1longer, bdi_q16_1shorter, bdi_q18_1longer, 
      bdi_q18_1shorter, bdi_q16_3longer, bdi_q16_3shorter, bdi_q18_3longer, 
      bdi_q18_3shorter)
    

    dat2 = scale(dat1, center = xmeans, scale = xsd)
    
    ### predict the outcome for the three different treamtents
    y1<- exp(predict(svm_Radial.t1, newdata = dat2))-1   #### continue sertraline
    y2<- exp(predict(svm_Radial.t2, newdata = dat2))-1   #### combine with mirtazapine
    y3<- exp(predict(svm_Radial.t3, newdata = dat2))-1   #### switch to mirtazapine
    ind=1*(y1<y2&y1<y3)+2*(y2<y1&y2<y3)+3*(y3<y2&y3<y1)
    best.strategy="combine sertraline and mirtazapine (second best: continue sertraline)"
    if(ind==2){best.strategy="combine sertraline and mirtazapine (or switch to mirtazapine)"}
    if(ind==3){best.strategy="switch to mirtazapine (or combine sertraline and mirtazapine)"}

    text1 = paste("The predicted PHQ9 score after 6 weeks, if continuing on sertraline, is ", round(y1,digits=1),".",sep="")
    text2 = paste("The predicted PHQ9 score after 6 weeks, if combining sertraline and mirtazapine, is ", round(y2,digits=1),".",sep="")
    text3 = paste("The predicted PHQ9 score after 6 weeks, if switching to mirtazapine, is ", round(y3,digits=1),".",sep="")
    text4 = paste("The best predicted treatment strategy is to ", best.strategy,".",sep="")
    
    data = list(dat1 = dat1, best.strategy = best.strategy, text1 = text1, text2 = text2, text3 = text3, text4 = text4)
    data
    
  })

  output$dataframe = renderTable({
    data = getData()
    apply(data$dat1, 2, as.numeric)
  }, rownames = TRUE)
  
  
  output$text1 = renderText({
    data = getData()
    data$text1
  })
  
  output$text2 = renderText({
    data = getData()
    data$text2
  })
  
  output$text3 = renderText({
    data = getData()
    data$text3
  })
  
  output$text4 = renderText({
    data = getData()
    paste(data$text4)
    })
  

  
})

shinyApp(ui = ui, server = server)
