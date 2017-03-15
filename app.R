
#load("C:/Users/vgw52064/Desktop/Coursera/10_Capstone/PredictApp/.RData")

library(shiny)
library(tm)
library(stringr)
library(R.utils)
library(RWeka)
library(readr)
library(stringi)
library(stylo)
library(ggplot2)

# Define UI for application 
ui <- shinyUI(fluidPage(
     
          # Application title
          titlePanel("Coursera Capstone by Claudia Morales"),     
          br(),
          mainPanel("This app is designed to provide the user with the next most probable 
                    word that could follow. To see how it works, just type any english word
                    in the text box below."),
          br(),     
          br(),
          br(),
               mainPanel(
                    span(
                         textInput(
                              "text",
                              "Text Input: ",
                              value = ""
                         )
                         
                    )
               ),
               br(),
               mainPanel(
                    strong("The next word predicted is:"),
                    textOutput("predictedWord"),
                    style="color:darkgreen"
               )
          )              
     )


     

# Define server logic required 
server <- shinyServer(function(input, output) {
     
     wordPrediction <- reactive({
          text <- input$text
          textInput <- cleanInput(text)
          wordCount <- length(textInput)
          wordPrediction <- nextWordPrediction(wordCount,textInput)})
     
     output$predictedWord <- renderPrint(wordPrediction())
     
})
     
     
# Run the application 
shinyApp(ui = ui, server = server)




