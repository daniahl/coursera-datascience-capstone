#Predictify app

library(shiny)
library(ngram)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predictify - n-gram Text Prediction"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("This is the Predictify app, a proof-of-concept application developed for the Data Science Capstone course on Coursera."),
            p("The app uses three simple n-gram models (n=2,3,4) to predict the next word given at least two words in the input text."),
            h2("How to use"),
            p("Just type a minimum of two words in the input box and click predict. The predicted word (if any) will be added to the input so you can keep clicking for the next word! If nothing happens when you click predict, no prediction was possible with the current input. Try 'This is' for a working example.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textAreaInput("textarea", NULL),
           actionButton("predict", "Predict")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$predict, {
        inp <- input$textarea
        pred <- predict(m2,m3,m4, inp)
        
        updateTextAreaInput(inputId="textarea", value=paste(inp, pred))
    })
}

# Read data and source helper functions
load("models.RData")
source("functions.R")

# Run the application 
shinyApp(ui = ui, server = server)
